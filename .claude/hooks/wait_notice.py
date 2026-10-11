#!/usr/bin/env python3
"""PreToolUse / PostToolUse hook for mcp__isabelle__*: tell the human watching when a call is expected to take a while.

The editor shows only "Isabelle [get_state]" while a call waits (up to minutes), which looks like a stuck agent.
- PreToolUse: from the call's arguments, "Isabelle: get_state waits up to N s for <file> to settle".
- PostToolUse: "Isabelle: get_state finished after N s: <first line of the result>" (or BLOCKED), for calls that took >= 5 s.
Both are systemMessages (shown to the user, not to the model). Nothing is printed for quick calls.
"""
import glob, json, os, sys, tempfile, time

MIN_SECONDS = 5
try:
    data = json.load(sys.stdin)
except Exception:
    sys.exit(0)  # never fail: this hook is registered for every session

def first_run():
    """The same hook may be registered twice (project and global settings): only the first invocation per call and event speaks."""
    try:
        base = os.path.join(tempfile.gettempdir(), "pide_wait_notice_")
        for old in glob.glob(base + "*"):  # forget markers older than an hour
            if time.time() - os.path.getmtime(old) > 3600:
                os.remove(old)
        fd = os.open(base + f"{data.get('hook_event_name')}_{data.get('tool_use_id', 'x')}", os.O_CREAT | os.O_EXCL | os.O_WRONLY)
        os.close(fd)
        return True
    except FileExistsError:
        return False
    except OSError:
        return True

def say(message):
    """Show the message to the user (with the clock time: the editor shows none) and keep it in
    <project>/.pide_mcp/status.log (a file to watch in the editor)."""
    if not first_run():
        return
    message = time.strftime("%H:%M:%S ") + message
    print(json.dumps({"systemMessage": message}))
    try:
        d = os.path.join(os.environ.get("CLAUDE_PROJECT_DIR") or os.getcwd(), ".pide_mcp")
        os.makedirs(d, exist_ok=True)
        with open(os.path.join(d, "status.log"), "a") as f:
            f.write(message + "\n")
    except OSError:
        pass
event = data.get("hook_event_name")
tool = (data.get("tool_name") or "").split("__")[-1]
args = data.get("tool_input") or {}

def name_of(origin):
    return os.path.basename(origin) if isinstance(origin, str) else "the file"

def expected(tool, args):
    """(max seconds, what) a call may wait, or None if it is quick."""
    if tool == "get_state":
        wait = args.get("wait_seconds") or 0
        wait = int(wait) if str(wait).isdigit() else 0
        if wait >= MIN_SECONDS:
            what = "to settle" if str(args.get("wait_settled")).lower() == "true" else "to load/process"
            return wait, f"{name_of(args.get('origin'))} {what}"
    elif tool == "edit" and str(args.get("check")).lower() == "true":
        wait = args.get("check_wait_seconds", 30)
        wait = int(wait) if str(wait).isdigit() else 30
        if wait >= MIN_SECONDS:
            return wait, f"{name_of(args.get('origin'))} to settle after the edit"
    elif tool == "start_session":
        return None  # can take minutes (build), unknown: the message below says so
    return None

try:
    if event == "PreToolUse":
        exp = expected(tool, args)
        if exp:
            say(f"Isabelle: {tool} waits up to {exp[0]} s for {exp[1]} (a long wait is normal, not stuck)")
        elif tool == "start_session":
            say("Isabelle: start_session builds/loads the session, this can take minutes")
    elif event == "PostToolUse":
        secs = (data.get("duration_ms") or 0) / 1000
        if secs >= MIN_SECONDS and tool in ("get_state", "edit", "start_session"):
            resp = data.get("tool_response")
            text = resp if isinstance(resp, str) else " ".join(b.get("text", "") for b in resp if isinstance(b, dict)) if isinstance(resp, list) else ""
            lines = [l.strip() for l in text.splitlines() if l.strip()]
            blocked = next((l for l in lines if "BLOCKED" in l), None)
            first = blocked or (lines[0] if lines else "")
            say(f"Isabelle: {tool} finished after {secs:.0f} s: {first[:160]}")
except Exception:
    pass
