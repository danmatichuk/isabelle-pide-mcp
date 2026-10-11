#!/usr/bin/env python3
"""PostToolUse hook for mcp__isabelle__*: show the user the sync diffs.

The MCP tools write a diff file when they pick up external edits and name it in
their output ("full diff: <path>", then an "ACTION REQUIRED" line for agents
without this hook; with format json: "diff_file"). Tool output is not visible to the user, so this hook
 - prepends the diff to <project>/.pide_mcp/sync_log.diff (newest first, capped;
   the directory ignores itself in git),
 - opens that log in VS Code the first time in a Claude session (PIDE_HOOK_OPEN=0: never),
 - shows the log path to the user via systemMessage (it cannot be a clickable link),
 - removes both lines from the output the agent sees, so the agent cannot ignore
   them and does not spend context on them.
"""
import json, os, re, shutil, subprocess, sys, tempfile, time

LOG_MAX_BYTES = 1_000_000

def prepend_to_log(path):
    project = os.environ.get("CLAUDE_PROJECT_DIR") or os.getcwd()
    log_dir = os.path.join(project, ".pide_mcp")
    os.makedirs(log_dir, exist_ok=True)
    ignore = os.path.join(log_dir, ".gitignore")
    if not os.path.exists(ignore):
        with open(ignore, "w") as f:
            f.write("*\n")
    log = os.path.join(log_dir, "sync_log.diff")
    with open(path, errors="replace") as f:
        diff = f.read()
    try:
        with open(log, errors="replace") as f:
            old = f.read()
    except OSError:
        old = ""
    entry = f"=== {time.strftime('%Y-%m-%d %H:%M:%S')}  {os.path.basename(path)} ===\n{diff.rstrip()}\n\n"
    with open(log, "w") as f:
        f.write((entry + old)[:LOG_MAX_BYTES])
    return log

def display(path):
    rel = os.path.relpath(path, os.environ.get("CLAUDE_PROJECT_DIR") or os.getcwd())
    return path if rel.startswith("..") else rel

def open_once(log, session_id):
    code = shutil.which("code")
    marker = os.path.join(tempfile.gettempdir(), f"pide_sync_log_opened_{session_id}")
    if code and not os.path.exists(marker) and os.environ.get("PIDE_HOOK_OPEN") != "0":
        open(marker, "w").close()
        subprocess.Popen([code, "--reuse-window", log], stdout=subprocess.DEVNULL,
                         stderr=subprocess.DEVNULL, start_new_session=True)

try:
    data = json.load(sys.stdin)
    resp = data.get("tool_response")
except Exception:
    sys.exit(0)  # never fail: this hook is registered for every session
path_re = re.compile(r'^full diff: (/\S*sync_\S*\.diff)\s*$', re.M)
action_re = re.compile(r'^ACTION REQUIRED: .*\n?', re.M)

def strip_json_diff_file(node):
    """Replace every "diff_file" value in parsed JSON; return the first path found."""
    found = None
    if isinstance(node, dict):
        for k, v in node.items():
            if k == "diff_file" and isinstance(v, str):
                found = found or v
                node[k] = "shown to the user"
            else:
                found = found or strip_json_diff_file(v)
    elif isinstance(node, list):
        for v in node:
            found = found or strip_json_diff_file(v)
    return found

def strip_block_text(text):
    """Return (path, new_text) for the text of one result block, path None if no diff."""
    m = path_re.search(text)
    if m:
        return m.group(1), action_re.sub("", path_re.sub("external edits: full diff shown to the user", text))
    if text.lstrip().startswith("{") and "diff_file" in text:
        try:
            parsed = json.loads(text)
        except ValueError:
            return None, text
        path = strip_json_diff_file(parsed)
        if path:
            return path, json.dumps(parsed, indent=2)
    return None, text

try:
    if isinstance(resp, (list, str)):
        path = None
        if isinstance(resp, str):  # format json: the structured result as one string
            path, blocks = strip_block_text(resp)
        else:
            blocks = []
            for b in resp:
                if isinstance(b, dict) and b.get("type") == "text":
                    p, text = strip_block_text(b.get("text", ""))
                    if p:
                        path = path or p
                        b = {**b, "text": text}
                blocks.append(b)
        if path:
            shown = path  # fall back to the single diff if the log cannot be written
            try:
                shown = prepend_to_log(path)
                open_once(shown, data.get("session_id", "none"))
            except OSError:
                pass  # the log is a convenience only
            print(json.dumps({
                "systemMessage": f"edits synced \u2192 {display(shown)}",
                "hookSpecificOutput": {"hookEventName": "PostToolUse", "updatedToolOutput": blocks},
            }))
except Exception:
    pass
