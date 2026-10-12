#!/usr/bin/env python3
"""Stdio MCP proxy that restarts the wrapped server when its sources changed.

Development helper: lets an agent rebuild the PIDE MCP server (edit src/*.scala, ml/*.thy, etc/*) and
every Claude Code session that uses it picks up the new version on its next request, without a manual
/mcp reconnect. All sessions of the old server are lost on a restart (same as a reconnect).

Usage in an MCP server entry:
  command: python3
  args: [/path/to/dev/mcp_reload_proxy.py, --root, /path/to/isabelle-pide-mcp, --, isabelle, pide_mcp, ...]

How it works: the proxy forwards the newline-delimited JSON-RPC of the stdio transport. Before it forwards
a client request it compares the newest mtime under --root (src, ml, etc) with the start time of the child.
If something is newer and no request is in flight, it stops the child, rebuilds (--build-cmd), starts a
new child, replays the client's initialize handshake, and sends notifications/tools/list_changed so that
the client refreshes its tool list.

Some clients ignore that notification. The proxy therefore remembers the tool list the client was given (it
sees the tools/list responses), compares it with the new server's list after a restart, and appends ONE line
to the first tools/call result after the restart, naming the new or changed tools and saying to reload the
tool list. Any failure in this note is ignored: a result is never dropped or changed otherwise.
"""
import argparse, json, os, signal, subprocess, sys, threading, time

def log(msg):
    print(f"[mcp-reload-proxy] {msg}", file=sys.stderr, flush=True)

def descendants(pid):
    """All descendants of pid (java, poly, ...); poly runs in its own session, so a signal to the group misses it."""
    out = subprocess.run(["ps", "-eo", "pid=,ppid="], capture_output=True, text=True).stdout
    children = {}
    for line in out.splitlines():
        parts = line.split()
        if len(parts) == 2:
            children.setdefault(int(parts[1]), []).append(int(parts[0]))
    result, todo = [], [pid]
    while todo:
        for c in children.get(todo.pop(), []):
            result.append(c)
            todo.append(c)
    return result

def is_alive(pid):
    try:
        with open(f"/proc/{pid}/stat") as f:
            return f.read().rsplit(")", 1)[1].split()[0] != "Z"
    except OSError:
        return False

def wait_gone(pids, seconds):
    deadline = time.time() + seconds
    while time.time() < deadline and any(is_alive(p) for p in pids):
        time.sleep(0.2)
    return not any(is_alive(p) for p in pids)

def newest_mtime(root):
    newest = 0.0
    for sub in ("src", "ml", "etc"):
        for dirpath, _, files in os.walk(os.path.join(root, sub)):
            for f in files:
                if f.endswith((".scala", ".thy", ".ML", ".props")) or f in ("options", "settings"):
                    try:
                        newest = max(newest, os.path.getmtime(os.path.join(dirpath, f)))
                    except OSError:
                        pass
    return newest

class Proxy:
    def __init__(self, root, cmd, build_cmd, init_timeout, java_options=""):
        self.root, self.cmd, self.build_cmd, self.init_timeout = root, cmd, build_cmd, init_timeout
        self.java_options = java_options
        self.max_request_age = 3900.0
        self.out_lock = threading.Lock()
        self.state_lock = threading.Lock()
        self.pending = {}  # id of a request in flight -> time it was forwarded
        self.init_request = None
        self.initialized = None
        self.child = None
        self.started = 0.0
        self.init_done = threading.Event()
        self.list_ids, self.call_ids = set(), set()  # ids of tools/list and tools/call requests of the client
        self.client_tools = None  # signatures of the tool list the client was last given: name -> text
        self.tools_done = threading.Event()
        self.new_tools = None
        self.note = None  # one line for the first tools/call result after a restart

    # --- output to the client
    def to_client(self, line):
        with self.out_lock:
            sys.stdout.write(line if line.endswith("\n") else line + "\n")
            sys.stdout.flush()

    def to_child(self, obj_or_line):
        line = obj_or_line if isinstance(obj_or_line, str) else json.dumps(obj_or_line)
        self.child.stdin.write(line if line.endswith("\n") else line + "\n")
        self.child.stdin.flush()

    # --- child management
    def reader(self, child):
        for line in child.stdout:
            try:
                msg = json.loads(line)
            except ValueError:
                msg = None
            if isinstance(msg, dict) and "method" not in msg and "id" in msg:
                if msg["id"] == "proxy-init":
                    self.init_done.set()
                    continue
                if msg["id"] == "proxy-tools":
                    self.new_tools = self.signatures(msg)
                    self.tools_done.set()
                    continue
                with self.state_lock:
                    self.pending.pop(msg["id"], None)
                if child is self.child:
                    line = self.annotate(msg, line)
            if child is self.child:
                self.to_client(line)

    # --- note about a changed tool list (never raises)
    @staticmethod
    def signatures(msg):
        try:
            return {t["name"]: json.dumps(t, sort_keys=True) for t in msg["result"]["tools"]}
        except Exception:
            return None

    def annotate(self, msg, line):
        """Track the tool list the client was given; add the note to the first tools/call result after a restart."""
        try:
            with self.state_lock:
                if msg["id"] in self.list_ids:
                    self.list_ids.discard(msg["id"])
                    sigs = self.signatures(msg)
                    if sigs is not None:
                        self.client_tools = sigs
                        self.note = None  # the client reloaded its tool list
                    return line
                if msg["id"] in self.call_ids:
                    self.call_ids.discard(msg["id"])
                    note, self.note = self.note, None
                    content = msg["result"]["content"] if note else None
                    if isinstance(content, list):
                        content.append({"type": "text", "text": note})
                        return json.dumps(msg) + "\n"
        except Exception as e:
            log(f"tool list note failed: {e!r}")
        return line

    def make_note(self, old, new):
        added = sorted(n for n in new if n not in old)
        changed = sorted(n for n in new if n in old and new[n] != old[n])
        if not added and not changed:
            return None
        parts = ([f"new: {', '.join(added)}"] if added else []) + ([f"changed: {', '.join(changed)}"] if changed else [])
        return ("Note: the server was restarted and its tools differ from the list this client was given (" + "; ".join(parts) +
                "). Reload your tool list to use them (e.g. ToolSearch).")

    def start_child(self):
        env = dict(os.environ)
        if self.java_options:  # JVM options for the server only: etc/settings of the component appends this variable
            env["PIDE_MCP_JVM_OPTIONS"] = (env.get("PIDE_MCP_JVM_OPTIONS", "") + " " + self.java_options).strip()
        self.child = subprocess.Popen(self.cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                      text=True, bufsize=1, stderr=None, start_new_session=True, env=env)
        self.started = time.time()
        threading.Thread(target=self.reader, args=(self.child,), daemon=True).start()

    def stop_child(self):
        """Stop the server and everything below it. Closing its stdin makes the Java server shut down cleanly
        (it stops its Isabelle processes); SIGTERM on the bash wrapper alone would leave java and poly running
        and holding their memory, because the old pipe stays open here."""
        child = self.child
        if not child:
            return
        tree = [child.pid] + descendants(child.pid)
        try:
            child.stdin.close()
        except OSError:
            pass
        if not wait_gone(tree, 30):
            log("server did not exit after stdin EOF; terminating its process tree")
            for sig, grace in ((signal.SIGTERM, 10), (signal.SIGKILL, 5)):
                for pid in reversed(tree):
                    if is_alive(pid):
                        try:
                            os.kill(pid, sig)
                        except OSError:
                            pass
                if wait_gone(tree, grace):
                    break
        try:
            child.wait(timeout=5)
        except subprocess.TimeoutExpired:
            pass

    def handshake(self):
        """Replay the client's initialize handshake to a freshly started child."""
        self.init_done.clear()
        req = dict(self.init_request)
        req["id"] = "proxy-init"
        self.to_child(req)
        if not self.init_done.wait(self.init_timeout):
            raise RuntimeError("new server did not answer initialize")
        if self.initialized:
            self.to_child(self.initialized)
        # the new server's tool list, to tell the client what changed (best effort)
        try:
            if self.client_tools is not None:
                self.tools_done.clear()
                self.new_tools = None
                self.to_child({"jsonrpc": "2.0", "id": "proxy-tools", "method": "tools/list"})
                if self.tools_done.wait(30) and self.new_tools is not None:
                    self.note = self.make_note(self.client_tools, self.new_tools)
        except Exception as e:
            log(f"could not compare tool lists: {e!r}")

    def restart(self, why):
        log(f"restarting server ({why})")
        self.stop_child()
        if self.build_cmd:
            r = subprocess.run(self.build_cmd, cwd="/tmp", stdout=sys.stderr, stderr=sys.stderr)
            if r.returncode != 0:
                log(f"build failed with {r.returncode}; starting anyway")
        self.start_child()
        if self.init_request:
            self.handshake()
            self.to_client(json.dumps({"jsonrpc": "2.0", "method": "notifications/tools/list_changed"}))
        log("server restarted")

    def maybe_restart(self):
        with self.state_lock:
            now = time.time()
            # a cancelled request is never answered (see below) and a lost response must not block restarts forever:
            # requests older than the longest tool wait (3600 s) no longer count
            busy = any(now - t < self.max_request_age for t in self.pending.values())
        if busy:
            return
        if self.child.poll() is not None:
            self.restart("server exited")
        elif newest_mtime(self.root) > self.started:
            self.restart("sources changed")

    # --- reap a child that died on its own (killed, crashed): without this its wrapper stays a zombie
    # until the next request, and a dead server holds no memory but hides that it needs a restart
    def watchdog(self):
        reported = None
        while True:
            time.sleep(3)
            child = self.child
            if child is not None and child.poll() is not None and reported is not child:
                reported = child
                log(f"server (pid {child.pid}) exited with {child.returncode}; it restarts on the next request")

    # --- main loop
    def run(self):
        self.start_child()
        threading.Thread(target=self.watchdog, daemon=True).start()
        try:
            for line in sys.stdin:
                try:
                    msg = json.loads(line)
                except ValueError:
                    msg = None
                if isinstance(msg, dict):
                    method = msg.get("method")
                    if method == "initialize":
                        self.init_request = msg
                    elif method == "notifications/initialized":
                        self.initialized = msg
                    elif method == "notifications/cancelled":
                        with self.state_lock:
                            self.pending.pop((msg.get("params") or {}).get("requestId"), None)
                    elif method and "id" in msg and self.init_request is not None:
                        self.maybe_restart()
                    if method and "id" in msg:
                        with self.state_lock:
                            self.pending[msg["id"]] = time.time()
                            if method == "tools/list":
                                self.list_ids.add(msg["id"])
                            elif method == "tools/call":
                                self.call_ids.add(msg["id"])
                self.to_child(line)
        finally:
            self.stop_child()

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--root", required=True, help="repository whose sources are watched")
    ap.add_argument("--build-cmd", default="flock /tmp/pide-mcp-build.lock isabelle scala_build",
                    help="run before restarting ('' to skip); the lock serialises concurrent proxies")
    ap.add_argument("--java-options", default="-XX:ZUncommitDelay=30",
                    help="passed as PIDE_MCP_JVM_OPTIONS to the server (ZGC returns unused heap after 30 s instead of 300 s);  = none")
    ap.add_argument("--init-timeout", type=float, default=900, help="seconds to wait for the new server's initialize")
    ap.add_argument("cmd", nargs=argparse.REMAINDER, help="-- <server command ...>")
    a = ap.parse_args()
    cmd = a.cmd[1:] if a.cmd and a.cmd[0] == "--" else a.cmd
    if not cmd:
        ap.error("missing server command after --")
    signal.signal(signal.SIGTERM, lambda *_: sys.exit(0))
    Proxy(a.root, cmd, a.build_cmd.split() if a.build_cmd else None, a.init_timeout, a.java_options).run()

if __name__ == "__main__":
    main()
