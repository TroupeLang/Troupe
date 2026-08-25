"""tmuxdrv -- drive a battallion session through a real tmux pane.

Everything this module assumes about tmux is verified by probe_tmux.sh and
probe_sendkeys.sh; the two findings that shaped it:

  * `send-keys -l ';'` is swallowed -- a standalone ";" argument is tmux's own
    command separator.  A literal that is exactly ";" is sent as "\\;" instead.
  * capture-pane -p drops trailing blank rows, so the capture is padded back to
    the pane height before it is compared.
"""

import os
import subprocess
import time

# This file sits at <checkout>/examples/battallion/testing/sim/, so the checkout
# root is four directories up.  BT_ROOT overrides it for a run against another
# checkout.
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.environ.get(
    "BT_ROOT", os.path.normpath(os.path.join(HERE, "..", "..", "..", "..")))

# Where the simulator's scratch lives: the compiled editor, the generated test
# files, and the per-run directories.  BT_SCRATCH overrides it.
SCRATCH = os.environ.get("BT_SCRATCH", "/tmp/battallion-sim")
BT_JS = os.environ.get("BT_JS", os.path.join(SCRATCH, "bt-current.js"))

# The runtime reads the dependencies pin file cwd-relative
# (rt/built/loadLibsAsync.mjs:76 -> moduleResolver.seedModuleResolver) and
# resolves each module to <cwd>/examples/battallion/out/<Name>.js.  BT_JS is
# compiled from the working tree with
#
#     bin/troupec examples/battallion/bt.trp -m --output=<SCRATCH>/bt-current.js
#
# which also refreshes examples/battallion/out/, so the working tree itself is
# the tree the pins resolve against and no frozen copy is needed.
CWD = ROOT


def tmux(*args, check=True):
    r = subprocess.run(["tmux"] + list(args), capture_output=True, text=True)
    if check and r.returncode != 0:
        raise RuntimeError("tmux %s failed: %s" % (" ".join(args), r.stderr.strip()))
    return r.stdout


class Pane:
    """One editor session in one tmux pane."""

    def __init__(self, session, io_root, filename, cols, rows, errlog=None):
        self.session = session
        self.io_root = io_root
        self.filename = filename
        self.cols, self.rows = cols, rows
        self.errlog = errlog or os.path.join(io_root, "stderr.log")
        cmd = ("node %s/rt/built/troupe.mjs -f=%s --localonly --io-root %s -- %s 2>%s"
               % (ROOT, BT_JS, io_root, filename, self.errlog))
        tmux("new-session", "-d", "-x", str(cols), "-y", str(rows),
             "-s", session, "-c", CWD, cmd)
        tmux("set-option", "-t", session, "remain-on-exit", "on")

    # -- input --------------------------------------------------------------

    def send_literal(self, s):
        tmux("send-keys", "-t", self.session, "-l", "\\;" if s == ";" else s)

    def send_key(self, name):
        tmux("send-keys", "-t", self.session, name)

    def resize(self, cols, rows):
        tmux("resize-window", "-t", self.session, "-x", str(cols), "-y", str(rows))
        self.cols, self.rows = cols, rows

    # -- output -------------------------------------------------------------

    def capture(self):
        """(screen rows padded to the pane height, cursor_x, cursor_y, dead).

        One tmux invocation for all of it: capture-pane and display-message are
        chained with tmux's own ";" separator so the screen and the cursor come
        from the same client round trip."""
        out = tmux("capture-pane", "-p", "-t", self.session, ";",
                   "display-message", "-p", "-t", self.session,
                   "@@@#{cursor_x} #{cursor_y} #{pane_dead} #{pane_dead_status}")
        head, _, tail = out.rpartition("@@@")
        rows = head.split("\n")
        if rows and rows[-1] == "":
            rows.pop()
        while len(rows) < self.rows:
            rows.append("")
        f = tail.split()
        cx, cy, dead = int(f[0]), int(f[1]), f[2] == "1"
        status = int(f[3]) if dead and len(f) > 3 else None
        return rows, cx, cy, dead, status

    def alive(self):
        return tmux("display-message", "-p", "-t", self.session,
                    "#{pane_dead}", check=False).strip() == "0"

    def kill(self):
        tmux("kill-session", "-t", self.session, check=False)


def wait_stable(pane, min_ms=130, poll_ms=5, timeout_ms=4000, want=None, t0=None):
    """Poll until the pane stops changing.  Returns (capture, ms_to_match).

    `ms_to_match` is the time from `t0` (the moment the last keystroke was sent)
    to the first capture that equals `want`, the model's expectation.  It is
    None when the pane already matched at the first poll -- a keystroke that
    produced no observable change has no latency to report -- and None when the
    pane never matched, which the caller reports as a divergence anyway."""
    t0 = t0 or time.time()
    prev = None
    stable = 0
    matched = None
    first = True
    while True:
        cap = pane.capture()
        el = (time.time() - t0) * 1000
        if want is not None and matched is None:
            if ([r.rstrip() for r in cap[0]], cap[1], cap[2]) == want:
                matched = None if first else el
                first = False
            else:
                first = False
        if cap == prev:
            stable += 1
        else:
            stable = 0
        prev = cap
        if el >= min_ms and stable >= 2:
            return cap, matched
        if el >= timeout_ms:
            return cap, matched
        time.sleep(poll_ms / 1000.0)


def wait_started(pane, timeout_ms=30000):
    """The first frame: node's startup is seconds, so wait for the screen to
    stop being blank before waiting for it to stop changing."""
    t0 = time.time()
    while (time.time() - t0) * 1000 < timeout_ms:
        cap = pane.capture()
        if cap[3]:
            return cap
        if any(r.strip() for r in cap[0]):
            return wait_stable(pane, min_ms=250, timeout_ms=8000)[0]
        time.sleep(0.05)
    return pane.capture()
