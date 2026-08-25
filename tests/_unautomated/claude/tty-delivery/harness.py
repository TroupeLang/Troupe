#!/usr/bin/env python3
"""Pty checks for terminal event delivery.

The golden runner hands the program pipes, which covers the message shapes, the
labels and the receive ceremony. What it cannot cover is what only a real
terminal has: raw-mode keystrokes with no echo, Ctrl-C arriving as a byte
instead of a signal, SIGWINCH, and the liveness net acting on a subscriber that
died while the terminal was raw.

Each case runs a compiled program under a fresh pty pair. The parent keeps the
slave fd open so the pty's termios can be read before the child starts, while it
runs, and after it exits; a fresh pair per case keeps a case that leaves the pty
raw from contaminating the next one.

Usage:
  python3 tests/_unautomated/claude/tty-delivery/harness.py "$PWD"
"""
import fcntl
import os
import pty
import select
import struct
import sys
import termios
import time

TROUPE = sys.argv[1]
COLS, ROWS = 80, 24

TIOCSCTTY = getattr(termios, "TIOCSCTTY", 0x20007461)


def set_winsize(fd, rows, cols):
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))


def snapshot(*fds):
    """The pty's termios, the same state `stty -g` prints."""
    last = None
    for fd in fds:
        try:
            return termios.tcgetattr(fd)
        except termios.error as e:
            last = e
    raise last


def is_raw(attrs):
    """Raw mode as the runtime sets it: canonical input off."""
    return not (attrs[3] & termios.ICANON)


def spawn(compiled):
    master, slave = pty.openpty()
    pid = os.fork()
    if pid == 0:
        os.close(master)
        os.setsid()
        fcntl.ioctl(slave, TIOCSCTTY, 0)
        os.dup2(slave, 0)
        os.dup2(slave, 1)
        os.dup2(slave, 2)
        if slave > 2:
            os.close(slave)
        os.execvp("node", [
            "node", os.path.join(TROUPE, "rt/built/troupe.mjs"),
            "-f=" + compiled, "--localonly"])
        os._exit(127)
    return pid, master, slave


def compile_prog(src):
    js = "/tmp/ttydelivery-%s.js" % os.path.basename(src).replace(".trp", "")
    rc = os.system('"%s/bin/troupec" %s -m --output=%s > /dev/null'
                   % (TROUPE, src, js))
    if rc != 0:
        raise SystemExit("compile failed: " + src)
    return js


HERE = os.path.dirname(os.path.abspath(__file__))
failures = []


def check(name, ok, detail=""):
    if isinstance(detail, list):
        detail = " | ".join(detail)
    print("  %-38s %s%s" % (name, "PASS" if ok else "FAIL",
                            ("  " + detail) if detail else ""))
    if not ok:
        failures.append(name)


class Run:
    """A running program under a pty, driven step by step.

    Cases that only inject and wait use `until`; case 4 also needs to read the
    termios and the process state at a chosen moment, which is why the runner is
    an object rather than one function.
    """

    def __init__(self, compiled, rows=ROWS, cols=COLS):
        self.pid, self.master, self.slave = spawn(compiled)
        set_winsize(self.master, rows, cols)
        self.before = snapshot(self.slave, self.master)
        self.out = b""
        self.exited = False
        self.status = None

    def pump(self, seconds):
        end = time.time() + seconds
        while time.time() < end:
            r, _, _ = select.select([self.master], [], [], 0.05)
            if r:
                try:
                    d = os.read(self.master, 4096)
                except OSError:
                    d = b""
                self.out += d
            if not self.exited:
                w, st = os.waitpid(self.pid, os.WNOHANG)
                if w == self.pid:
                    self.exited, self.status = True, st

    def until(self, marker, timeout=15):
        end = time.time() + timeout
        while time.time() < end:
            if marker in self.text():
                return True
            if self.exited:
                return marker in self.text()
            self.pump(0.1)
        return marker in self.text()

    def wait_exit(self, timeout=15):
        end = time.time() + timeout
        while time.time() < end and not self.exited:
            self.pump(0.1)
        return self.exited

    def inject(self, data):
        os.write(self.master, data)

    def alive(self):
        return not self.exited

    def text(self):
        return self.out.decode("utf8", "replace")

    def termios_now(self):
        return snapshot(self.slave, self.master)

    def finish(self):
        if not self.exited:
            os.kill(self.pid, 9)
            os.waitpid(self.pid, 0)
            self.exited = True
        after = snapshot(self.slave, self.master)
        os.close(self.master)
        os.close(self.slave)
        return after


def case1():
    print("case 1: raw mode + subscription, keystrokes arrive, no echo")
    r = Run(compile_prog(HERE + "/pty-case1-keys.trp"))
    r.until("READY")
    r.inject(b"abcXYZ")
    r.until("DONE")
    r.wait_exit()
    r.finish()
    out = r.text()
    check("keystrokes delivered as TTYDATA",
          "GOT: 97 98 99 88 89 90" in out,
          [l for l in out.splitlines() if "GOT:" in l])
    check("injected bytes not echoed", "abcXYZ" not in out)
    check("exited on its own", r.exited)
    return out


def case2():
    print("case 2: Ctrl-C in raw mode is a byte, not a signal")
    r = Run(compile_prog(HERE + "/pty-case2-ctrlc.trp"))
    r.until("READY")
    r.inject(b"\x03")
    r.until("DONE")
    r.wait_exit()
    r.finish()
    out = r.text()
    check("0x03 delivered as a byte", "GOT: 3" in out,
          [l for l in out.splitlines() if "GOT:" in l])
    check("runtime did not exit on the byte", "STILL-RUNNING" in out)
    check("exited on its own afterwards", r.exited)
    check("exit status 0", r.status == 0, "status=%s" % r.status)
    return out


def case3():
    print("case 3: a window-size change reaches the subscriber")
    r = Run(compile_prog(HERE + "/pty-case3-resize.trp"))
    r.until("READY")
    set_winsize(r.master, 40, 100)
    r.until("DONE")
    r.wait_exit()
    r.finish()
    out = r.text()
    check("size before the change", "SIZE-BEFORE:{tag=\"Ok\", value={cols=80, rows=24}}" in out,
          [l for l in out.splitlines() if "SIZE-BEFORE" in l])
    check("TTYRESIZE carries the new size", "RESIZE:100x40" in out,
          [l for l in out.splitlines() if "RESIZE:" in l])
    check("ttySize agrees afterwards",
          "SIZE-AFTER:{tag=\"Ok\", value={cols=100, rows=40}}" in out,
          [l for l in out.splitlines() if "SIZE-AFTER" in l])
    check("exited on its own", r.exited)
    return out


def case4():
    print("case 4: the liveness net restores the terminal after the")
    print("        subscriber dies, while the program runs on")
    r = Run(compile_prog(HERE + "/pty-case4-liveness.trp"))
    r.until("SUBSCRIBER-DEAD")
    mid = r.termios_now()
    check("terminal is raw with the subscriber gone", is_raw(mid))
    r.inject(b"k")
    r.pump(1.0)
    after_net = r.termios_now()
    check("terminal left raw mode on the next event", not is_raw(after_net))
    check("the program was still running when it happened", r.alive())
    check("no program help involved (MAIN-ALIVE not yet printed)",
          "MAIN-ALIVE" not in r.text())
    # Injected while nothing is subscribed. The teardown paused the stream, so
    # these bytes stay buffered instead of being read and dropped, and the
    # re-subscription below sees them: that is the observable consequence of
    # the pause. A stream left flowing would have consumed them and the
    # program would never reach GOT-AGAIN.
    r.inject(b"z\r")
    r.until("REARMED")
    got = r.until("GOT-AGAIN", timeout=8)
    check("stdin was paused: bytes sent while unsubscribed survive", got)
    r.until("DONE")
    r.wait_exit()
    r.finish()
    out = r.text()
    check("a later subscription works", "GOT-AGAIN" in out)
    check("exited on its own", r.exited)
    return out


outs = []
for c in (case1, case2, case3, case4):
    outs.append(c())
    print("")

if failures:
    print("FAILURES: " + ", ".join(failures))
    for o in outs:
        print("--- output ---")
        print(o)
    sys.exit(1)
print("ALL PASS")
