#!/usr/bin/env python3
"""Run one probe program under a pseudo-terminal, feed it a key, then SIGTERM it.

probe-sigterm-in-region.trp needs three things the golden runner cannot give it:
a terminal to put into raw mode, a keystroke that arrives before any newline,
and a live process to signal. This is the smallest driver that supplies them.
harness.py is the full stage-4 check; this is for re-running a single probe.

Usage:
  python3 examples/battallion/testing/viewer/run-pty-probe.py \\
      "$PWD" examples/battallion/testing/viewer/probe-sigterm-in-region.trp
"""
import fcntl
import os
import pty
import select
import signal
import struct
import sys
import termios
import time

TROUPE = sys.argv[1]
SRC = sys.argv[2]
TIOCSCTTY = getattr(termios, "TIOCSCTTY", 0x20007461)

js = "/tmp/battallion-pty-probe.js"
if os.system('cd "%s" && ./bin/troupec %s -m --output=%s > /dev/null'
             % (TROUPE, os.path.relpath(os.path.abspath(SRC), TROUPE), js)) != 0:
    raise SystemExit("compile failed: " + SRC)

master, slave = pty.openpty()
fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
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
    os.execvp("node", ["node", os.path.join(TROUPE, "rt/built/troupe.mjs"),
                       "-f=" + js, "--localonly"])
    os._exit(127)

out = b""


def pump(seconds):
    global out
    end = time.time() + seconds
    while time.time() < end:
        r, _, _ = select.select([master], [], [], 0.05)
        if r:
            try:
                out += os.read(master, 65536)
            except OSError:
                return


def until(marker, timeout=25):
    end = time.time() + timeout
    while time.time() < end:
        if marker in out.decode("utf8", "replace"):
            return True
        pump(0.1)
    return marker in out.decode("utf8", "replace")


print("reached READY:", until("READY"))
os.write(master, b"a")
print("saw a key event:", until("EVENT:data"))
os.kill(pid, signal.SIGTERM)
pump(3)
try:
    os.waitpid(pid, os.WNOHANG)
except ChildProcessError:
    pass
print("----- output -----")
print(out.decode("utf8", "replace"))
