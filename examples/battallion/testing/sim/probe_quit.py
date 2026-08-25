#!/usr/bin/env python3
"""probe_quit.py -- the ways a session ends, and a write the runtime refuses.

The seeded generator never quits (it would end the run), so the quit paths are
checked here: the exit code, the file on disk after `:wq`, and the status line a
refused write leaves behind.

  python3 probe_quit.py
"""

import os
import shutil
import subprocess
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import btmodel as M
from simulate import Run, WORKROOT, make_files
from tmuxdrv import wait_started

FILE = "quit.txt"
TEXT = "alpha\nbeta\ngamma\n"


def session(name, cols=40, rows=8, readonly=False):
    outdir = os.path.join(WORKROOT, "probes", name)
    shutil.rmtree(outdir, ignore_errors=True)
    os.makedirs(outdir)
    r = Run(0, FILE, cols, rows, 0, False, outdir, [], "q" + name[:6])
    r.prepare()
    if readonly:
        os.chmod(os.path.join(r.io_root, FILE), 0o444)
    wait_started(r.pane)
    return r


def send(r, *sends):
    r.do_action({"kind": "probe", "sends": list(sends)}, check=False)


def final(r, wait_s=3.0):
    t0 = time.time()
    while time.time() - t0 < wait_s:
        cap = r.pane.capture()
        if cap[3]:
            return cap[4]
        time.sleep(0.1)
    return None


def main():
    files = os.path.join(WORKROOT, "files")
    if not os.path.isdir(files):
        make_files(files)
    with open(os.path.join(files, FILE), "w", encoding="utf-8") as f:
        f.write(TEXT)

    print("-- :q on a clean buffer")
    r = session("quit-clean")
    send(r, ["lit", ":q"], ["pause", 150], ["key", "Enter"], ["pause", 400])
    print("   exit status: %s (expect 0, Session.exitOk)" % final(r))
    r.pane.kill()

    print("-- q on a clean buffer")
    r = session("quit-q")
    send(r, ["lit", "q"], ["pause", 400])
    print("   exit status: %s" % final(r))
    r.pane.kill()

    print("-- CTRL-c on a clean buffer")
    r = session("quit-ctrlc")
    send(r, ["key", "C-c"], ["pause", 400])
    print("   exit status: %s" % final(r))
    r.pane.kill()

    print("-- :q refused on a modified buffer, then :q!")
    r = session("quit-dirty")
    send(r, ["lit", "i"], ["pause", 150], ["lit", "Z"], ["pause", 200],
         ["key", "Escape"], ["pause", 300])
    send(r, ["lit", ":q"], ["pause", 150], ["key", "Enter"], ["pause", 500])
    cap = r.pane.capture()
    print("   alive after :q  : %s" % (not cap[3]))
    print("   status row      : %r" % cap[0][r.rows - 1].rstrip())
    print("   model status row: %r" % M.from_u16(M.status_row(r.model.st)))
    send(r, ["lit", ":q!"], ["pause", 150], ["key", "Enter"], ["pause", 400])
    print("   exit status     : %s" % final(r))
    disk = open(os.path.join(r.io_root, FILE), encoding="utf-8").read()
    print("   file unchanged  : %s" % (disk == TEXT))
    r.pane.kill()

    print("-- :wq writes and quits")
    r = session("quit-wq")
    send(r, ["lit", "i"], ["pause", 150], ["lit", "Q"], ["pause", 200],
         ["key", "Escape"], ["pause", 300])
    send(r, ["lit", ":wq"], ["pause", 150], ["key", "Enter"], ["pause", 600])
    st = final(r)
    disk = open(os.path.join(r.io_root, FILE), "rb").read()
    want = M.from_u16(r.model.st.contents()).encode("utf-8")
    print("   exit status     : %s" % st)
    print("   file == model   : %s  (%r)" % (disk == want, disk[:20]))
    r.pane.kill()

    print("-- :x is a synonym for :wq")
    r = session("quit-x")
    send(r, ["lit", "i"], ["pause", 150], ["lit", "Y"], ["pause", 200],
         ["key", "Escape"], ["pause", 300])
    send(r, ["lit", ":x"], ["pause", 150], ["key", "Enter"], ["pause", 600])
    print("   exit status     : %s" % final(r))
    disk = open(os.path.join(r.io_root, FILE), "rb").read()
    print("   file == model   : %s"
          % (disk == M.from_u16(r.model.st.contents()).encode("utf-8")))
    r.pane.kill()

    print("-- a write the runtime refuses (read-only file)")
    r = session("write-refused", readonly=True)
    send(r, ["lit", "i"], ["pause", 150], ["lit", "W"], ["pause", 200],
         ["key", "Escape"], ["pause", 300])
    send(r, ["lit", ":w"], ["pause", 150], ["key", "Enter"], ["pause", 800])
    cap = r.pane.capture()
    print("   alive           : %s" % (not cap[3]))
    print("   status row      : %r" % cap[0][r.rows - 1].rstrip())
    print("   [+] still set   : %s" % ("[+]" in cap[0][r.rows - 1]))
    disk = open(os.path.join(r.io_root, FILE), encoding="utf-8").read()
    print("   file unchanged  : %s" % (disk == TEXT))
    r.pane.kill()

    print("-- EOF on stdin (the pipe closes)")
    p = subprocess.run(
        "cd %s && printf '' | node %s/rt/built/troupe.mjs -f=%s --localonly "
        "--io-root %s -- %s > /dev/null 2>&1"
        % (__import__("tmuxdrv").CWD, __import__("tmuxdrv").ROOT,
           __import__("tmuxdrv").BT_JS,
           os.path.join(WORKROOT, "probes", "quit-clean", "io"), FILE),
        shell=True)
    print("   exit status     : %s" % p.returncode)
    return 0


if __name__ == "__main__":
    sys.exit(main())
