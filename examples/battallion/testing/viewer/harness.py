#!/usr/bin/env python3
"""Pty checks for the battallion viewer (mvp stage 4).

The viewer is interactive: it needs a terminal to size itself, raw mode to see a
keystroke before a newline, SIGWINCH to resize, and a live process to send
SIGTERM to. None of that is reachable from the golden runner, which hands a
program pipes, so every check here drives a real pty.

Each case compiles a program, runs it under a fresh pty pair, injects bytes or
signals, and reads the escape sequences the renderer wrote. A fresh pair per
case keeps a case that leaves the pty raw from contaminating the next one.

WHAT IS ASSERTED ABOUT THE TERMINAL, AND WHY IT IS ASSERTED THAT WAY.
The pty's termios is read while the child is alive, never after it has died: on
darwin the pty is revoked when the process that owns it exits, tcgetattr on the
slave then fails and the master reports the reset default, so a post-mortem
reading witnesses the pty tearing itself down rather than anything the program
did (the same confound the runtime's own signal-handling harness documents).
What makes the restore
observable while the process is still alive is the teardown order the viewer
uses: cooked mode is restored *before* the last escapes are written, so the
alternate-screen-leave sequence appearing in the output is proof that termios
was already back. Every restore case therefore asserts three things: raw mode
seen while the session ran, the leave sequence in the output, and the exit code.

Usage:
  python3 examples/battallion/testing/viewer/harness.py "$PWD"

Observed 2026-08-02 (dev-text-editor, display-width truncation in the tree),
88 checks, all PASS:
  case 1  open              alternate screen, wrap cleared, first viewport, cursor at (1,1), raw
  case 2  motions           h j k l, arrows, 0, $, scroll past the bottom, PageUp
  case 3  q                 leave alternate screen, wrap restored, cursor shown, exit 0
  case 4  kernel crash      terminal restored, exit 3 (Session.exitKernelLost)
  case 5  resize            10x40 to 20x100, redraw at the new size, truncation at 100
  case 6  SIGTERM           trapped, terminal restored, exit 0 (not signal death)
  case 7  unreadable file   message on stderr, terminal never touched, exit 1
  case 8  goal column       j and k over a short line, $ sticking to the ends, gg and G
  case 9  display width     i Tab Esc on a full-width line, tab stops in the rows and the cursor
"""
import fcntl
import os
import pty
import re
import select
import signal
import struct
import sys
import termios
import time

TROUPE = sys.argv[1]
HERE = os.path.dirname(os.path.abspath(__file__))
WORK = "/tmp/battallion-viewer"
COLS, ROWS = 80, 24
TIOCSCTTY = getattr(termios, "TIOCSCTTY", 0x20007461)

# The alternate-screen, wrap and cursor escapes Screen.trp writes.
ENTER_ALT = "\x1b[?1049h"
LEAVE_ALT = "\x1b[?1049l"
NO_WRAP = "\x1b[?7l"
WRAP = "\x1b[?7h"
HIDE_CUR = "\x1b[?25l"
SHOW_CUR = "\x1b[?25h"
CSI_H = re.compile(r"\x1b\[(\d+);(\d+)H")
TAB_STOP = 8


def rendered_width(row):
    """The columns `row` is drawn in, over the tab stops Screen.trp truncates by:
    a tab moves to the next multiple of TAB_STOP, everything else takes one."""
    col = 0
    for ch in row:
        col = col + TAB_STOP - (col % TAB_STOP) if ch == "\t" else col + 1
    return col

# The viewer and the crash probe, relative to this directory.
BT = "../../bt.trp"
CRASH = "../../crashprobe.trp"

failures = []


def check(name, ok, detail=""):
    print("  %-52s %s%s" % (name, "PASS" if ok else "FAIL",
                            ("  " + detail) if detail else ""))
    if not ok:
        failures.append(name)


def make_file(name, lines):
    os.makedirs(WORK, exist_ok=True)
    path = os.path.join(WORK, name)
    with open(path, "w") as f:
        f.write("\n".join(lines) + "\n")
    return path


def compile_prog(src):
    """Compile `src` and return the emitted JS.

    The source is named relative to the repository root, and the compiler is
    run from there: a program-relative module's pin key is the path the import
    resolves to, and compiling the same program by absolute path makes those
    keys absolute, which matches nothing in the .deps.json file. crashkernel.trp
    imports across directories and shows this at once."""
    os.makedirs(WORK, exist_ok=True)
    rel = os.path.relpath(src, TROUPE)
    js = os.path.join(WORK, os.path.basename(src).replace(".trp", ".js"))
    rc = os.system('cd "%s" && ./bin/troupec %s -m --output=%s > /dev/null'
                   % (TROUPE, rel, js))
    if rc != 0:
        raise SystemExit("compile failed: " + rel)
    return js


def set_winsize(fd, rows, cols):
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))


def is_raw(attrs):
    """Raw mode as the runtime sets it: canonical input off."""
    return not (attrs[3] & termios.ICANON)


class Run:
    """A viewer session under a pty, driven step by step."""

    def __init__(self, prog, filename, rows=ROWS, cols=COLS):
        js = compile_prog(os.path.normpath(os.path.join(HERE, prog)))
        self.master, self.slave = pty.openpty()
        set_winsize(self.master, rows, cols)
        self.before = termios.tcgetattr(self.slave)
        self.saw_raw = False
        self.out = b""
        self.exited = False
        self.status = None
        self.pid = os.fork()
        if self.pid == 0:
            os.close(self.master)
            os.setsid()
            fcntl.ioctl(self.slave, TIOCSCTTY, 0)
            os.dup2(self.slave, 0)
            os.dup2(self.slave, 1)
            os.dup2(self.slave, 2)
            if self.slave > 2:
                os.close(self.slave)
            os.execvp("node", [
                "node", os.path.join(TROUPE, "rt/built/troupe.mjs"),
                "-f=" + js, "--localonly",
                "--io-root", WORK, "--", filename])
            os._exit(127)

    def text(self):
        return self.out.decode("utf8", "replace")

    def termios_now(self):
        """The pty's termios, or None once the child has gone."""
        try:
            return termios.tcgetattr(self.slave)
        except termios.error:
            return None

    def pump(self, seconds):
        end = time.time() + seconds
        while time.time() < end:
            r, _, _ = select.select([self.master], [], [], 0.02)
            if r:
                try:
                    self.out += os.read(self.master, 65536)
                except OSError:
                    pass
            if not self.exited:
                attrs = self.termios_now()
                if attrs is not None and is_raw(attrs):
                    self.saw_raw = True
                w, st = os.waitpid(self.pid, os.WNOHANG)
                if w == self.pid:
                    self.exited, self.status = True, st

    def until(self, predicate, timeout=25):
        end = time.time() + timeout
        while time.time() < end:
            if predicate(self.text()):
                return True
            if self.exited:
                return predicate(self.text())
            self.pump(0.05)
        return predicate(self.text())

    def quiet(self, seconds=0.4):
        """Let the screen settle, then return everything written since the
        marker the caller took."""
        self.pump(seconds)
        return self.text()

    def send(self, data):
        os.write(self.master, data)

    def wait_exit(self, timeout=25):
        end = time.time() + timeout
        while time.time() < end and not self.exited:
            self.pump(0.1)
        return self.exited

    def exit_code(self):
        if self.status is None:
            return None
        if os.WIFEXITED(self.status):
            return os.WEXITSTATUS(self.status)
        return -os.WTERMSIG(self.status)

    def close(self):
        if not self.exited:
            os.kill(self.pid, signal.SIGKILL)
            os.waitpid(self.pid, 0)
            self.exited = True
        for fd in (self.master, self.slave):
            try:
                os.close(fd)
            except OSError:
                pass


def last_cursor(text):
    """The last cursor-position escape in `text`, as (row, col)."""
    found = CSI_H.findall(text)
    return (int(found[-1][0]), int(found[-1][1])) if found else None


def frames(text):
    """How many full-frame redraws the renderer wrote. Every frame starts by
    homing the cursor."""
    return text.count("\x1b[H")


CSI_ANY = re.compile(r"\x1b\[[0-9;?]*[A-Za-z]")


def frame_rows(text):
    """The rows of the last full frame, with the escapes stripped. A frame is
    everything after the last cursor-home, its rows separated by CRLF, each row
    erased to the end of the line and the last one followed by the cursor
    position.

    The trailing CR is stripped because raw mode leaves output post-processing
    on: the LF of the CRLF the renderer writes is expanded to CRLF again, so
    each row arrives ending in CR CR LF."""
    last = text.split("\x1b[H")[-1]
    return [CSI_ANY.sub("", row).rstrip("\r") for row in last.split("\r\n")]


# The buffer every case opens: more lines than the pty has rows, each line
# naming itself, and long enough at the bottom to be cut by an 80-column
# terminal but not by a 200-column one.
LINES = ["line %02d %s" % (i, "y" * (i if i < 30 else 120)) for i in range(1, 61)]


def case1():
    print("case 1: the file opens on the alternate screen")
    r = Run(BT, "notes.txt")
    try:
        ok = r.until(lambda t: "line 23" in t)
        text = r.quiet()
        check("alternate screen entered", ENTER_ALT in text)
        # Automatic wrap is cleared with the alternate-screen switch and stays
        # cleared for the session: a row wider than the terminal is then clipped
        # at the right edge instead of continuing onto the next physical row and
        # scrolling the frame's top row away (Screen.trp's header).
        check("automatic wrap cleared at startup", NO_WRAP in text)
        check("the wrap escape comes with the alternate-screen switch, before "
              "any frame",
              text.find(ENTER_ALT) < text.find(NO_WRAP) < text.find(HIDE_CUR),
              "alt=%d wrap=%d hide=%d"
              % (text.find(ENTER_ALT), text.find(NO_WRAP), text.find(HIDE_CUR)))
        check("wrap is never set again while the session runs", WRAP not in text)
        # The cursor is hidden for the length of a frame, not the length of the
        # session: the session writes no hide of its own, and every frame ends
        # by showing the cursor again, so it is on the screen between frames.
        check("the frame hid the cursor while it drew", HIDE_CUR in text)
        check("the cursor is shown again once the frame is drawn",
              text.rfind(SHOW_CUR) > text.rfind(HIDE_CUR))
        check("first viewport line drawn", "line 01" in text)
        check("last viewport line drawn", "line 23" in text, "rows=%d" % ROWS)
        check("nothing past the viewport drawn", "line 25 " not in text)
        check("cursor positioned", last_cursor(text) is not None,
              str(last_cursor(text)))
        check("cursor starts at the first cell", last_cursor(text) == (1, 1))
        check("raw mode entered", r.saw_raw)
        return r, text
    finally:
        pass


def case2(r):
    print("case 2: motions move the cursor and scroll the viewport")
    mark = len(r.text())
    r.send(b"jjj")
    r.until(lambda t: frames(t[mark:]) >= 3)
    text = r.quiet()
    check("j moved the cursor down three rows", last_cursor(text) == (4, 1),
          str(last_cursor(text)))

    mark = len(r.text())
    r.send(b"\x1b[B\x1b[B")            # two down-arrows
    r.until(lambda t: frames(t[mark:]) >= 2)
    text = r.quiet()
    check("the down arrow moves like j", last_cursor(text) == (6, 1),
          str(last_cursor(text)))

    mark = len(r.text())
    r.send(b"kk")
    r.until(lambda t: frames(t[mark:]) >= 2)
    text = r.quiet()
    check("k moves back up", last_cursor(text) == (4, 1), str(last_cursor(text)))

    mark = len(r.text())
    r.send(b"\x1b[C\x1b[C\x1b[C")      # three right-arrows
    r.until(lambda t: frames(t[mark:]) >= 3)
    text = r.quiet()
    check("the right arrow moves the column", last_cursor(text) == (4, 4),
          str(last_cursor(text)))

    mark = len(r.text())
    r.send(b"$")
    r.until(lambda t: frames(t[mark:]) >= 1)
    text = r.quiet()
    check("$ goes to the end of the line",
          last_cursor(text) == (4, 1 + len(LINES[3])), str(last_cursor(text)))

    mark = len(r.text())
    r.send(b"0")
    r.until(lambda t: frames(t[mark:]) >= 1)
    text = r.quiet()
    check("0 goes back to the first column", last_cursor(text) == (4, 1),
          str(last_cursor(text)))

    # Past the bottom of the viewport: the window must shift.
    mark = len(r.text())
    r.send(b"j" * 25)
    r.until(lambda t: "line 29" in t[mark:], timeout=30)
    text = r.quiet(1.0)
    after = text[mark:]
    check("a line below the first viewport is now drawn", "line 29" in after)
    check("the viewport scrolled off line 01", "line 01 " not in after.split("\x1b[H")[-1])
    check("the cursor is on the last row", last_cursor(text) == (ROWS - 1, 1),
          str(last_cursor(text)))

    mark = len(r.text())
    r.send(b"\x1b[5~")                 # PageUp
    r.until(lambda t: frames(t[mark:]) >= 1)
    text = r.quiet()
    check("PageUp scrolls a screen back", "line 06" in text[mark:])
    return r


def case3(r):
    print("case 3: q quits and gives the terminal back")
    saw_raw = r.saw_raw
    r.send(b"q")
    ok = r.wait_exit()
    text = r.text()
    check("the session exited", ok)
    check("raw mode had been entered", saw_raw)
    check("alternate screen left", LEAVE_ALT in text)
    check("automatic wrap set again", WRAP in text)
    check("wrap was restored before the alternate screen was left",
          text.rfind(WRAP) < text.rfind(LEAVE_ALT))
    check("cursor left shown", text.rfind(SHOW_CUR) > text.rfind(HIDE_CUR))
    check("the leave sequence came after the last frame",
          text.rfind(LEAVE_ALT) > text.rfind("line 29"))
    check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    r.close()


def case4():
    print("case 4: a kernel that dies leaves the terminal restored")
    r = Run(CRASH, "notes.txt")
    try:
        r.until(lambda t: "line 23" in t)
        r.pump(0.3)
        check("raw mode entered", r.saw_raw)
        r.send(b"!")
        ok = r.wait_exit(timeout=30)
        text = r.text()
        check("the session exited", ok)
        check("the kernel really died",
              "Runtime error in thread" in text or "runtime error" in text.lower())
        check("alternate screen left", LEAVE_ALT in text)
        check("cursor left shown", text.rfind(SHOW_CUR) > text.rfind(HIDE_CUR))
        check("exit code is exitKernelLost (3)", r.exit_code() == 3,
              "rc=%s" % r.exit_code())
    finally:
        r.close()


def case5():
    print("case 5: a resize redraws at the new size")
    r = Run(BT, "notes.txt", rows=10, cols=40)
    try:
        r.until(lambda t: "line 09" in t)
        text = r.quiet()
        check("the small viewport shows 9 body rows", "line 09" in text and "line 10 " not in text)
        narrow = [row for row in frame_rows(text) if row.startswith("line 09")]
        check("a line is cut to the narrow width", narrow and len(narrow[-1]) <= 40,
              "drawn width=%s" % (len(narrow[-1]) if narrow else None))
        mark = len(r.text())

        set_winsize(r.master, 20, 100)
        os.kill(r.pid, signal.SIGWINCH)
        r.until(lambda t: "line 19" in t[mark:], timeout=20)
        text = r.quiet(0.6)
        after = text[mark:]
        check("the taller viewport shows 19 body rows", "line 19" in after)
        # Width: every line from 30 on is 120 y's long, so the bottom of the file
        # is cut at the terminal's width. Three PageDowns reach it.
        mark2 = len(r.text())
        r.send(b"\x1b[6~" * 3)
        r.until(lambda t: "line 60" in t[mark2:], timeout=20)
        text = r.quiet(0.8)
        drawn = [row for row in frame_rows(text) if row.startswith("line ")]
        widest = max((len(row) for row in drawn), default=None)
        check("a long line is cut to the new width", widest == 100,
              "widest drawn=%s, untruncated=%d, rows=%d"
              % (widest, len(LINES[59]), len(drawn)))
        r.send(b"q")
        r.wait_exit()
        check("alternate screen left", LEAVE_ALT in r.text())
        check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()


def case6():
    print("case 6: SIGTERM is trapped and the terminal is given back")
    r = Run(BT, "notes.txt")
    try:
        r.until(lambda t: "line 23" in t)
        r.pump(0.3)
        check("raw mode entered", r.saw_raw)
        os.kill(r.pid, signal.SIGTERM)
        ok = r.wait_exit(timeout=30)
        text = r.text()
        check("the session exited", ok)
        check("not killed by the signal", r.exit_code() is not None and r.exit_code() >= 0,
              "rc=%s" % r.exit_code())
        check("alternate screen left", LEAVE_ALT in text)
        check("cursor left shown", text.rfind(SHOW_CUR) > text.rfind(HIDE_CUR))
        check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()


def case7():
    print("case 7: a file that cannot be read fails before the terminal is touched")
    r = Run(BT, "no-such-file.txt")
    try:
        r.wait_exit(timeout=25)
        text = r.text()
        check("a message names the file", "cannot open" in text, text.strip()[-90:])
        check("the alternate screen was never entered", ENTER_ALT not in text)
        check("the terminal's wrap was never touched",
              NO_WRAP not in text and WRAP not in text)
        check("raw mode was never entered", not r.saw_raw)
        check("exit code 1", r.exit_code() == 1, "rc=%s" % r.exit_code())
    finally:
        r.close()


# ---------------------------------------------------------------- case 8

# Four lines whose lengths differ, so that a walk down them shows what the
# remembered column does: wide, narrow, wide again, and an empty one.
STEPS = ["0123456789abcdefghij", "abc", "0123456789abcdefghij", ""]


def case8():
    print("case 8: the column is remembered across vertical motions")
    make_file("steps.txt", STEPS)
    r = Run(BT, "steps.txt")
    try:
        r.until(lambda t: "abcdefghij" in t)
        r.quiet(0.5)

        # Column 13 on the first line, then down onto a line of three
        # characters: the cursor goes to that line's end and the column it came
        # from is remembered rather than lost.
        r.send(b"l" * 12)
        r.until(lambda t: last_cursor(t) == (1, 13))
        text = r.quiet()
        check("twelve rights put the cursor at column 13",
              last_cursor(text) == (1, 13), str(last_cursor(text)))

        mark = len(r.text())
        r.send(b"j")
        r.until(lambda t: frames(t[mark:]) >= 1)
        text = r.quiet()
        check("j onto a short line stops at that line's end",
              last_cursor(text) == (2, 4), str(last_cursor(text)))

        mark = len(r.text())
        r.send(b"j")
        r.until(lambda t: frames(t[mark:]) >= 1)
        text = r.quiet()
        check("j onto a long line returns to the remembered column",
              last_cursor(text) == (3, 13), str(last_cursor(text)))

        mark = len(r.text())
        r.send(b"jk")
        r.until(lambda t: frames(t[mark:]) >= 2)
        text = r.quiet()
        check("an empty line and back keeps the column",
              last_cursor(text) == (3, 13), str(last_cursor(text)))

        # A horizontal motion sets a new goal, and the walk aims at that.
        mark = len(r.text())
        r.send(b"hhj")
        r.until(lambda t: frames(t[mark:]) >= 3)
        text = r.quiet()
        check("a horizontal motion sets the column the next j aims for",
              last_cursor(text) == (4, 1), str(last_cursor(text)))

        mark = len(r.text())
        r.send(b"k")
        r.until(lambda t: frames(t[mark:]) >= 1)
        text = r.quiet()
        check("and k aims for it too", last_cursor(text) == (3, 11),
              str(last_cursor(text)))

        # $ aims at the end of the line rather than at a column, so a walk from
        # it follows the ends.
        mark = len(r.text())
        r.send(b"gg$")
        r.until(lambda t: frames(t[mark:]) >= 2)
        text = r.quiet()
        check("gg goes to the first line", last_cursor(text)[0] == 1,
              str(last_cursor(text)))
        check("$ goes one past the last character",
              last_cursor(text) == (1, 21), str(last_cursor(text)))

        mark = len(r.text())
        r.send(b"j")
        r.until(lambda t: frames(t[mark:]) >= 1)
        text = r.quiet()
        check("j after $ goes to the short line's end",
              last_cursor(text) == (2, 4), str(last_cursor(text)))

        mark = len(r.text())
        r.send(b"j")
        r.until(lambda t: frames(t[mark:]) >= 1)
        text = r.quiet()
        check("and on to the next line's end rather than to column 4",
              last_cursor(text) == (3, 21), str(last_cursor(text)))

        # G and gg are the ends of the buffer, at column one.
        mark = len(r.text())
        r.send(b"G")
        r.until(lambda t: frames(t[mark:]) >= 1)
        text = r.quiet()
        check("G goes to the last line at the first column",
              last_cursor(text) == (5, 1), str(last_cursor(text)))

        mark = len(r.text())
        r.send(b"gg")
        r.until(lambda t: frames(t[mark:]) >= 2)
        text = r.quiet()
        check("gg goes to the first line at the first column",
              last_cursor(text) == (1, 1), str(last_cursor(text)))

        # A key that does not complete the prefix spends it and does nothing.
        mark = len(r.text())
        r.send(b"gj")
        r.until(lambda t: frames(t[mark:]) >= 2)
        text = r.quiet()
        check("a prefix followed by an unbound key leaves the cursor alone",
              last_cursor(text) == (1, 1), str(last_cursor(text)))

        r.send(b":q!\r")
        ok = r.wait_exit()
        check("the session exited", ok)
        check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()


# ---------------------------------------------------------------- case 9

# A first line that exactly fills an 80-column terminal. One tab typed in front
# of it makes the row eight columns too wide, which is the one-keystroke
# reproduction of the frame destruction the session simulator found: the row
# used to be cut to 79 code units, still be 87 columns wide, continue onto a
# second physical row, scroll the alternate screen, take the frame's top row
# with it and leave the wrapped tail over the row below -- and every redraw
# after it repeated the damage. Two things stop it: the terminal's automatic
# wrap is off, so an over-wide row is clipped rather than continued (asserted in
# case 1), and truncation counts the columns a row is drawn in rather than its
# code units, so the tab's eight are subtracted from what fits.
WIDE = ["z" * 79, "second line", "third line"]

# Tab-indented text, the everyday case: the rows are cut at the tab stops and
# the cursor is drawn on the character after the indent rather than left of it.
TABBED = ["fun main () =", "\tlet val x = 1", "\t\tval y = 2", "\tin x + y", "\tend"]


def case9():
    print("case 9: rows are cut by the columns they are drawn in, not by code units")
    make_file("wide.txt", WIDE)
    r = Run(BT, "wide.txt", rows=8, cols=80)
    try:
        r.until(lambda t: "second line" in t)
        text = r.quiet(0.5)
        rows = frame_rows(text)
        check("the opening frame draws one row per terminal row", len(rows) == 8,
              "rows=%d" % len(rows))
        check("a line that exactly fills the width is drawn whole",
              rows[0] == "z" * 79, "%d z's" % rows[0].count("z"))

        # i Tab Esc. The Esc is read as the Esc key once the escape timer
        # elapses, so the frame that shows normal mode again is the one to read.
        mark = len(r.text())
        r.send(b"i\t")
        r.until(lambda t: frames(t[mark:]) >= 1)
        r.send(b"\x1b")
        r.until(lambda t: "NORMAL" in frame_rows(t)[-1])
        text = r.quiet(0.6)
        rows = frame_rows(text)
        widths = [rendered_width(row) for row in rows]
        check("the frame still draws one row per terminal row", len(rows) == 8,
              "rows=%d" % len(rows))
        check("no row is drawn wider than the terminal", max(widths) <= COLS,
              "widths=%s" % widths)
        check("the tabbed row is cut where its columns run out",
              rows[0] == "\t" + "z" * 72,
              "%d z's over %d columns" % (rows[0].count("z"),
                                          rendered_width(rows[0])))
        check("the row below it is intact", rows[1] == "second line",
              repr(rows[1]))
        check("the status line is still the bottom row",
              rows[7].startswith("wide.txt [+]"), repr(rows[7]))
        check("the cursor is drawn on the character after the tab",
              last_cursor(text) == (1, 9), str(last_cursor(text)))
        r.send(b":q!\r")
        r.wait_exit()
        check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()

    make_file("tabs.txt", TABBED)
    r = Run(BT, "tabs.txt", rows=8, cols=80)
    try:
        r.until(lambda t: "val y" in t)
        text = r.quiet(0.5)
        rows = frame_rows(text)
        check("an indented row that fits is drawn whole, tab and all",
              rows[1] == "\tlet val x = 1", repr(rows[1]))
        check("and it is drawn over the tab stop plus its text",
              rendered_width(rows[1]) == 8 + 13,
              "columns=%d" % rendered_width(rows[1]))

        mark = len(r.text())
        r.send(b"j")
        r.until(lambda t: frames(t[mark:]) >= 1)
        text = r.quiet()
        check("the cursor on the tab itself is drawn at the first column",
              last_cursor(text) == (2, 1), str(last_cursor(text)))

        mark = len(r.text())
        r.send(b"l")
        r.until(lambda t: frames(t[mark:]) >= 1)
        text = r.quiet()
        check("the column after the tab is drawn at the tab stop",
              last_cursor(text) == (2, 9), str(last_cursor(text)))

        mark = len(r.text())
        r.send(b"j0ll")
        r.until(lambda t: frames(t[mark:]) >= 4)
        text = r.quiet()
        check("two tabs put the character after them at the second stop",
              last_cursor(text) == (3, 17), str(last_cursor(text)))

        mark = len(r.text())
        r.send(b"$")
        r.until(lambda t: frames(t[mark:]) >= 1)
        text = r.quiet()
        check("the end of an indented line counts the indent's columns",
              last_cursor(text) == (3, 26), str(last_cursor(text)))

        # The same rows on a terminal too narrow for them.
        mark = len(r.text())
        set_winsize(r.master, 8, 20)
        os.kill(r.pid, signal.SIGWINCH)
        r.until(lambda t: frames(t[mark:]) >= 1)
        text = r.quiet(0.6)
        rows = frame_rows(text)
        widths = [rendered_width(row) for row in rows]
        check("no row is drawn wider than the narrow terminal",
              max(widths) <= 20, "widths=%s" % widths)
        check("an indented row is cut at the column the terminal ends at",
              rows[1] == "\tlet val x = ", repr(rows[1]))
        check("a doubly indented row keeps only what the two stops leave",
              rows[2] == "\t\tval ", repr(rows[2]))
        check("a cursor past the right edge is drawn at the last column",
              last_cursor(text) == (3, 20), str(last_cursor(text)))

        r.send(b":q!\r")
        r.wait_exit()
        check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()


make_file("notes.txt", LINES)

r = None
try:
    r, _ = case1()
    print("")
    case2(r)
    print("")
    case3(r)
    r = None
    print("")
    case4()
    print("")
    case5()
    print("")
    case6()
    print("")
    case7()
    print("")
    case8()
    print("")
    case9()
finally:
    if r is not None:
        r.close()

print("")
if failures:
    print("FAILURES: " + ", ".join(failures))
    sys.exit(1)
print("ALL PASS")
