#!/usr/bin/env python3
"""Pty checks for battallion's cursor discipline (mvp stage 6).

The plugin extraction this directory was made for is checked by the stage-4 and
stage-5 harnesses (examples/battallion/testing/{viewer,editor}/
harness.py): what moved into plugin modules kept its behaviour. What this one
checks is what the editor does with the terminal's cursor, which is the one
thing the plugin set still has a seam for and presently does nothing with. The
pty machinery is reproduced here rather than imported, those modules running
their own cases at import time.

WHAT IS ASSERTED, AND WHY IT IS THIS AND NOT SHAPES. Until 2026-08-01 this
harness asserted CursorStyle.trp's DECSCUSR shape against the cursor's position
— a bar on a character, an underscore past the end of a line. Those 25 checks
are superseded: the editor's owner ran the editor, found the cursor invisible
(the session hid it at startup and showed it only at teardown, so a shape
selected nothing anybody could see), and decided that battallion draws the
user's own default cursor and selects no shape at all until there is a reason
to. CursorStyle.trp survives as the seam and emits nothing. So what is checked
here is the property that decision turns on:

  - no DECSCUSR byte is written, in any frame or at teardown;
  - a frame hides the cursor, draws, positions the cursor, and shows it again
    as the last thing it writes — the hide being what stops a full-frame redraw
    from trailing the cursor across the screen (Screen.trp's header);
  - the cursor is therefore visible at rest, which is nearly always, and that
    is asserted in normal, insert and command mode separately, because the
    frame is assembled per mode (Editor.frameOf);
  - the session writes no hide of its own, so nothing outlives a frame;
  - teardown leaves the cursor shown.

Usage:
  python3 examples/battallion/testing/plugins/harness.py "$PWD"

Observed 2026-08-02 (dev-text-editor, display-width truncation in the tree),
58 checks, all PASS:
  case 1  frames        the per-frame hide/show discipline, and no shape bytes
  case 2  modes         the cursor is shown and placed in all three modes
  case 3  teardown      the last word on the cursor is a show, and no reset

Case 4 was added with the help page: it is here rather than with the editing
checks because what `:help` changes is the *frame* — the rows above the status
line come from the plugin set's `help` hook instead of from the buffer, and
nothing else about the state moves. That is a claim about frame assembly
(Editor.frameOf), which is what this harness reads.

  case 4  help page     the frame the overlay draws, and the buffer it gives back
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
WORK = "/tmp/battallion-plugins"
COLS, ROWS = 80, 24
TIOCSCTTY = getattr(termios, "TIOCSCTTY", 0x20007461)

ENTER_ALT = "\x1b[?1049h"
LEAVE_ALT = "\x1b[?1049l"
NO_WRAP = "\x1b[?7l"
WRAP = "\x1b[?7h"
HIDE_CUR = "\x1b[?25l"
SHOW_CUR = "\x1b[?25h"

# DECSCUSR, `CSI Ps SP q`: the escape family CursorStyle.trp is parked on. Any
# occurrence of it is a regression against the owner's decision.
DECSCUSR = re.compile(r"\x1b\[[0-9;]* q")

BT = "../../bt.trp"

failures = []


def check(name, ok, detail=""):
    print("  %-52s %s%s" % (name, "PASS" if ok else "FAIL",
                            ("  " + detail) if detail else ""))
    if not ok:
        failures.append(name)


def make_file(name, text):
    os.makedirs(WORK, exist_ok=True)
    path = os.path.join(WORK, name)
    with open(path, "wb") as f:
        f.write(text)
    return path


def read_file(name):
    with open(os.path.join(WORK, name), "rb") as f:
        return f.read()


def compile_prog(src):
    """Compile `src` and return the emitted JS.

    The source is named relative to the repository root, and the compiler is
    run from there: a program-relative module's pin key is the path the import
    resolves to, and compiling the same program by absolute path makes those
    keys absolute, which matches nothing in the .deps.json file."""
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


class Run:
    """An editor session under a pty, driven step by step."""

    def __init__(self, prog, filename, rows=ROWS, cols=COLS):
        js = compile_prog(os.path.normpath(os.path.join(HERE, prog)))
        self.master, self.slave = pty.openpty()
        set_winsize(self.master, rows, cols)
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

    def send(self, data):
        os.write(self.master, data)

    def type(self, data, settle=0.6):
        """Send bytes and let the frames they cause settle."""
        mark = len(self.text())
        self.send(data)
        self.pump(settle)
        return self.text()[mark:]

    def resize(self, rows, cols, settle=1.0):
        """Resize the pty and let the redraw it causes settle."""
        mark = len(self.text())
        set_winsize(self.master, rows, cols)
        os.kill(self.pid, signal.SIGWINCH)
        self.pump(settle)
        return self.text()[mark:]

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


CSI_H = re.compile(r"\x1b\[(\d+);(\d+)H")
CSI_ANY = re.compile(r"\x1b\[[0-9;?]*[A-Za-z]")


def frames(text):
    """The frames in `text`.

    A frame is exactly what Screen.frame writes: it opens with the cursor hide
    and closes with the cursor show. Splitting on the hide and keeping what
    precedes the matching show gives one entry per frame and drops the session
    prologue, which contains no hide of its own."""
    out = []
    for piece in text.split(HIDE_CUR)[1:]:
        if SHOW_CUR in piece:
            out.append(HIDE_CUR + piece.split(SHOW_CUR)[0] + SHOW_CUR)
    return out


def last_frame(text):
    fs = frames(text)
    return fs[-1] if fs else ""


def status_row(text):
    rows = [CSI_ANY.sub("", row).rstrip("\r")
            for row in last_frame(text).split("\r\n")]
    return rows[-1] if rows else ""


def last_cursor(text):
    found = CSI_H.findall(text)
    return (int(found[-1][0]), int(found[-1][1])) if found else None


def cursor_at_rest(text):
    """Whether the cursor is on the screen once the output has gone quiet: the
    last visibility escape written is a show."""
    return text.rfind(SHOW_CUR) > text.rfind(HIDE_CUR)


def open_editor(name, content):
    make_file(name, content)
    r = Run(BT, name)
    r.until(lambda t: ENTER_ALT in t and len(frames(t)) >= 1)
    r.pump(0.5)
    return r


ORIGINAL = b"alpha\nbeta\n"


# ---------------------------------------------------------------- case 1


def case1():
    print("case 1: a frame hides the cursor, draws, and shows it again")
    r = open_editor("cursor.txt", ORIGINAL)
    try:
        text = r.text()
        fs = frames(text)
        check("a frame was drawn", len(fs) >= 1, "frames=%d" % len(fs))
        check("the frame opens with the cursor hide",
              fs[0].startswith(HIDE_CUR), repr(fs[0][:12]))
        check("the frame closes with the cursor show",
              fs[0].endswith(SHOW_CUR), repr(fs[0][-12:]))
        check("the show is the last thing the frame writes: nothing follows it",
              fs[0].count(SHOW_CUR) == 1 and fs[0].count(HIDE_CUR) == 1,
              "hide=%d show=%d" % (fs[0].count(HIDE_CUR),
                                   fs[0].count(SHOW_CUR)))
        check("the cursor is positioned inside the frame, before the show",
              CSI_H.search(fs[0]) is not None)
        # The session's own startup writes the alternate-screen switch and
        # nothing about the cursor, so the first hide belongs to a frame.
        prologue = text.split(HIDE_CUR)[0]
        check("the session enters the alternate screen before any frame",
              ENTER_ALT in prologue, repr(prologue[-12:]))
        # The frame writes no wrap escape of its own: the automatic wrap the
        # session clears is terminal setup, and it belongs to the prologue.
        check("the session clears the automatic wrap before any frame",
              NO_WRAP in prologue, repr(prologue[-12:]))
        check("no frame writes a wrap escape",
              not any(NO_WRAP in f or WRAP in f for f in fs))
        check("the session hides the cursor for no longer than a frame",
              SHOW_CUR not in prologue and prologue.count(HIDE_CUR) == 0)
        check("the cursor is on the screen at rest", cursor_at_rest(text))
        check("no DECSCUSR shape is selected",
              DECSCUSR.search(text) is None,
              repr(DECSCUSR.search(text).group(0)) if DECSCUSR.search(text)
              else "")
        check("the cursor rests at the first cell", last_cursor(text) == (1, 1),
              str(last_cursor(text)))

        after = r.type(b"jl")
        check("a motion draws a frame with the same discipline",
              all(f.startswith(HIDE_CUR) and f.endswith(SHOW_CUR)
                  for f in frames(after)) and len(frames(after)) >= 1,
              "frames=%d" % len(frames(after)))
        check("the cursor is on the screen after a motion",
              cursor_at_rest(r.text()))
        check("the cursor moved with the motion",
              last_cursor(r.text()) == (2, 2), str(last_cursor(r.text())))
        check("still no DECSCUSR after a motion",
              DECSCUSR.search(r.text()) is None)
    finally:
        r.close()


# ---------------------------------------------------------------- case 2


def case2():
    print("case 2: the cursor is shown and placed in every mode")
    r = open_editor("cursor2.txt", ORIGINAL)
    try:
        r.type(b"i")
        check("insert mode leaves the cursor on the screen",
              cursor_at_rest(r.text()))
        r.type(b"zz", settle=0.9)
        check("the cursor follows what is typed",
              last_cursor(r.text()) == (1, 3), str(last_cursor(r.text())))
        check("typing selects no shape", DECSCUSR.search(r.text()) is None)

        # Esc, to the end of the line, and back into insert mode: the cursor
        # sits one past the last character, which is a position the editor
        # allows in every mode (Api.trp's header).
        r.type(b"\x1b$", settle=0.9)
        check("the cursor is on the screen past the end of a line",
              cursor_at_rest(r.text()))
        check("the cursor is one past the line's characters",
              last_cursor(r.text()) == (1, 8), str(last_cursor(r.text())))

        after = r.type(b":se", settle=0.9)
        check("command mode leaves the cursor on the screen",
              cursor_at_rest(r.text()))
        check("the command line is on the status row",
              status_row(after).startswith(":"), repr(status_row(after)))
        # The status row reads ":se" — the colon plus the two characters of the
        # command, which is what the state stores — so the next character typed
        # lands in the fourth column.
        check("the cursor sits where the next command character will go",
              last_cursor(r.text()) == (ROWS, 4),
              str(last_cursor(r.text())))
        check("command mode selects no shape",
              DECSCUSR.search(r.text()) is None)
    finally:
        r.close()


# ---------------------------------------------------------------- case 3


def case3():
    print("case 3: teardown leaves the cursor shown and no shape behind")
    r = open_editor("cursor3.txt", ORIGINAL)
    try:
        r.type(b":q!\r", settle=1.0)
        ok = r.wait_exit()
        text = r.text()
        check("the session exited", ok)
        check("the cursor is shown after the last frame's hide",
              cursor_at_rest(text))
        check("the show comes before the alternate screen is left",
              text.rfind(SHOW_CUR) < text.rfind(LEAVE_ALT))
        check("no shape is written on the way out",
              DECSCUSR.search(text) is None)
        check("the automatic wrap is set again, before the screen is left",
              WRAP in text and text.rfind(WRAP) < text.rfind(LEAVE_ALT))
        check("alternate screen left", LEAVE_ALT in text)
        check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()


# ---------------------------------------------------------------- case 4


def frame_rows(text):
    """The rows of the last full frame, with the escapes stripped."""
    return [CSI_ANY.sub("", row).rstrip("\r")
            for row in last_frame(text).split("\r\n")]


HELP_TEXT = b"alpha\nbeta\ngamma\n"


def case4():
    print("case 4: :help draws a page over the buffer and any key gives it back")
    r = open_editor("help.txt", HELP_TEXT)
    try:
        before = frame_rows(r.text())
        check("the buffer is on the screen to begin with",
              before[0] == "alpha" and before[1] == "beta", repr(before[:3]))

        after = r.type(b":help\r", settle=1.2)
        rows = frame_rows(after)
        check("the page names itself on its first row",
              rows[0] == "battallion: keys and commands", repr(rows[0]))
        check("the buffer is not on the screen while the page is up",
              "alpha" not in rows and "beta" not in rows, repr(rows[:4]))

        # The sections come from the plugins that bind the keys: the keymap's
        # motions and edits, and the command interpreter's words.
        page = "\n".join(rows)
        for want in ("MOTION", "EDIT", "SEARCH", "COMMANDS",
                     "h j k l", "dd", "/text", ":w PATH", ":help"):
            check("the page lists %r" % want, want in page)
        check("the page fills the rows above the status line",
              len(rows) == ROWS, "%d rows" % len(rows))
        check("the status row says how to put the page away",
              rows[-1] == "press any key to return", repr(rows[-1]))
        check("the cursor is parked at the page's first cell",
              last_cursor(after) == (1, 1), str(last_cursor(after)))
        check("the page is drawn with the same cursor discipline",
              all(f.startswith(HIDE_CUR) and f.endswith(SHOW_CUR)
                  for f in frames(after)))
        check("no shape is selected for the page",
              DECSCUSR.search(after) is None)

        # The page is a frame and not a change of state: the key that dismisses
        # it is spent on dismissing it, and the buffer comes back as it was.
        after = r.type(b"z", settle=1.0)
        rows = frame_rows(after)
        check("any key puts the buffer back",
              rows[0] == "alpha" and rows[1] == "beta", repr(rows[:3]))
        check("the cursor is back where it was",
              last_cursor(after) == (1, 1), str(last_cursor(after)))
        check("the key that dismissed the page did not reach the buffer",
              "[+]" not in status_row(after), repr(status_row(after)))
        check("the file is untouched", read_file("help.txt") == HELP_TEXT)

        # :h is the same command, and the page follows the terminal's size.
        r.type(b":h\r", settle=1.2)
        after = r.resize(12, 50)
        rows = frame_rows(after)
        check(":h opens the same page", rows[0].startswith("battallion:"),
              repr(rows[0]))
        check("the page is redrawn at the new size", len(rows) == 12,
              "%d rows" % len(rows))
        check("a page that does not fit says how much is missing",
              "more columns not shown" in "\n".join(rows), repr(rows[-3:]))

        r.type(b"\x1b", settle=1.0)
        r.send(b":q\r")
        ok = r.wait_exit()
        check("the session exited", ok)
        check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()


for c in (case1, case2, case3, case4):
    c()
    print("")

if failures:
    print("FAILURES: " + ", ".join(failures))
    sys.exit(1)
print("ALL PASS")
