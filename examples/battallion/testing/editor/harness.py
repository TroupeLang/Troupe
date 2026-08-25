#!/usr/bin/env python3
"""Pty checks for battallion's editing and saving (mvp stage 5).

The stage-4 harness (examples/battallion/testing/viewer/harness.py)
checks that the editor opens, navigates, resizes and gives the terminal back;
its pty machinery is reproduced here rather than imported, that module running
its own cases at import time. What this one checks is what stage 5 added: the
edits, the undo history, the command line, the file `:w` writes, and the
supervision that replaced the stage-4 watchdog.

WHAT MAKES AN EDIT CHECKABLE. Every case that edits ends by writing the buffer
out and comparing the file against the expected bytes, read back in binary:
a screen assertion would only say what the renderer drew, and the point of an
editor is what ends up in the file. The multibyte cases are the reason the
comparison is byte-exact rather than by decoded string — a character that
survived as the wrong encoding would compare equal after decoding.

WHAT IS ASSERTED ABOUT THE TERMINAL, AND WHY IT IS ASSERTED THAT WAY.
The pty's termios is read while the child is alive, never after it has died: on
darwin the pty is revoked when the process that owns it exits, tcgetattr on the
slave then fails and the master reports the reset default, so a post-mortem
reading witnesses the pty tearing itself down rather than anything the program
did. What makes the restore observable while the process is still alive is the
teardown order the editor uses: cooked mode is restored *before* the last
escapes are written, so the alternate-screen-leave sequence appearing in the
output is proof that termios was already back.

Usage:
  python3 examples/battallion/testing/editor/harness.py "$PWD"

Observed 2026-08-02 (dev-text-editor, display-width truncation in the tree),
187 checks, all PASS:
  case 1  insert flow           i, multibyte text, Esc, :w, :q; file byte-exact
  case 2  x / o / Enter / BS    scripted edits incl. a line join; file byte-exact
  case 3  undo                  every edit undone, file back to the original
  case 4  dirty :q              refused with a message; :q! leaves the file alone
  case 5  kernel crash          DONE restores the terminal well inside the watchdog
  case 6  lone Esc              resolved by the timer, and by the key that follows
  case 7  refused write         reported in the status line; the session survives
  case 8  undo past a write     still modified, :q refused, :w then :q, byte-exact
  case 9  search                /pattern, n, N, both wrap notes, a miss, the empty repeat
  case 10 linewise edits        yy, p, P and dd, undone; the file byte-exact
  case 11 save as               :w PATH writes a copy and leaves the buffer modified
  case 12 goto                  :N, :$, :0, a number past the end, and a word that is not one
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
WORK = "/tmp/battallion-editor"
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
CSI_ANY = re.compile(r"\x1b\[[0-9;?]*[A-Za-z]")

# Session.trp's constants, which several assertions are stated against.
WATCHDOG_MS = 2000
ESCAPE_MS = 50

# The editor and the crash probe, relative to this directory.
BT = "../../bt.trp"
CRASH = "../../crashprobe.trp"

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


def is_raw(attrs):
    """Raw mode as the runtime sets it: canonical input off."""
    return not (attrs[3] & termios.ICANON)


class Run:
    """An editor session under a pty, driven step by step."""

    def __init__(self, prog, filename, rows=ROWS, cols=COLS):
        js = compile_prog(os.path.normpath(os.path.join(HERE, prog)))
        self.master, self.slave = pty.openpty()
        set_winsize(self.master, rows, cols)
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
        self.pump(seconds)
        return self.text()

    def send(self, data):
        os.write(self.master, data)

    def type(self, data, settle=0.6):
        """Send bytes and let the frames they cause settle."""
        mark = len(self.text())
        self.send(data)
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


def last_cursor(text):
    found = CSI_H.findall(text)
    return (int(found[-1][0]), int(found[-1][1])) if found else None


def frames(text):
    """How many full-frame redraws the renderer wrote."""
    return text.count("\x1b[H")


def frame_rows(text):
    """The rows of the last full frame, with the escapes stripped.

    The trailing CR is stripped because raw mode leaves output post-processing
    on: the LF of the CRLF the renderer writes is expanded to CRLF again, so
    each row arrives ending in CR CR LF."""
    last = text.split("\x1b[H")[-1]
    return [CSI_ANY.sub("", row).rstrip("\r") for row in last.split("\r\n")]


def status_row(text):
    """The bottom row of the last full frame: battallion's status line."""
    rows = frame_rows(text)
    return rows[-1] if rows else ""


def body_rows(text):
    """Everything above the status line in the last full frame."""
    return frame_rows(text)[:-1]


def open_editor(name, content):
    """A file with known contents, and a session on it that has drawn once."""
    make_file(name, content)
    r = Run(BT, name)
    r.until(lambda t: ENTER_ALT in t and frames(t) >= 1)
    r.quiet(0.5)
    check("automatic wrap cleared before the first frame",
          NO_WRAP in r.text().split(HIDE_CUR)[0])
    return r


def quit_and_check(r, expect_code=0):
    ok = r.wait_exit()
    check("the session exited", ok)
    check("alternate screen left", LEAVE_ALT in r.text())
    # The session runs with the terminal's automatic wrap cleared so that a row
    # wider than the terminal is clipped rather than continued onto the next
    # physical row (Screen.trp's header). The teardown sets it again, before
    # the screen is switched back.
    check("automatic wrap restored before the screen was left",
          WRAP in r.text() and r.text().rfind(WRAP) < r.text().rfind(LEAVE_ALT))
    check("cursor left shown",
          r.text().rfind(SHOW_CUR) > r.text().rfind(HIDE_CUR))
    check("exit code %d" % expect_code, r.exit_code() == expect_code,
          "rc=%s" % r.exit_code())


# ---------------------------------------------------------------- case 1

ORIGINAL = b"alpha\nbeta\n"
TYPED = "Hello, wörld! ✓"


def case1():
    print("case 1: insert mode types text, :w writes it, and the file matches")
    r = open_editor("insert.txt", ORIGINAL)
    try:
        first = r.text()
        check("a status line names the file", "insert.txt" in status_row(first),
              repr(status_row(first)))
        check("the status line starts in normal mode", "NORMAL" in status_row(first))
        check("the buffer starts clean", "[+]" not in status_row(first))
        check("the text rows are one fewer than the terminal's",
              len(body_rows(first)) == ROWS - 1, "%d rows" % len(body_rows(first)))

        after = r.type(b"i")
        check("i enters insert mode", "INSERT" in status_row(after),
              repr(status_row(after)))

        after = r.type(TYPED.encode("utf8"), settle=1.0)
        check("the typed text is on the screen", TYPED in after)
        check("the buffer is marked modified", "[+]" in status_row(after),
              repr(status_row(after)))
        check("the cursor column counts the typed characters",
              ("1,%d" % (len(TYPED) + 1)) in status_row(after),
              repr(status_row(after)))

        after = r.type(b"\x1b:w\r", settle=1.5)
        check("the write is confirmed in the status line",
              "written" in status_row(after), repr(status_row(after)))
        check("the buffer is clean again", "[+]" not in status_row(after))

        saved = read_file("insert.txt")
        expected = TYPED.encode("utf8") + ORIGINAL
        check("the saved file matches byte for byte", saved == expected,
              "%r vs %r" % (saved[:40], expected[:40]))

        r.send(b":q\r")
        quit_and_check(r)
    finally:
        r.close()


# ---------------------------------------------------------------- case 2

EDIT_ORIGINAL = b"one two\nthree\n"
EDIT_EXPECTED = b"ne two\nmid\nthree\n"


def case2():
    print("case 2: x, o, Enter and Backspace, including a join across lines")
    r = open_editor("edit.txt", EDIT_ORIGINAL)
    try:
        after = r.type(b"x")
        check("x deleted the character under the cursor",
              body_rows(after)[0] == "ne two", repr(body_rows(after)[0]))

        after = r.type(b"o", settle=0.8)
        check("o opened a line below and entered insert mode",
              "INSERT" in status_row(after) and body_rows(after)[1] == "",
              repr(status_row(after)))
        check("the cursor moved to the new line", "2,1" in status_row(after),
              repr(status_row(after)))

        after = r.type(b"mid\rX", settle=1.0)
        rows = body_rows(after)
        check("Enter split the line", rows[1] == "mid" and rows[2] == "X",
              repr(rows[:4]))

        after = r.type(b"\x7f", settle=1.0)
        rows = body_rows(after)
        check("the first Backspace removed the character, leaving the line empty",
              rows[1] == "mid" and rows[2] == "" and rows[3] == "three",
              repr(rows[:4]))

        after = r.type(b"\x7f", settle=1.0)
        rows = body_rows(after)
        check("the second Backspace joined the empty line to the one above",
              rows[1] == "mid" and rows[2] == "three", repr(rows[:4]))
        check("the cursor sits at the end of the joined line",
              "2,4" in status_row(after), repr(status_row(after)))

        r.send(b"\x1b:wq\r")
        quit_and_check(r)
        saved = read_file("edit.txt")
        check("the saved file matches byte for byte", saved == EDIT_EXPECTED,
              "%r vs %r" % (saved, EDIT_EXPECTED))
    finally:
        r.close()


# ---------------------------------------------------------------- case 3


def case3():
    print("case 3: undo walks every change back, and stops harmlessly at the oldest")
    r = open_editor("undo.txt", ORIGINAL)
    try:
        after = r.type(b"xxx", settle=1.0)
        check("three deletions took effect", body_rows(after)[0] == "ha",
              repr(body_rows(after)[0]))

        after = r.type(b"ozz", settle=1.0)
        rows = body_rows(after)
        check("o and two characters made a new line", rows[1] == "zz",
              repr(rows[:3]))
        check("the buffer is modified", "[+]" in status_row(after))

        # Six edits went in (three deletions, the opened line, two characters);
        # eight undos are two more than there is history for.
        after = r.type(b"\x1buuuuuuuu", settle=2.0)
        rows = body_rows(after)
        check("the text is back to what was opened",
              rows[0] == "alpha" and rows[1] == "beta", repr(rows[:3]))
        check("undoing past the oldest change is a message, not an error",
              "already at the oldest change" in status_row(after),
              repr(status_row(after)))
        check("the buffer is clean again after undoing every change",
              "[+]" not in status_row(after), repr(status_row(after)))

        after = r.type(b":w\r", settle=1.5)
        check("the write is confirmed", "written" in status_row(after),
              repr(status_row(after)))
        saved = read_file("undo.txt")
        check("the saved file is the original byte for byte", saved == ORIGINAL,
              "%r vs %r" % (saved, ORIGINAL))

        # The keys behind a command's Enter are folded after the write, not
        # before it: the fold stops at the request and resumes on the answer.
        # One chunk draws one frame, so the confirmation this write puts in the
        # status line is overwritten by the motions before anything is drawn —
        # what the file holds is the evidence that the write happened where the
        # command sat rather than after the motions.
        after = r.type(b"x:w\rjj", settle=2.0)
        check("the keys behind the command took effect after the write",
              "3,1" in status_row(after), repr(status_row(after)))
        saved = read_file("undo.txt")
        check("the write in the same chunk used the state at the command",
              saved == b"lpha\nbeta\n", repr(saved))

        r.send(b":q\r")
        quit_and_check(r)
    finally:
        r.close()


# ---------------------------------------------------------------- case 4


def case4():
    print("case 4: :q refuses a modified buffer and :q! overrides it")
    r = open_editor("dirty.txt", ORIGINAL)
    try:
        after = r.type(b"x")
        check("the buffer is modified", "[+]" in status_row(after))

        after = r.type(b":q\r", settle=1.2)
        check("the refusal names the reason",
              "no write since last change" in status_row(after),
              repr(status_row(after)))
        check("the session is still running", not r.exited)
        check("the buffer is still modified", "[+]" in status_row(after))
        check("the file on disk is untouched", read_file("dirty.txt") == ORIGINAL)

        r.send(b":q!\r")
        quit_and_check(r)
        check("the file on disk is still untouched",
              read_file("dirty.txt") == ORIGINAL)
    finally:
        r.close()


# ---------------------------------------------------------------- case 5


def case5():
    print("case 5: a kernel that dies is found by the monitor, not by the watchdog")
    make_file("crash.txt", ORIGINAL)
    r = Run(CRASH, "crash.txt")
    try:
        r.until(lambda t: ENTER_ALT in t and frames(t) >= 1)
        r.pump(0.3)
        check("raw mode entered", r.saw_raw)
        started = time.time()
        r.send(b"!")
        ok = r.wait_exit(timeout=30)
        elapsed = time.time() - started
        text = r.text()
        check("the session exited", ok)
        check("the kernel really died",
              "Runtime error in thread" in text or "runtime error" in text.lower())
        check("alternate screen left", LEAVE_ALT in text)
        check("cursor left shown", text.rfind(SHOW_CUR) > text.rfind(HIDE_CUR))
        check("exit code is exitKernelLost (3)", r.exit_code() == 3,
              "rc=%s" % r.exit_code())
        check("the restore happened well inside the watchdog interval",
              elapsed < WATCHDOG_MS / 2000.0,
              "%.2fs against a %.1fs watchdog" % (elapsed, WATCHDOG_MS / 1000.0))
    finally:
        r.close()


# ---------------------------------------------------------------- case 6


def case6():
    print("case 6: a lone Esc is resolved by the timer, and by the key behind it")
    r = open_editor("esc.txt", ORIGINAL)
    try:
        r.type(b"iab", settle=0.8)
        after = r.type(b"\x1b", settle=ESCAPE_MS / 1000.0 + 0.6)
        check("the lone Esc left insert mode without another key",
              "NORMAL" in status_row(after), repr(status_row(after)))
        check("the cursor kept its column", "1,3" in status_row(after),
              repr(status_row(after)))

        # Esc and the key behind it, arriving inside the timer's window: the
        # decoder carries the Esc into the next chunk and both keys take effect.
        r.type(b"i", settle=0.6)
        mark = len(r.text())
        r.send(b"\x1b")
        time.sleep(0.01)
        r.send(b"h")
        r.pump(0.9)
        after = r.text()[mark:]
        check("Esc inside the window still left insert mode",
              "NORMAL" in status_row(after), repr(status_row(after)))
        check("the key behind the Esc moved the cursor",
              "1,2" in status_row(after), repr(status_row(after)))
        check("nothing of the escape byte reached the buffer",
              body_rows(after)[0] == "abalpha", repr(body_rows(after)[0]))

        # An escape sequence that starts and never finishes. Each chunk draws a
        # frame; the third frame is the timer flushing the stuck carry, and it
        # is the whole assertion — a kernel that re-armed on every superseded
        # timer would supersede the live one each time and never flush at all.
        mark = len(r.text())
        r.send(b"\x1b")
        time.sleep(0.01)
        r.send(b"[")
        r.pump(0.8)
        after = r.text()[mark:]
        check("an unfinished escape sequence is flushed by the timer",
              frames(after) >= 3, "%d frames" % frames(after))
        check("the editor is still in normal mode after the flush",
              "NORMAL" in status_row(after), repr(status_row(after)))

        after = r.type(b"l", settle=0.8)
        check("the next key is acted on normally", "1,3" in status_row(after),
              repr(status_row(after)))

        r.send(b":q!\r")
        quit_and_check(r)
    finally:
        r.close()


# ---------------------------------------------------------------- case 7


def case7():
    print("case 7: a write the filesystem refuses is a status message, not a crash")
    path = make_file("readonly.txt", ORIGINAL)
    os.chmod(path, 0o444)
    r = Run(BT, "readonly.txt")
    try:
        r.until(lambda t: ENTER_ALT in t and frames(t) >= 1)
        r.quiet(0.5)
        after = r.type(b"x", settle=0.8)
        check("the buffer is modified", "[+]" in status_row(after))

        after = r.type(b":w\r", settle=1.5)
        check("the refusal is reported in the status line",
              "denied" in status_row(after) or "EACCES" in status_row(after),
              repr(status_row(after)))
        check("the session is still running", not r.exited)
        check("the buffer is still modified", "[+]" in status_row(after),
              repr(status_row(after)))
        check("the file on disk is untouched",
              read_file("readonly.txt") == ORIGINAL)

        r.send(b":q!\r")
        quit_and_check(r)
    finally:
        r.close()
        os.chmod(path, 0o644)


# ---------------------------------------------------------------- case 8


def case8():
    print("case 8: undoing past a write leaves the buffer modified")
    r = open_editor("savedundo.txt", ORIGINAL)
    try:
        after = r.type(b"x", settle=0.8)
        check("the deletion took effect", body_rows(after)[0] == "lpha",
              repr(body_rows(after)[0]))
        check("the buffer is modified", "[+]" in status_row(after))

        after = r.type(b":w\r", settle=1.5)
        check("the write is confirmed", "written" in status_row(after),
              repr(status_row(after)))
        check("the buffer is clean once written", "[+]" not in status_row(after),
              repr(status_row(after)))
        check("the file holds the edited text",
              read_file("savedundo.txt") == b"lpha\nbeta\n",
              repr(read_file("savedundo.txt")))

        # The undo puts the buffer back to what was *opened*, which is no longer
        # what the file holds. A modified flag carried in the undo history says
        # clean here — the state was clean when the snapshot was taken — and the
        # editor would then quit on :q without a word, losing the write. The
        # revision comparison (Api.trp's header) is what gets this right.
        after = r.type(b"u", settle=1.0)
        check("the undo put the opened text back",
              body_rows(after)[0] == "alpha", repr(body_rows(after)[0]))
        check("the buffer is modified again: it differs from the file",
              "[+]" in status_row(after), repr(status_row(after)))

        after = r.type(b":q\r", settle=1.2)
        check("the quit is refused", "no write since last change" in
              status_row(after), repr(status_row(after)))
        check("the session is still running", not r.exited)
        check("the file still holds the written text",
              read_file("savedundo.txt") == b"lpha\nbeta\n",
              repr(read_file("savedundo.txt")))

        after = r.type(b":w\r", settle=1.5)
        check("the second write is confirmed", "written" in status_row(after),
              repr(status_row(after)))
        check("the file is the original byte for byte",
              read_file("savedundo.txt") == ORIGINAL,
              repr(read_file("savedundo.txt")))
        check("the buffer is clean once the file matches it",
              "[+]" not in status_row(after), repr(status_row(after)))

        r.send(b":q\r")
        quit_and_check(r)
    finally:
        r.close()


# ---------------------------------------------------------------- case 9

SEARCH_TEXT = b"alpha one\nbeta gamma\nalpha two\n"


def case9():
    print("case 9: search finds, repeats, wraps both ways, and says when it misses")
    r = open_editor("search.txt", SEARCH_TEXT)
    try:
        after = r.type(b"/gamma", settle=0.8)
        check("the search prompt is the status row",
              status_row(after) == "/gamma", repr(status_row(after)))

        after = r.type(b"\r", settle=1.0)
        check("the pattern is found on the line it is on",
              "2,6" in status_row(after), repr(status_row(after)))
        check("a hit that needed no wrap says nothing",
              "search hit" not in status_row(after), repr(status_row(after)))

        # The only occurrence: repeating runs off the end, comes back at the
        # top, and finds the same one again.
        after = r.type(b"n", settle=1.0)
        check("repeating past the only hit wraps to the top",
              "search hit the bottom" in status_row(after),
              repr(status_row(after)))
        check("and lands on the same hit", "2,6" in status_row(after),
              repr(status_row(after)))

        after = r.type(b"gg/alpha\r", settle=1.2)
        check("a search from the first line finds the hit below it",
              "3,1" in status_row(after), repr(status_row(after)))

        after = r.type(b"n", settle=1.0)
        check("n from the last hit wraps to the first",
              "1,1" in status_row(after) and "search hit the bottom" in status_row(after),
              repr(status_row(after)))

        after = r.type(b"N", settle=1.0)
        check("N from the first hit wraps to the last",
              "3,1" in status_row(after) and "search hit the top" in status_row(after),
              repr(status_row(after)))

        after = r.type(b"N", settle=1.0)
        check("N again walks back without wrapping",
              "1,1" in status_row(after) and "search hit" not in status_row(after),
              repr(status_row(after)))

        # An empty pattern repeats the last one that was typed.
        after = r.type(b"/\r", settle=1.0)
        check("an empty pattern repeats the last one",
              "3,1" in status_row(after), repr(status_row(after)))

        # A miss leaves the cursor where it was and says so.
        after = r.type(b"/zzz\r", settle=1.0)
        check("a miss is reported", "pattern not found: zzz" in status_row(after),
              repr(status_row(after)))
        check("a miss leaves the cursor where it was",
              "3,1" in status_row(after), repr(status_row(after)))

        # A pattern is remembered whether or not it was found, which is what vi
        # does with it: the empty repeat runs the miss again rather than the
        # last pattern that worked.
        after = r.type(b"/\r", settle=1.0)
        check("a pattern that missed is still the one that is remembered",
              "pattern not found: zzz" in status_row(after),
              repr(status_row(after)))

        # Case is not folded.
        after = r.type(b"/ALPHA\r", settle=1.0)
        check("the search is case-sensitive",
              "pattern not found: ALPHA" in status_row(after),
              repr(status_row(after)))

        r.send(b":q\r")
        quit_and_check(r)
        check("nothing was written", read_file("search.txt") == SEARCH_TEXT)
    finally:
        r.close()


# ---------------------------------------------------------------- case 10

LINES_TEXT = b"one\ntwo\nthree\n"


def case10():
    print("case 10: yy, p, P and dd move whole lines, and undo puts them back")
    r = open_editor("lines.txt", LINES_TEXT)
    try:
        after = r.type(b"y", settle=0.7)
        check("the prefix is shown while the pair is half pressed",
              "(y)" in status_row(after), repr(status_row(after)))

        after = r.type(b"y", settle=0.8)
        check("yy says the line was taken", "line yanked" in status_row(after),
              repr(status_row(after)))
        check("yanking changes nothing", "[+]" not in status_row(after),
              repr(status_row(after)))

        after = r.type(b"jjp", settle=1.0)
        rows = body_rows(after)
        check("p put the line below the cursor's",
              rows[0] == "one" and rows[1] == "two" and rows[2] == "three"
              and rows[3] == "one", repr(rows[:5]))
        check("the cursor is on the line that was put",
              "4,1" in status_row(after), repr(status_row(after)))

        after = r.type(b"dd", settle=1.0)
        rows = body_rows(after)
        check("dd took the line away again",
              rows[0] == "one" and rows[1] == "two" and rows[2] == "three"
              and rows[3] == "", repr(rows[:5]))

        after = r.type(b"ggP", settle=1.0)
        rows = body_rows(after)
        check("P put the deleted line above the first",
              rows[0] == "one" and rows[1] == "one" and rows[2] == "two",
              repr(rows[:4]))

        after = r.type(b":w\r", settle=1.5)
        check("the write is confirmed", "written" in status_row(after),
              repr(status_row(after)))
        check("the file holds the line that was put",
              read_file("lines.txt") == b"one\none\ntwo\nthree\n",
              repr(read_file("lines.txt")))

        # Three edits went in: the put, the delete, and the second put. Undoing
        # them puts the buffer back to what was opened, and the yank register is
        # not part of that — `p` after the undos puts the same line again.
        after = r.type(b"uuu", settle=1.5)
        rows = body_rows(after)
        check("the undos put the buffer back",
              rows[0] == "one" and rows[1] == "two" and rows[2] == "three"
              and rows[3] == "", repr(rows[:5]))
        check("the buffer differs from the file that was written",
              "[+]" in status_row(after), repr(status_row(after)))

        after = r.type(b"ggp", settle=1.2)
        rows = body_rows(after)
        check("the yank register survived the undos",
              rows[0] == "one" and rows[1] == "one" and rows[2] == "two",
              repr(rows[:4]))

        r.send(b":q!\r")
        quit_and_check(r)
        check("the file is what the write left",
              read_file("lines.txt") == b"one\none\ntwo\nthree\n",
              repr(read_file("lines.txt")))
    finally:
        r.close()


# ---------------------------------------------------------------- case 11


def case11():
    print("case 11: :w PATH writes a copy without adopting the path")
    make_file("copy.txt", b"stale\n")
    r = open_editor("saveas.txt", ORIGINAL)
    try:
        after = r.type(b"x", settle=0.8)
        check("the buffer is modified", "[+]" in status_row(after))

        after = r.type(b":w copy.txt\r", settle=1.8)
        check("the status line names the file that was written",
              "copy.txt written" in status_row(after), repr(status_row(after)))
        check("the copy holds the buffer byte for byte",
              read_file("copy.txt") == b"lpha\nbeta\n",
              repr(read_file("copy.txt")))
        check("the file the buffer came from is untouched",
              read_file("saveas.txt") == ORIGINAL, repr(read_file("saveas.txt")))
        check("the status line still names the buffer's own file",
              status_row(after).startswith("saveas.txt"), repr(status_row(after)))
        check("the buffer is still modified: its own file is out of date",
              "[+]" in status_row(after), repr(status_row(after)))

        after = r.type(b":q\r", settle=1.2)
        check("and :q still refuses it",
              "no write since last change" in status_row(after),
              repr(status_row(after)))
        check("the session is still running", not r.exited)

        # A bare :w goes to the path the buffer was opened with, which is what
        # makes the copy a copy rather than a change of file.
        after = r.type(b":w\r", settle=1.8)
        check("a bare :w writes the buffer's own file",
              "saveas.txt written" in status_row(after), repr(status_row(after)))
        check("the buffer is clean once its own file matches",
              "[+]" not in status_row(after), repr(status_row(after)))
        check("that file now holds the edit",
              read_file("saveas.txt") == b"lpha\nbeta\n",
              repr(read_file("saveas.txt")))

        r.send(b":q\r")
        quit_and_check(r)
    finally:
        r.close()

    # :wq PATH writes the copy and ends the session, the quit being conditional
    # on the write and not on which file it went to.
    r = open_editor("saveas2.txt", ORIGINAL)
    try:
        r.type(b"x", settle=0.8)
        r.send(b":wq copy2.txt\r")
        quit_and_check(r)
        check("the copy holds the edited buffer",
              read_file("copy2.txt") == b"lpha\nbeta\n",
              repr(read_file("copy2.txt")))
        check("the file the buffer came from is untouched",
              read_file("saveas2.txt") == ORIGINAL,
              repr(read_file("saveas2.txt")))
    finally:
        r.close()


# ---------------------------------------------------------------- case 12

GOTO_TEXT = b"l1\nl2\nl3\nl4\nl5\n"


def case12():
    print("case 12: a line number is a command")
    r = open_editor("goto.txt", GOTO_TEXT)
    try:
        after = r.type(b":4\r", settle=1.2)
        check(":4 goes to the fourth line", "4,1" in status_row(after),
              repr(status_row(after)))

        after = r.type(b":1\r", settle=1.2)
        check(":1 goes back to the first", "1,1" in status_row(after),
              repr(status_row(after)))

        # The buffer ends in a newline, so its last line is the empty sixth.
        after = r.type(b":$\r", settle=1.2)
        check(":$ goes to the last line", "6,1" in status_row(after),
              repr(status_row(after)))

        after = r.type(b":999\r", settle=1.2)
        check("a number past the end stops at the last line",
              "6,1" in status_row(after), repr(status_row(after)))

        after = r.type(b":0\r", settle=1.2)
        check(":0 goes to the first line", "1,1" in status_row(after),
              repr(status_row(after)))

        after = r.type(b":12x\r", settle=1.2)
        check("a word that is not all digits is not a line number",
              "not an editor command: 12x" in status_row(after),
              repr(status_row(after)))

        r.send(b":q\r")
        quit_and_check(r)
        check("nothing was written", read_file("goto.txt") == GOTO_TEXT)
    finally:
        r.close()


for c in (case1, case2, case3, case4, case5, case6, case7, case8,
          case9, case10, case11, case12):
    c()
    print("")

if failures:
    print("FAILURES: " + ", ".join(failures))
    sys.exit(1)
print("ALL PASS")
