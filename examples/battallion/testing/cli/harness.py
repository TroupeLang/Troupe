#!/usr/bin/env python3
"""Pty checks for battallion's edges: the command line, the path, the file's
contents, and the terminal's size (mvp stage 6).

The stage-4, stage-5 and stage-6 harnesses (examples/battallion/testing/
{viewer,editor,plugins}/harness.py) check what the editor does when
it is used the way it is meant to be. This one checks what it does when it is
not: no file named, several named, a path that leaves the subtree file access is
confined to, a file that is a directory, a file with no last newline, a file
with bytes that are not text, and a terminal too small to have a status line.
The pty machinery is reproduced here rather than imported, those modules running
their own cases at import time.

WHAT A STARTUP REFUSAL MUST LOOK LIKE, and why every refusal case asserts the
same three things. The editor takes the terminal away from the shell — raw mode,
the alternate screen — and a program that fails after doing so leaves the user
with a terminal that is not theirs and a message they cannot see. So a refusal
is asserted to be a line on stderr, an exit code of 1, *and* an untouched
terminal: no alternate screen in the output and raw mode never seen on the pty
while the process ran.

WHERE THE PATH RULES COME FROM. `--io-root` is the runtime's flag, not
battallion's: rt/src/builtins/simplefileio.mts resolves every path against that
subtree and refuses lexical escapes (`..`, an absolute path outside) and symlink
escapes before touching the filesystem. Case 2 pins the behaviour battallion
sees for each shape of path, which is what its startup message reports.

ON DARWIN AN ABSOLUTE PATH IS THE INTERESTING ONE. The io-root is realpath'd at
runtime start, and `/tmp` is a symlink to `/private/tmp`, so an absolute path
under `/tmp/...` naming a file that *is* inside the root is still refused — the
lexical check compares against the resolved root. The realpath'd spelling of the
same file is accepted. Both are asserted, because the pair is the whole rule —
on a system whose temp directory is not a symlink the refusal has nothing to
assert and is skipped, leaving three checks fewer.

Usage:
  python3 examples/battallion/testing/cli/harness.py "$PWD"

Observed 2026-08-02 (dev-text-editor, display-width truncation in the tree),
103 checks, all PASS:
  case 1  arguments        none, empty, several: usage on stderr, terminal untouched
  case 2  paths            subdirectory, escapes, symlink, absolute, directory, not text
  case 3  io-root unset    the message says where the path was resolved
  case 4  file contents    empty, no last newline, a last empty line, control bytes
  case 5  terminal size    two rows, one row, resize to one, resize while inserting
  case 6  :wq refused      a write that fails does not quit the session
  case 7  long line        clamped to the width, typing at the clamp, byte-exact
  case 8  :w PATH          a subdirectory, a lexical escape, a symlink out of the root
"""
import fcntl
import os
import pty
import re
import select
import shutil
import signal
import struct
import sys
import termios
import time

TROUPE = sys.argv[1]
HERE = os.path.dirname(os.path.abspath(__file__))
WORK = "/tmp/battallion-cli"
OUTSIDE = "/tmp/battallion-cli-outside"
COLS, ROWS = 80, 24
TIOCSCTTY = getattr(termios, "TIOCSCTTY", 0x20007461)

ENTER_ALT = "\x1b[?1049h"
LEAVE_ALT = "\x1b[?1049l"
NO_WRAP = "\x1b[?7l"
WRAP = "\x1b[?7h"
HIDE_CUR = "\x1b[?25l"
SHOW_CUR = "\x1b[?25h"
CSI_H = re.compile(r"\x1b\[(\d+);(\d+)H")
CSI_ANY = re.compile(r"\x1b\[[0-9;?]*[A-Za-z]")

USAGE = "usage: bt.trp --io-root <dir> -- <file>"
ROOT_NOTE = "paths resolve inside --io-root"

BT = "../../bt.trp"

failures = []


def check(name, ok, detail=""):
    print("  %-54s %s%s" % (name, "PASS" if ok else "FAIL",
                            ("  " + detail) if detail else ""))
    if not ok:
        failures.append(name)


def make_file(name, content, root=WORK):
    path = os.path.join(root, name)
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "wb") as f:
        f.write(content)
    return path


def read_file(name, root=WORK):
    with open(os.path.join(root, name), "rb") as f:
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
    """A session under a pty, with the runtime's argument list spelled out.

    `tail` is everything after `--`, so a case can pass no argument at all or
    several. `io_root` is None for a run that leaves the flag off."""

    def __init__(self, tail, io_root=WORK, rows=ROWS, cols=COLS):
        js = compile_prog(os.path.normpath(os.path.join(HERE, BT)))
        self.master, self.slave = pty.openpty()
        set_winsize(self.master, rows, cols)
        self.saw_raw = False
        self.out = b""
        self.exited = False
        self.status = None
        argv = ["node", os.path.join(TROUPE, "rt/built/troupe.mjs"),
                "-f=" + js, "--localonly"]
        if io_root is not None:
            argv += ["--io-root", io_root]
        argv += ["--"] + list(tail)
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
            os.execvp("node", argv)
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

    def type(self, data, settle=0.8):
        mark = len(self.text())
        self.send(data)
        self.pump(settle)
        return self.text()[mark:]

    def resize(self, rows, cols, settle=1.0):
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


def frames(text):
    return text.count("\x1b[H")


def frame_rows(text):
    """The rows of the last full frame, escapes removed."""
    last = text.split("\x1b[H")[-1]
    return [CSI_ANY.sub("", row).rstrip("\r") for row in last.split("\r\n")]


def status_row(text):
    rows = frame_rows(text)
    return rows[-1] if rows else ""


def last_cursor(text):
    found = CSI_H.findall(text)
    return (int(found[-1][0]), int(found[-1][1])) if found else None


def open_editor(tail, io_root=WORK, rows=ROWS, cols=COLS):
    r = Run(tail, io_root=io_root, rows=rows, cols=cols)
    r.until(lambda t: ENTER_ALT in t and frames(t) >= 1)
    r.quiet(0.5)
    check("the terminal's automatic wrap was cleared with the screen switch",
          NO_WRAP in r.text().split(HIDE_CUR)[0])
    return r


def refusal(label, tail, io_root=WORK, expect=()):
    """Run a session that must refuse to start, and assert it did so cleanly."""
    r = Run(tail, io_root=io_root)
    try:
        r.wait_exit(timeout=25)
        text = r.text()
        tail_line = (text.strip().splitlines() or [""])[-1]
        for fragment in expect:
            check("%s: the message says %r" % (label, fragment),
                  fragment in text, repr(tail_line[:60]))
        check("%s: the terminal was never taken" % label,
              ENTER_ALT not in text and not r.saw_raw
              and NO_WRAP not in text and WRAP not in text,
              "alt=%s raw=%s wrap=%s"
              % (ENTER_ALT in text, r.saw_raw,
                 NO_WRAP in text or WRAP in text))
        check("%s: exit code 1" % label, r.exit_code() == 1,
              "rc=%s" % r.exit_code())
        return text
    finally:
        r.close()


# ---------------------------------------------------------------- case 1


def case1():
    print("case 1: the argument list")
    refusal("no argument", [], expect=["no file to open", USAGE])
    refusal("empty argument", [""],
            expect=["the file to open is an empty path", USAGE])
    refusal("two arguments", ["a.txt", "b.txt"],
            expect=["more than one file to open", USAGE])


# ---------------------------------------------------------------- case 2


def case2():
    print("case 2: a path is resolved inside --io-root")
    # A file in a subdirectory of the root opens: the path is relative to the
    # root and may descend.
    r = open_editor(["sub/deep.txt"])
    try:
        check("a path into a subdirectory opens",
              "deep" in r.text(), repr(frame_rows(r.text())[0]))
        r.send(b":q\r")
        r.wait_exit()
        check("and quits with 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()

    # The realpath'd spelling of a file inside the root is accepted; the
    # symlinked spelling of the same file is not (see this file's header).
    inside_real = os.path.join(os.path.realpath(WORK), "plain.txt")
    r = open_editor([inside_real])
    try:
        check("an absolute path inside the root opens",
              "plain" in r.text(), repr(frame_rows(r.text())[0]))
        r.send(b":q\r")
        r.wait_exit()
    finally:
        r.close()

    refusal("relative escape", ["../battallion-cli-outside/out.txt"],
            expect=["cannot open ../battallion-cli-outside/out.txt",
                    "path escapes the io-root sandbox", ROOT_NOTE])
    refusal("absolute outside", ["/etc/passwd"],
            expect=["cannot open /etc/passwd",
                    "path escapes the io-root sandbox"])
    if os.path.realpath(WORK) != WORK:
        refusal("absolute inside by a symlinked spelling",
                [os.path.join(WORK, "plain.txt")],
                expect=["path escapes the io-root sandbox"])
    refusal("symlink out of the root", ["escape.txt"],
            expect=["cannot open escape.txt",
                    "path escapes the io-root sandbox via a symlink"])
    refusal("a directory", ["sub"],
            expect=["cannot open sub", "path is a directory"])
    refusal("a file that is not there", ["nowhere.txt"],
            expect=["cannot open nowhere.txt", "file not found"])
    # A file whose bytes are not UTF-8 is refused by the runtime's read, so it never
    # reaches the buffer. Before the read validated, the byte arrived as U+FFFD and a
    # write put the replacement on disk where the original byte had been.
    refusal("a file that is not valid UTF-8", ["binary.bin"],
            expect=["cannot open binary.bin", "file is not valid UTF-8"])
    check("the refused binary file is untouched",
          read_file("binary.bin") == b"a\xffb\n", repr(read_file("binary.bin")))


# ---------------------------------------------------------------- case 3


def case3():
    print("case 3: --io-root left off")
    # Without the flag the runtime resolves against a fresh empty scratch
    # directory it made for the invocation, so every path is a file that is not
    # there. The message has to say where it looked, or it is unreadable.
    text = refusal("no io-root", ["plain.txt"], io_root=None,
                   expect=["cannot open plain.txt", "file not found", ROOT_NOTE])
    check("no io-root: the note explains the empty directory",
          "fresh empty directory" in text,
          repr((text.strip().splitlines() or [""])[-1][:60]))


# ---------------------------------------------------------------- case 4


def case4():
    print("case 4: what is in the file")
    # A file of no bytes is one empty line, not no lines: the rope's line count
    # is one more than its newline count (lib/Rope.trp).
    r = open_editor(["empty.txt"])
    try:
        rows = frame_rows(r.text())
        check("an empty file draws one empty line and then filler",
              rows[0] == "" and rows[1] == "~", repr(rows[:3]))
        check("the cursor is at the first cell",
              last_cursor(r.text()) == (1, 1), str(last_cursor(r.text())))
        r.send(b":q\r")
        r.wait_exit()
        check("an empty file quits with 0", r.exit_code() == 0,
              "rc=%s" % r.exit_code())
    finally:
        r.close()

    # A file whose last line has no newline keeps it that way: the buffer is
    # the file's bytes and `:w` writes them back. No newline is added.
    r = open_editor(["nonl.txt"])
    try:
        after = r.type(b"iZ\x1b:w\r", settle=1.5)
        check("the write is confirmed", "written" in status_row(after),
              repr(status_row(after)))
        check("the missing last newline is not added",
              read_file("nonl.txt") == b"Zabc", repr(read_file("nonl.txt")))
        r.send(b":q\r")
        r.wait_exit()
    finally:
        r.close()

    # A file that ends in a newline has an empty last line, and the editor
    # draws it — where vi would show filler from that row on. The buffer is the
    # file's bytes, and the empty line is where the cursor goes to append.
    r = open_editor(["trail.txt"])
    try:
        rows = frame_rows(r.text())
        check("a trailing newline shows as a last empty line",
              rows[0] == "a" and rows[1] == "b" and rows[2] == ""
              and rows[3] == "~", repr(rows[:5]))
        after = r.type(b"jj", settle=0.9)
        check("the cursor reaches that empty line",
              last_cursor(after) == (3, 1), str(last_cursor(after)))
        r.send(b":q\r")
        r.wait_exit()
    finally:
        r.close()

    # A NUL is a character like any other to a utf8 read: it arrives, it draws,
    # and a write puts it back unchanged.
    r = open_editor(["nul.txt"])
    try:
        after = r.type(b":w\r", settle=1.5)
        check("a file with a NUL byte writes back byte for byte",
              read_file("nul.txt") == b"a\x00b\nc\n", repr(read_file("nul.txt")))
        check("the session survived the control byte", not r.exited)
        r.send(b":q\r")
        r.wait_exit()
        check("and quit with 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()


# ---------------------------------------------------------------- case 5


def case5():
    print("case 5: a terminal with almost no rows")
    # Two rows: one for the text and one for the status line.
    r = open_editor(["three.txt"], rows=2, cols=30)
    try:
        rows = frame_rows(r.text())
        check("two rows draw one text row and the status line",
              len(rows) == 2 and rows[0] == "one" and "three.txt" in rows[1],
              repr(rows))

        # One row: the row goes to the text and there is no status line, so the
        # status plugin is not asked for one (Api.hasStatus).
        after = r.resize(1, 30)
        rows = frame_rows(after)
        check("one row keeps the row for the text and drops the status line",
              len(rows) == 1 and rows[0] == "one", repr(rows))
        check("the cursor stays on the screen", last_cursor(after) == (1, 1),
              str(last_cursor(after)))

        after = r.type(b"jj", settle=0.9)
        check("the viewport scrolls in a terminal of one row",
              frame_rows(after)[0] == "three", repr(frame_rows(after)))

        # A command typed on a terminal with no status line is not drawn, and
        # the cursor is not sent to a row that is not there.
        after = r.type(b":q", settle=0.9)
        check("a command line has nowhere to go and the cursor stays put",
              last_cursor(after) == (1, 1), str(last_cursor(after)))
        r.send(b"\x1b")
        r.quiet(0.4)

        # Back to a usable size, then a resize while insert mode is on: the
        # mode, the cursor and the modified state all survive it.
        r.resize(6, 30)
        r.type(b"iZZ", settle=0.9)
        after = r.resize(8, 40)
        check("a resize during insert mode stays in insert mode",
              "INSERT" in status_row(after), repr(status_row(after)))
        after = r.type(b"QQ", settle=0.9)
        check("and typing continues where it left off",
              frame_rows(after)[0] == "ZZQQthree", repr(frame_rows(after)[0]))
        check("the buffer is still modified across the resize",
              "[+]" in status_row(after), repr(status_row(after)))

        r.send(b"\x1b:q!\r")
        r.wait_exit()
        check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()


# ---------------------------------------------------------------- case 6


def case6():
    print("case 6: :wq on a file that cannot be written does not quit")
    path = make_file("wqro.txt", b"ro\n")
    os.chmod(path, 0o444)
    r = open_editor(["wqro.txt"])
    try:
        after = r.type(b"iZ\x1b:wq\r", settle=2.0)
        check("the refusal is reported in the status line",
              "denied" in status_row(after) or "EACCES" in status_row(after),
              repr(status_row(after)))
        check("the session did not quit", not r.exited)
        check("the buffer is still modified", "[+]" in status_row(after),
              repr(status_row(after)))
        check("the file is untouched", read_file("wqro.txt") == b"ro\n",
              repr(read_file("wqro.txt")))
        r.send(b":q!\r")
        r.wait_exit()
        check("exit code 0 on the override", r.exit_code() == 0,
              "rc=%s" % r.exit_code())
    finally:
        r.close()
        os.chmod(path, 0o644)


# ---------------------------------------------------------------- case 7


def case7():
    print("case 7: a line longer than the terminal is wide")
    r = open_editor(["long.txt"], rows=6, cols=20)
    try:
        rows = frame_rows(r.text())
        check("the line is cut to the terminal's width",
              rows[0] == "L" * 20, "%d units" % len(rows[0]))
        after = r.type(b"$", settle=0.9)
        check("the cursor stops at the last column, there being no wrapping",
              last_cursor(after) == (1, 20), str(last_cursor(after)))
        # The status line is a row like any other and is cut to the width too,
        # so at twenty columns it ends mid-position and says nothing about the
        # buffer column. What the cursor did is the assertion above; what the
        # buffer holds is the byte comparison below.
        check("the status line is cut to the width like any other row",
              len(status_row(after)) <= 20, repr(status_row(after)))

        after = r.type(b"iAB", settle=1.2)
        check("typing at the clamp keeps the cursor at the last column",
              last_cursor(after) == (1, 20), str(last_cursor(after)))
        check("the session survived it", not r.exited)

        r.type(b"\x1b:w\r", settle=1.5)
        check("the long line is written whole",
              read_file("long.txt") == b"L" * 500 + b"AB\n",
              "%d bytes" % len(read_file("long.txt")))
        r.send(b":q\r")
        r.wait_exit()
        check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()


# ------------------------------------------------------------------ fixtures

shutil.rmtree(WORK, ignore_errors=True)
shutil.rmtree(OUTSIDE, ignore_errors=True)
os.makedirs(WORK, exist_ok=True)
os.makedirs(OUTSIDE, exist_ok=True)
make_file("plain.txt", b"plain\n")
make_file("sub/deep.txt", b"deep\n")
make_file("empty.txt", b"")
make_file("nonl.txt", b"abc")
make_file("trail.txt", b"a\nb\n")
make_file("nul.txt", b"a\x00b\nc\n")
make_file("binary.bin", b"a\xffb\n")
make_file("three.txt", b"one\ntwo\nthree\n")
make_file("long.txt", b"L" * 500 + b"\n")
make_file("out.txt", b"outside\n", root=OUTSIDE)
link = os.path.join(WORK, "escape.txt")
if os.path.islink(link) or os.path.exists(link):
    os.remove(link)
os.symlink(os.path.join(OUTSIDE, "out.txt"), link)

# ---------------------------------------------------------------- case 8


def case8():
    print("case 8: the path :w names is bounded by --io-root, like the one opened")
    # `:w PATH` lets a keystroke name where the bytes go, and the bound on it is
    # the same subtree the file was opened from: the supervisor performs the
    # write with the root authority (Session.trp), and SimpleFileIO resolves and
    # refuses the path. What is checked here is the three shapes of path a
    # command line can name — one inside the root, one outside it lexically, and
    # a symlink pointing out — against the same rules case 2 pins for opening.
    r = open_editor(["plain.txt"])
    try:
        after = r.type(b"iZ\x1b:w sub/written.txt\r", settle=2.0)
        check("a path into a subdirectory of the root is written",
              "sub/written.txt written" in status_row(after),
              repr(status_row(after)))
        check("and holds the buffer byte for byte",
              read_file("sub/written.txt") == b"Zplain\n",
              repr(read_file("sub/written.txt")))
        check("the file the buffer came from is untouched",
              read_file("plain.txt") == b"plain\n", repr(read_file("plain.txt")))
        check("the buffer is still modified after a write elsewhere",
              "[+]" in status_row(after), repr(status_row(after)))

        after = r.type(b":w ../battallion-cli-outside/stolen.txt\r", settle=2.0)
        check("a path that leaves the root is refused",
              "escapes the io-root sandbox" in status_row(after),
              repr(status_row(after)))
        check("the session survives the refusal", not r.exited)
        check("nothing was written outside the root",
              not os.path.exists(os.path.join(OUTSIDE, "stolen.txt")))

        after = r.type(b":w escape.txt\r", settle=2.0)
        check("a symlink pointing out of the root is refused",
              "escapes the io-root sandbox" in status_row(after),
              repr(status_row(after)))
        check("the file the symlink points at is untouched",
              read_file("out.txt", root=OUTSIDE) == b"outside\n",
              repr(read_file("out.txt", root=OUTSIDE)))

        after = r.type(b":w\r", settle=2.0)
        check("a bare :w still writes the buffer's own file",
              read_file("plain.txt") == b"Zplain\n",
              repr(read_file("plain.txt")))
        check("and the buffer is clean again", "[+]" not in status_row(after),
              repr(status_row(after)))

        r.send(b":q\r")
        r.wait_exit()
        check("exit code 0", r.exit_code() == 0, "rc=%s" % r.exit_code())
    finally:
        r.close()


for c in (case1, case2, case3, case4, case5, case6, case7, case8):
    c()
    print("")

if failures:
    print("FAILURES: " + ", ".join(failures))
    sys.exit(1)
print("ALL PASS")
