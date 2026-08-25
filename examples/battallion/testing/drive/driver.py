#!/usr/bin/env python3
"""An interactive pty driver for battallion (or any full-screen Troupe program).

Holds the editor open on a pty and renders its output into a terminal screen
model, so a caller with only one-shot shell commands can drive a live session:
append command lines to <session>/cmd.txt, read numbered snapshots from
<session>/snap-NNN.txt. The screen model covers the escape vocabulary
Screen.trp documents (CUP, EL, ED, alt screen, cursor visibility, DECSCUSR)
and ignores anything else it does not know, listing it in the snapshot header.

Usage:
  driver.py <troupe-root> <session-dir> <compiled.js> <io-root> <file> [rows cols]

Commands (one per line in cmd.txt, processed in order):
  text:<chars>       send the characters as typed
  key:<name>         enter esc tab backspace up down left right pgup pgdn home end
  ctrl:<letter>      the control chord
  raw:<base64>       arbitrary bytes
  resize:<R>x<C>     TIOCSWINSZ + SIGWINCH
  snap               force a snapshot now
  quit               SIGTERM the child

A snapshot is written automatically ~300 ms after output goes quiet following
any activity. status.txt holds one line: RUNNING or EXITED <code/signal>.
"""
import base64, fcntl, os, pty, select, signal, struct, sys, termios, time

TROUPE, SESSION, JS, IOROOT, FNAME = sys.argv[1:6]
ROWS = int(sys.argv[6]) if len(sys.argv) > 6 else 24
COLS = int(sys.argv[7]) if len(sys.argv) > 7 else 80

KEYS = {"enter": b"\r", "esc": b"\x1b", "tab": b"\t", "backspace": b"\x7f",
        "up": b"\x1b[A", "down": b"\x1b[B", "right": b"\x1b[C", "left": b"\x1b[D",
        "pgup": b"\x1b[5~", "pgdn": b"\x1b[6~", "home": b"\x1b[H", "end": b"\x1b[F"}


class Screen:
    def __init__(self, rows, cols):
        self.resize(rows, cols)
        self.alt = False
        self.saved = None
        self.cursor_shown = True
        self.autowrap = True       # DECAWM, as a terminal is normally found
        self.shape = None          # last DECSCUSR parameter seen
        self.unknown = []          # CSI sequences the model ignored
        self.pending = b""         # incomplete escape or UTF-8 tail

    def resize(self, rows, cols):
        self.rows, self.cols = rows, cols
        self.grid = [[" "] * cols for _ in range(rows)]
        self.r = self.c = 0

    def clear(self):
        self.grid = [[" "] * self.cols for _ in range(self.rows)]

    def putc(self, ch):
        if ch == "\r":
            self.c = 0
        elif ch == "\n":
            self.r = min(self.r + 1, self.rows - 1)
        elif ch == "\b":
            self.c = max(self.c - 1, 0)
        elif ch == "\t":
            self.c = min((self.c // 8 + 1) * 8, self.cols - 1)
        else:
            if self.c < self.cols and self.r < self.rows:
                self.grid[self.r][self.c] = ch
            self.c = min(self.c + 1, self.cols)   # one-past-end permitted

    def csi(self, params, inter, final):
        if final == "H" or final == "f":
            p = [int(x) if x else 1 for x in params.split(";")] if params else [1, 1]
            while len(p) < 2:
                p.append(1)
            self.r = min(max(p[0] - 1, 0), self.rows - 1)
            self.c = min(max(p[1] - 1, 0), self.cols - 1)
        elif final == "K":
            n = int(params) if params else 0
            if n == 0:
                for x in range(self.c, self.cols):
                    self.grid[self.r][x] = " "
            elif n == 2:
                self.grid[self.r] = [" "] * self.cols
        elif final == "J":
            self.clear()
        elif final in "hl" and params.startswith("?"):
            modes = params[1:].split(";")
            if "1049" in modes:
                if final == "h":
                    self.saved = ([row[:] for row in self.grid], self.r, self.c)
                    self.clear()
                    self.alt = True
                else:
                    if self.saved:
                        self.grid, self.r, self.c = self.saved
                        self.saved = None
                    self.alt = False
            if "25" in modes:
                self.cursor_shown = (final == "h")
            if "7" in modes:
                # DECAWM. The session clears it at startup and sets it again at
                # teardown (Screen.trp). This model never wrapped in the first
                # place -- putc saturates the column at self.cols -- so the mode
                # is recorded rather than acted on, and recorded so that the
                # escape does not read as one the model failed to recognise.
                self.autowrap = (final == "h")
        elif final == "q" and inter == " ":
            self.shape = params
        elif final == "m":
            pass
        else:
            self.unknown.append("CSI %s%s%s" % (params, inter, final))

    def feed(self, data):
        data = self.pending + data
        self.pending = b""
        i, n = 0, len(data)
        while i < n:
            b = data[i]
            if b == 0x1b:
                j = i + 1
                if j >= n:
                    self.pending = data[i:]
                    return
                if data[j:j + 1] == b"[":
                    k = j + 1
                    while k < n and not (0x40 <= data[k] <= 0x7e):
                        k += 1
                    if k >= n:
                        self.pending = data[i:]
                        return
                    body = data[j + 1:k].decode("latin1")
                    params = body.rstrip(" !\"#$%&'()*+,-./")
                    inter = body[len(params):]
                    self.csi(params, inter, chr(data[k]))
                    i = k + 1
                else:
                    self.unknown.append("ESC " + chr(data[j]))
                    i = j + 1
            elif b < 0x80:
                self.putc(chr(b))
                i += 1
            else:
                need = 2 if b >= 0xc0 and b < 0xe0 else 3 if b < 0xf0 else 4
                if i + need > n:
                    self.pending = data[i:]
                    return
                self.putc(data[i:i + need].decode("utf8", "replace"))
                i += need

    def snapshot(self, seq, child_status):
        lines = ["=== snap %03d  %s  alt=%s wrap=%s cursor=%s shape=%s "
                 "cur=(%d,%d) size=%dx%d"
                 % (seq, child_status, "on" if self.alt else "off",
                    "on" if self.autowrap else "off",
                    "shown" if self.cursor_shown else "hidden",
                    self.shape, self.r + 1, self.c + 1, self.rows, self.cols)]
        if self.unknown:
            lines.append("ignored: " + " | ".join(self.unknown[-8:]))
        for idx, row in enumerate(self.grid):
            lines.append("%3d|%s" % (idx + 1, "".join(row).rstrip()))
        return "\n".join(lines) + "\n"


def set_winsize(fd, rows, cols):
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))


os.makedirs(SESSION, exist_ok=True)
cmdpath = os.path.join(SESSION, "cmd.txt")
open(cmdpath, "a").close()

pid, master = pty.fork()
if pid == 0:
    os.chdir(TROUPE)   # the module resolver reads the deps pin file cwd-relative
    os.execvp("node", ["node", os.path.join(TROUPE, "rt/built/troupe.mjs"),
                       "-f=" + JS, "--localonly", "--io-root", IOROOT, "--", FNAME])
    os._exit(127)

set_winsize(master, ROWS, COLS)
screen = Screen(ROWS, COLS)
seq = 0
cmd_off = 0
dirty = False          # output since the last snapshot
last_out = time.time()
child_status = "child=alive"
rawlog = open(os.path.join(SESSION, "out.raw"), "wb")


def write_status(s):
    with open(os.path.join(SESSION, "status.txt"), "w") as f:
        f.write(s + "\n")


def take_snapshot():
    global seq, dirty
    seq += 1
    with open(os.path.join(SESSION, "snap-%03d.txt" % seq), "w") as f:
        f.write(screen.snapshot(seq, child_status))
    dirty = False


write_status("RUNNING")
alive = True
while True:
    r, _, _ = select.select([master], [], [], 0.1)
    if r:
        try:
            data = os.read(master, 65536)
        except OSError:
            data = b""
        if data:
            rawlog.write(data); rawlog.flush()
            screen.feed(data)
            dirty = True
            last_out = time.time()
        else:
            if alive:
                _, st = os.waitpid(pid, 0)
                code = os.waitstatus_to_exitcode(st)
                child_status = "child=exited(%s)" % code
                write_status("EXITED %s" % code)
                alive = False
                take_snapshot()
                break
    if dirty and time.time() - last_out > 0.3:
        take_snapshot()
    try:
        with open(cmdpath) as f:
            f.seek(cmd_off)
            new = f.read()
            cmd_off = f.tell()
    except FileNotFoundError:
        new = ""
    for line in new.splitlines():
        line = line.rstrip("\r\n")   # payloads may end in spaces; strip nothing else
        if not line:
            continue
        if line.startswith("text:"):
            os.write(master, line[5:].encode("utf8"))
        elif line.startswith("key:"):
            os.write(master, KEYS.get(line[4:], b""))
        elif line.startswith("ctrl:"):
            os.write(master, bytes([ord(line[5:6].lower()) - 0x60]))
        elif line.startswith("raw:"):
            os.write(master, base64.b64decode(line[4:]))
        elif line.startswith("resize:"):
            rr, cc = line[7:].split("x")
            set_winsize(master, int(rr), int(cc))
            screen.resize(int(rr), int(cc))
            os.kill(pid, signal.SIGWINCH)
        elif line == "snap":
            take_snapshot()
        elif line == "quit":
            try:
                os.kill(pid, signal.SIGTERM)
            except ProcessLookupError:
                pass

rawlog.close()
