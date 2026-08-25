"""btmodel -- a shadow model of battallion at commit e4024e39.

Every rule below is derived by reading the current sources; each is cited as
<file>.trp:<line> against the working tree of

    examples/battallion/
    lib/Rope.trp

The model was previously written against 2d4e5b10 and was recalibrated for the
wave that added HELP mode, `:w PATH`, search, the linewise register, the word
motions, the goal column and the pending-prefix mechanism (760691eb), the
revision-based modified state (a9f4bbc8), and erase-before-draw (fc8e868d).

Code units.  Every length, offset and column in the editor counts UTF-16 code
units (Api.trp:77-81, Screen.trp:42-46, Rope.trp's offsets).  So the model holds
text as a "u16 string": a Python str in which a character outside the basic
multilingual plane has been expanded into its surrogate pair, which makes
len() == String.size and s[a:b] == String.substring (lib/String.trp:27, and
lib/String.trp's `substring (s, start, end)` is a half-open range).  to_u16 /
from_u16 convert at the edges (the terminal, the file on disk).
"""

# ---------------------------------------------------------------------------
# UTF-16 code units
# ---------------------------------------------------------------------------

def to_u16(s):
    """A Python str as the editor stores it: astral characters as surrogate pairs."""
    out = []
    for ch in s:
        o = ord(ch)
        if o > 0xFFFF:
            o -= 0x10000
            out.append(chr(0xD800 + (o >> 10)))
            out.append(chr(0xDC00 + (o & 0x3FF)))
        else:
            out.append(ch)
    return "".join(out)


def from_u16(s):
    """A u16 string back to text; a lone surrogate is left as it stands."""
    out = []
    i, n = 0, len(s)
    while i < n:
        c = ord(s[i])
        if 0xD800 <= c <= 0xDBFF and i + 1 < n and 0xDC00 <= ord(s[i + 1]) <= 0xDFFF:
            out.append(chr(0x10000 + ((c - 0xD800) << 10) + (ord(s[i + 1]) - 0xDC00)))
            i += 2
        else:
            out.append(s[i])
            i += 1
    return "".join(out)


# ---------------------------------------------------------------------------
# Key values and the decoder (Key.trp -- unchanged since 2d4e5b10)
# ---------------------------------------------------------------------------
# A key is a tuple: ('PRINT', u16str) | ('CTRL', letter) | ('UNKNOWN', raw) |
# ('ENTER',) | ('TAB',) | ('BACKSPACE',) | ('DELETE',) | ('ESC',) | ('UP',) |
# ('DOWN',) | ('LEFT',) | ('RIGHT',) | ('HOME',) | ('END',) | ('PAGEUP',) |
# ('PAGEDOWN',)
#
# The decoder's input is the payload of TTY_DATA, built with
# chunk.toString('latin1'): one code unit per byte (Key.trp:5-9).  The model
# therefore decodes `bytes`.

BYTE_BS, BYTE_TAB, BYTE_LF, BYTE_CR, BYTE_ESC, BYTE_DEL = 0x08, 0x09, 0x0A, 0x0D, 0x1B, 0x7F
CTRL_LOW, CTRL_HIGH, CTRL_LETTER = 0x01, 0x1A, 0x60          # Key.trp:133-137
PARAM_LOW, PARAM_HIGH = 0x30, 0x3F                            # Key.trp:145
INTER_LOW, INTER_HIGH = 0x20, 0x2F
FINAL_LOW, FINAL_HIGH = 0x40, 0x7E
PRINT_LOW, PRINT_HIGH = 0x20, 0x7F                            # Key.trp:153-154


def _run_end(b, n, j, lo, hi):                                # Key.trp:200-204
    while j < n and lo <= b[j] <= hi:
        j += 1
    return j


def _leading_number(p):                                       # Key.trp:206-217
    acc = 0
    for d in p:
        if 0x30 <= d <= 0x39:
            acc = acc * 10 + (d - 0x30)
        else:
            return acc
    return acc


def _tilde_key(p, raw):                                       # Key.trp:223-231
    m = _leading_number(p)
    if m in (1, 7):
        return ("HOME",)
    if m in (4, 8):
        return ("END",)
    if m == 5:
        return ("PAGEUP",)
    if m == 6:
        return ("PAGEDOWN",)
    if m == 3:
        return ("DELETE",)
    return ("UNKNOWN", raw)


def _final_key(f, p, raw):                                    # Key.trp:235-243
    return {0x41: ("UP",), 0x42: ("DOWN",), 0x43: ("RIGHT",), 0x44: ("LEFT",),
            0x48: ("HOME",), 0x46: ("END",)}.get(
        f, _tilde_key(p, raw) if f == 0x7E else ("UNKNOWN", raw))


def _step(b, n, i):
    """The key at i and the position after it, or None for an unfinished prefix.
    Key.trp:301-324 (`step`), 245-275 (`stepCsi`/`stepSs3`/`stepEsc`)."""
    c = b[i]
    if c == BYTE_ESC:                                         # Key.trp:269-275
        if i + 1 >= n:
            return None
        nx = b[i + 1]
        if nx == 0x5B:                                        # CSI, Key.trp:245-259
            p_end = _run_end(b, n, i + 2, PARAM_LOW, PARAM_HIGH)
            k = _run_end(b, n, p_end, INTER_LOW, INTER_HIGH)
            if k >= n:
                return None
            f = b[k]
            if f < FINAL_LOW or f > FINAL_HIGH:
                return (("UNKNOWN", b[i:k]), k)
            return (_final_key(f, b[i + 2:p_end], b[i:k + 1]), k + 1)
        if nx == 0x4F:                                        # SS3, Key.trp:261-267
            if i + 2 >= n:
                return None
            return (_final_key(b[i + 2], b"", b[i:i + 3]), i + 3)
        return (("ESC",), i + 1)
    if c in (BYTE_CR, BYTE_LF):
        return (("ENTER",), i + 1)
    if c == BYTE_TAB:
        return (("TAB",), i + 1)
    if c in (BYTE_BS, BYTE_DEL):
        return (("BACKSPACE",), i + 1)
    if CTRL_LOW <= c <= CTRL_HIGH:
        return (("CTRL", chr(c + CTRL_LETTER)), i + 1)
    if c < PRINT_LOW:
        return (("UNKNOWN", b[i:i + 1]), i + 1)
    if c < PRINT_HIGH:                                        # 0x20..0x7e printable
        return (("PRINT", chr(c)), i + 1)
    if c < 0xC2:
        return (("UNKNOWN", b[i:i + 1]), i + 1)
    # UTF-8, Key.trp:283-297
    if c < 0xE0:
        ln, acc, lo1, hi1 = 2, c - 0xC0, 0x80, 0xBF
    elif c < 0xF0:
        ln, acc = 3, c - 0xE0
        lo1 = 0xA0 if c == 0xE0 else 0x80
        hi1 = 0x9F if c == 0xED else 0xBF
    elif c <= 0xF4:
        ln, acc = 4, c - 0xF0
        lo1 = 0x90 if c == 0xF0 else 0x80
        hi1 = 0x8F if c == 0xF4 else 0xBF
    else:
        return (("UNKNOWN", b[i:i + 1]), i + 1)
    j = 1
    while True:
        if j >= ln:
            return (("PRINT", to_u16(chr(acc))), i + ln)
        if i + j >= n:
            return None
        bb = b[i + j]
        lo = lo1 if j == 1 else 0x80
        hi = hi1 if j == 1 else 0xBF
        if bb < lo or bb > hi:
            return (("UNKNOWN", b[i:i + 1]), i + 1)
        acc = acc * 0x40 + (bb - 0x80)
        j += 1


def decode(data):
    """(keys, remainder).  Key.trp:326-340."""
    keys, i, n = [], 0, len(data)
    while i < n:
        r = _step(data, n, i)
        if r is None:
            return keys, data[i:]
        k, nxt = r
        keys.append(k)
        i = nxt
    return keys, b""


def flush(data):
    """decode plus the final reading of the remainder.  Key.trp:344-349."""
    keys, rest = decode(data)
    if rest == b"":
        return keys
    if rest == b"\x1b":
        return keys + [("ESC",)]
    return keys + [("UNKNOWN", rest)]


# ---------------------------------------------------------------------------
# The rope, as far as the editor uses it (lib/Rope.trp -- unchanged)
# ---------------------------------------------------------------------------
# The buffer is a list of u16 lines.  lineCount is one more than the number of
# newlines, so a text ending in a newline has a final empty line and the empty
# rope has one line (Rope.trp:106) -- exactly str.split("\n").

def buf_from_text(text):
    return to_u16(text).split("\n")


def buf_text(lines):
    """Rope.text -- the buffer as one string (Rope.trp:267)."""
    return "\n".join(lines)


def buf_line_start(lines, i):
    return sum(len(l) + 1 for l in lines[:i])


def buf_length(lines):
    return sum(len(l) for l in lines) + len(lines) - 1


def position_to_offset(lines, line, col):
    """Rope.trp:383-391.  Both clamped; a column past the end of its line gives
    that line's end rather than an offset on a later line."""
    last = len(lines) - 1
    k = 0 if line < 0 else (last if line > last else line)
    start = buf_line_start(lines, k)
    n = len(lines[k])
    c = 0 if col < 0 else (n if col > n else col)
    return start + c


def offset_to_position(lines, off):
    """Rope.trp:373-378.  The offset is clamped; one past the last character is
    the end of the last line."""
    n = buf_length(lines)
    p = 0 if off < 0 else (n if off > n else off)
    acc = 0
    for i, l in enumerate(lines):
        if p <= acc + len(l):
            return i, p - acc
        acc += len(l) + 1
    return len(lines) - 1, len(lines[-1])


def buf_insert(lines, off, s):
    """Rope.insert; the index is clamped, and an empty string is a no-op
    (Rope.trp:305-309)."""
    if s == "":
        return lines
    t = buf_text(lines)
    off = max(0, min(off, len(t)))
    return (t[:off] + s + t[off:]).split("\n")


def buf_delete(lines, off, ln):
    """Rope.delete; the range is clamped (Rope.trp:313-319)."""
    if ln <= 0:
        return lines
    t = buf_text(lines)
    off = max(0, min(off, len(t)))
    ln = max(0, min(ln, len(t) - off))
    return (t[:off] + t[off + ln:]).split("\n")


def buf_line_at(lines, l):
    """Rope.lineAt; the line number is clamped (Rope.trp:347-352)."""
    last = len(lines) - 1
    k = 0 if l < 0 else (last if l > last else l)
    return lines[k]


# ---------------------------------------------------------------------------
# The editor state and the operations over it (Api.trp)
# ---------------------------------------------------------------------------

NORMAL, INSERT, COMMAND, HELP = "NORMAL", "INSERT", "COMMAND", "HELP"   # Api.trp:144

# Fault injection, for the harness's own self-test.  Each value makes the model
# believe one thing the editor does not do, so a run that exercises it must
# diverge and the minimizer must find the shortest prefix that does.  Unset in
# every real run; the report says so.
#   x     `x` deletes nothing
#   goal  vertical motions do not keep the goal column (the pre-wave rule)
#   dirty a write to another path clears the modified state
import os as _os
BREAK = _os.environ.get("BTSIM_BREAK", "")

OPENING_MESSAGE = "type :help for the key list"                   # Api.trp:155
OLDEST_MESSAGE = "already at the oldest change"                   # Api.trp:167
MODIFIED_MESSAGE = "no write since last change (add ! to override)"  # FileOps.trp:55
HELP_MESSAGE = "press any key to return"                          # StatusLine.trp:41

NO_PATTERN_MESSAGE = "no previous search pattern"                 # VimMotions.trp:91
WRAP_DOWN_MESSAGE = "search hit the bottom, continuing at the top" # VimMotions.trp:92
WRAP_UP_MESSAGE = "search hit the top, continuing at the bottom"   # VimMotions.trp:93
NOTHING_YANKED_MESSAGE = "nothing has been yanked"                # VimMotions.trp:94
YANKED_MESSAGE = "line yanked"                                    # VimMotions.trp:95

GOAL_END = 1073741823                                             # Api.trp:213

SUR_HIGH_LOW, SUR_HIGH_HIGH = 0xD800, 0xDBFF                      # Api.trp:245
SUR_LOW_LOW, SUR_LOW_HIGH = 0xDC00, 0xDFFF                        # Api.trp:246


class State:
    """The record Api.trp:20-39 describes, with the fields Api.trp:159-163 gives
    a fresh session."""

    __slots__ = ("lines", "path", "top", "line", "col", "cols", "rows",
                 "mode", "rev", "rev_next", "saved_rev", "undo",
                 "prompt", "cmd", "status", "goal_col", "pending", "find",
                 "yank", "req")

    def __init__(self, lines, path, cols, rows):
        # Api.trp:159-163
        self.lines, self.path = lines, path
        self.top = self.line = self.col = 0
        self.cols, self.rows = cols, rows
        self.mode = NORMAL
        self.rev, self.rev_next, self.saved_rev = 0, 1, 0
        self.undo = []
        self.prompt, self.cmd, self.status = "", "", OPENING_MESSAGE
        self.goal_col, self.pending, self.find = 0, "", ""
        self.yank = None                    # None | u16 string     Api.trp:163
        self.req = None                     # None | ('WRITE', path, quit)

    def copy(self):
        s = State.__new__(State)
        for f in State.__slots__:
            setattr(s, f, getattr(self, f))
        s.undo = list(self.undo)
        return s

    # -- queries ------------------------------------------------------------

    def text_rows(self):                                           # Api.trp:173
        return self.rows - 1 if self.rows > 1 else self.rows

    def has_status(self):                                          # Api.trp:176
        return self.rows > 1

    def last_line(self):                                           # Api.trp:178
        return len(self.lines) - 1

    def line_at(self, l):                                          # Api.trp:180
        return buf_line_at(self.lines, l)

    def line_length(self, l):                                      # Api.trp:182
        return len(self.line_at(l))

    def is_dirty(self):                                            # Api.trp:186
        """There is no modified flag: the identity of the present contents
        against the identity the file holds."""
        return self.rev != self.saved_rev

    def contents(self):                                            # Api.trp:189
        return buf_text(self.lines)

    # -- motions ------------------------------------------------------------

    def clamp_line(self, l):                                       # Api.trp:193-196
        last = self.last_line()
        return 0 if l < 0 else (last if l > last else l)

    def clamp_col(self, l, c):                                     # Api.trp:198-201
        n = self.line_length(l)
        return 0 if c < 0 else (n if c > n else c)

    def scroll_to(self, line):                                     # Api.trp:204-209
        h = self.text_rows()
        if line < self.top:
            return line
        if line >= self.top + h:
            return line - h + 1
        return self.top

    def move_to(self, l, c):                                       # Api.trp:217-221
        """An exact position, and that column becomes the goal."""
        st = self.copy()
        l2 = st.clamp_line(l)
        c2 = st.clamp_col(l2, c)
        st.line, st.col, st.goal_col = l2, c2, c2
        st.top = st.scroll_to(l2)
        return st

    def move_line(self, l):                                        # Api.trp:226-230
        """A line, at the goal column clamped to it; the goal is carried
        through.  This is the operation that makes a walk over a short line and
        back onto a long one return to the column it started at."""
        st = self.copy()
        l2 = st.clamp_line(l)
        goal = st.col if BREAK == "goal" else st.goal_col
        st.line, st.col = l2, st.clamp_col(l2, goal)
        st.top = st.scroll_to(l2)
        return st

    def move_end(self, l):                                         # Api.trp:234-238
        """The end of a line, with `goalEnd` as the goal, so that `$` sticks."""
        st = self.copy()
        l2 = st.clamp_line(l)
        st.line, st.col = l2, st.line_length(l2)
        st.goal_col = GOAL_END
        st.top = st.scroll_to(l2)
        return st

    # -- editing ------------------------------------------------------------

    def _unit_at(self, i):                                         # Api.trp:249
        t = self.contents()
        return ord(t[i]) if 0 <= i < len(t) else 0

    def offset_of(self):                                           # Api.trp:252
        return position_to_offset(self.lines, self.line, self.col)

    def units_before(self, off):                                   # Api.trp:255-262
        if off <= 1:
            return 1
        lo, hi = self._unit_at(off - 1), self._unit_at(off - 2)
        if SUR_LOW_LOW <= lo <= SUR_LOW_HIGH and SUR_HIGH_LOW <= hi <= SUR_HIGH_HIGH:
            return 2
        return 1

    def units_at(self, off):                                       # Api.trp:265-268
        hi = self._unit_at(off)
        return 2 if SUR_HIGH_LOW <= hi <= SUR_HIGH_HIGH else 1

    def at_offset(self, lines, off):                               # Api.trp:275-280
        """The one place a buffer changes, and so the one place a revision is
        handed out.  The goal column is the column landed on."""
        st = self.copy()
        st.lines = lines
        st.line, st.col = offset_to_position(lines, off)
        st.goal_col = st.col
        st.rev, st.rev_next = st.rev_next, st.rev_next + 1
        st.top = st.scroll_to(st.line)
        return st

    def remember(self):                                            # Api.trp:285-287
        """Buffer, cursor and revision, and nothing else.  `revNext` is not in
        the snapshot: it counts identities handed out, which undoing does not
        give back."""
        st = self.copy()
        st.undo = [(self.lines, self.line, self.col, self.rev)] + self.undo
        return st

    def insert_text(self, s):                                      # Api.trp:290-294
        off = self.offset_of()
        st1 = self.remember()
        return st1.at_offset(buf_insert(st1.lines, off, s), off + len(s))

    def delete_at(self, off, n):                                   # Api.trp:297-301
        if n <= 0:
            return self
        st1 = self.remember()
        return st1.at_offset(buf_delete(st1.lines, off, n), off)

    def delete_char(self):                                         # Api.trp:306-310
        off = self.offset_of()
        if self.col >= self.line_length(self.line):
            return self
        return self.delete_at(off, self.units_at(off))

    def backspace(self):                                           # Api.trp:314-320
        off = self.offset_of()
        if off <= 0:
            return self
        n = self.units_before(off)
        return self.delete_at(off - n, n)

    def open_below(self):                                          # Api.trp:323-328
        stop = position_to_offset(self.lines, self.line, self.line_length(self.line))
        st1 = self.remember()
        st2 = st1.at_offset(buf_insert(st1.lines, stop, "\n"), stop + 1)
        st2.mode = INSERT
        return st2

    def delete_line(self):                                         # Api.trp:342-361
        """A line that has one below it goes with the break that follows it, the
        last line with the break before it, so the buffer loses exactly one line
        either way.  The only line of a buffer has its text emptied instead, and
        one that is both the only line and already empty leaves nothing to do --
        nothing is pushed onto the undo history for it."""
        l, last = self.line, self.last_line()
        n = self.line_length(l)
        here = position_to_offset(self.lines, l, 0)
        if l < last:
            st1 = self.remember()
            return st1.at_offset(buf_delete(st1.lines, here, n + 1), here)
        if l > 0:
            above = position_to_offset(self.lines, l - 1, 0)
            st1 = self.remember()
            return st1.at_offset(buf_delete(st1.lines, here - 1, n + 1), above)
        if n > 0:
            st1 = self.remember()
            return st1.at_offset(buf_delete(st1.lines, 0, n), 0)
        return self

    def put_below(self, s):                                        # Api.trp:364-368
        stop = position_to_offset(self.lines, self.line, self.line_length(self.line))
        st1 = self.remember()
        return st1.at_offset(buf_insert(st1.lines, stop, "\n" + s), stop + 1)

    def put_above(self, s):                                        # Api.trp:371-375
        here = position_to_offset(self.lines, self.line, 0)
        st1 = self.remember()
        return st1.at_offset(buf_insert(st1.lines, here, s + "\n"), here)

    def undo_op(self):                                             # Api.trp:378-384
        """The revision is restored with the contents it belongs to, which is
        what makes undoing past a write show modified again (Api.trp:99-115)."""
        if not self.undo:
            st = self.copy()
            st.status = OLDEST_MESSAGE
            return st
        (b, l, c, rev), rest = self.undo[0], self.undo[1:]
        st1 = self.copy()
        st1.lines, st1.undo, st1.rev = b, rest, rev
        return st1.move_to(l, c)

    # -- mode, prompt, status, keymap bookkeeping ---------------------------

    def set_mode(self, m):                                         # Api.trp:388
        st = self.copy(); st.mode = m; return st

    def set_cmd(self, c):                                          # Api.trp:391
        st = self.copy(); st.cmd = c; return st

    def open_prompt(self, p):                                      # Api.trp:396
        st = self.copy(); st.mode, st.prompt, st.cmd = COMMAND, p, ""; return st

    def close_prompt(self):                                        # Api.trp:399
        st = self.copy(); st.mode, st.prompt, st.cmd = NORMAL, "", ""; return st

    def set_status(self, s):                                       # Api.trp:402
        st = self.copy(); st.status = s; return st

    def set_pending(self, s):                                      # Api.trp:405
        st = self.copy(); st.pending = s; return st

    def set_find(self, s):                                         # Api.trp:408
        st = self.copy(); st.find = s; return st

    def set_yank(self, y):                                         # Api.trp:411
        st = self.copy(); st.yank = y; return st

    def mark_saved(self):                                          # Api.trp:415
        st = self.copy(); st.saved_rev = st.rev; return st

    # -- requests -----------------------------------------------------------

    def request_write(self, path, quit_after):                     # Api.trp:422-423
        st = self.copy(); st.req = ("WRITE", path, quit_after); return st

    def clear_request(self):                                       # Api.trp:426
        st = self.copy(); st.req = None; return st

    def has_request(self):                                         # Api.trp:429
        return self.req is not None

    def write_path(self):                                          # Api.trp:432
        return self.req[1] if self.req is not None else ""

    def write_quits(self):                                         # Api.trp:435
        return self.req[2] if self.req is not None else False

    # -- resizing -----------------------------------------------------------

    def on_resize(self, cols, rows):                               # Api.trp:443-447
        """The goal column is carried across unchanged: a window resize is not a
        motion."""
        st = self.copy()
        st.cols, st.rows = cols, rows
        st2 = st.move_to(st.line, st.col)
        st2.goal_col = self.goal_col
        return st2


# ---------------------------------------------------------------------------
# FileOps.trp -- what a command line means
# ---------------------------------------------------------------------------

def _split_cmd(c):
    """FileOps.trp:59-64.  The line split at its first space: the word, and what
    follows it trimmed (lib/String.trp:182-190 trims ASCII whitespace)."""
    i = c.find(" ")
    if i < 0:
        return c, ""
    return c[:i], c[i + 1:].strip(" \t\n\r")


def _line_number(s):
    """FileOps.trp:68-76.  Every character has to be a digit, so `12x` is not a
    line number; the empty string is not one either."""
    if s == "":
        return -1
    acc = 0
    for ch in s:
        u = ord(ch)
        if 48 <= u <= 57:
            acc = acc * 10 + (u - 48)
        else:
            return -1
    return acc


def fileops_run(st, c):
    """(state', quit).  FileOps.trp:90-106."""
    word, arg = _split_cmd(c)
    bare = arg == ""
    target = st.path if arg == "" else arg                         # FileOps.trp:79
    if word == "w":
        return st.request_write(target, False), False
    if word in ("wq", "x"):
        return st.request_write(target, True), False
    if word == "q" and bare:                                       # FileOps.trp:82-83
        if st.is_dirty():
            return st.set_status(MODIFIED_MESSAGE), False
        return st, True
    if word == "q!" and bare:
        return st, True
    if word in ("help", "h") and bare:
        return st.set_mode(HELP), False
    if word == "$" and bare:
        return st.move_to(st.last_line(), 0), False
    if bare and _line_number(word) >= 0:
        return st.move_to(_line_number(word) - 1, 0), False
    return st.set_status("not an editor command: " + c), False     # FileOps.trp:85


def fileops_on_written(st, ok, message):
    """FileOps.trp:114-121.  The status names the file that was written, and
    only a write to the buffer's own path moves `savedRev`."""
    quit_after = st.write_quits()
    path = st.write_path()
    note = (path + " written") if ok else message
    st2 = st.clear_request().set_status(note)
    if ok and (path == st.path or BREAK == "dirty"):
        st2 = st2.mark_saved()
    return st2, (ok and quit_after)


# ---------------------------------------------------------------------------
# VimMotions.trp -- the keymap
# ---------------------------------------------------------------------------

CLASS_SPACE, CLASS_WORD, CLASS_OTHER = 0, 1, 2                     # VimMotions.trp:103-105


def _class_of(ch):                                                 # VimMotions.trp:107-116
    if ch in (" ", "\t"):
        return CLASS_SPACE
    u = ord(ch)
    if (48 <= u <= 57) or (65 <= u <= 90) or (97 <= u <= 122) or u == 95 or u >= 128:
        return CLASS_WORD
    return CLASS_OTHER


def _next_word_in(line, n, c):                                     # VimMotions.trp:124-130
    def skip(i, k):
        while i < n and _class_of(line[i]) == k:
            i += 1
        return i
    here = _class_of(line[c]) if c < n else CLASS_SPACE
    after = c if here == CLASS_SPACE else skip(c, here)
    start = skip(after, CLASS_SPACE)
    return start if start < n else -1


def _prev_word_in(line, n, c):                                     # VimMotions.trp:134-145
    frm = n if c > n else c
    i = frm - 1
    while i >= 0 and _class_of(line[i]) == CLASS_SPACE:
        i -= 1
    if i < 0:
        return -1
    k = _class_of(line[i])
    j = i
    while j >= 0 and _class_of(line[j]) == k:
        j -= 1
    return j + 1


def _word_forward(st):                                             # VimMotions.trp:150-164
    last = st.last_line()
    l, c, fresh = st.line, st.col, False
    while True:
        line = st.line_at(l)
        n = len(line)
        if fresh and (n == 0 or _class_of(line[0]) != CLASS_SPACE):
            return st.move_to(l, 0)
        hit = _next_word_in(line, n, c)
        if hit >= 0:
            return st.move_to(l, hit)
        if l < last:
            l, c, fresh = l + 1, 0, True
            continue
        return st.move_to(l, n)


def _word_backward(st):                                            # VimMotions.trp:168-181
    l, c, fresh = st.line, st.col, False
    while True:
        line = st.line_at(l)
        n = len(line)
        if fresh and n == 0:
            return st.move_to(l, 0)
        hit = _prev_word_in(line, n, c)
        if hit >= 0:
            return st.move_to(l, hit)
        if l > 0:
            l, c, fresh = l - 1, GOAL_END, True
            continue
        return st.move_to(l, 0)


def _first_non_blank(st, l):                                       # VimMotions.trp:185-190
    line = st.line_at(l)
    i, n = 0, len(line)
    while i < n and _class_of(line[i]) == CLASS_SPACE:
        i += 1
    return i


# -- search ----------------------------------------------------------------
# `strIndexOf (s, needle, from)` is JS String.prototype.indexOf
# (rt/src/builtins/string.mts:64-85): -1 for no hit, `from` read as 0 when
# negative.  The pattern is never empty here -- `runSearch` and `repeatSearch`
# both intercept that case.

def _hit_from(st, pat, l0, c0):                                    # VimMotions.trp:196-204
    last = st.last_line()
    l, frm = l0, c0
    while l <= last:
        i = st.line_at(l).find(pat, max(0, frm))
        if i >= 0:
            return (l, i)
        l, frm = l + 1, 0
    return None


def _last_hit_in(line, pat, limit):                                # VimMotions.trp:207-213
    i, best = 0, -1
    while True:
        j = line.find(pat, i)
        if j < 0 or j >= limit:
            return best
        i, best = j + 1, j


def _hit_before(st, pat, l0, c0):                                  # VimMotions.trp:216-223
    l, limit = l0, c0
    while l >= 0:
        i = _last_hit_in(st.line_at(l), pat, limit)
        if i >= 0:
            return (l, i)
        l, limit = l - 1, GOAL_END
    return None


def _search_for(st, pat, down):                                    # VimMotions.trp:229-245
    if down:
        hit = _hit_from(st, pat, st.line, st.col + 1)
        wrapped = hit is None
        if wrapped:
            hit = _hit_from(st, pat, 0, 0)
    else:
        hit = _hit_before(st, pat, st.line, st.col)
        wrapped = hit is None
        if wrapped:
            hit = _hit_before(st, pat, st.last_line(), GOAL_END)
    note = WRAP_DOWN_MESSAGE if down else WRAP_UP_MESSAGE
    if hit is None:
        return st.set_status("pattern not found: " + pat)           # VimMotions.trp:97
    st2 = st.move_to(hit[0], hit[1])
    return st2.set_status(note) if wrapped else st2


def _run_search(st, pat):                                          # VimMotions.trp:249-253
    if pat == "":
        if st.find == "":
            return st.set_status(NO_PATTERN_MESSAGE)
        return _search_for(st, st.find, True)
    return _search_for(st.set_find(pat), pat, True)


def _repeat_search(st, down):                                      # VimMotions.trp:256-258
    if st.find == "":
        return st.set_status(NO_PATTERN_MESSAGE)
    return _search_for(st, st.find, down)


# -- yank and put -----------------------------------------------------------

def _yank_line(st):                                                # VimMotions.trp:262-263
    return st.set_yank(st.line_at(st.line)).set_status(YANKED_MESSAGE)


def _put_line(st, below):                                          # VimMotions.trp:265-268
    if st.yank is None:
        return st.set_status(NOTHING_YANKED_MESSAGE)
    return st.put_below(st.yank) if below else st.put_above(st.yank)


# -- the keymap -------------------------------------------------------------

def _motion(st, k):
    """VimMotions.trp:275-283.  The vertical motions go through `moveLine`,
    which keeps the goal column, and the horizontal ones through `moveTo` and
    `moveEnd`, which set it.  Any other key leaves the state alone."""
    t = k[0]
    if t == "LEFT":
        return st.move_to(st.line, st.col - 1)
    if t == "RIGHT":
        return st.move_to(st.line, st.col + 1)
    if t == "DOWN":
        return st.move_line(st.line + 1)
    if t == "UP":
        return st.move_line(st.line - 1)
    if t == "HOME":
        return st.move_to(st.line, 0)
    if t == "END":
        return st.move_end(st.line)
    if t == "PAGEDOWN":
        return st.move_line(st.line + st.text_rows())
    if t == "PAGEUP":
        return st.move_line(st.line - st.text_rows())
    return st


def _pending_key(st, prefix, k):
    """VimMotions.trp:289-294.  The prefix is already cleared from `st`: a pair
    that is not bound leaves the state as it stands, so no key can be left
    half-pressed.  `dd` yanks the line it deletes, which leaves the yank
    message in the status line."""
    if k[0] != "PRINT":
        return st, False
    c = k[1]
    if prefix == "g" and c == "g":
        return st.move_to(0, 0), False
    if prefix == "d" and c == "d":
        return _yank_line(st).delete_line(), False
    if prefix == "y" and c == "y":
        return _yank_line(st), False
    return st, False


def _normal_print(st, c):
    """VimMotions.trp:298-321."""
    if c == "h":
        return st.move_to(st.line, st.col - 1), False
    if c == "l":
        return st.move_to(st.line, st.col + 1), False
    if c == "j":
        return st.move_line(st.line + 1), False
    if c == "k":
        return st.move_line(st.line - 1), False
    if c == "0":
        return st.move_to(st.line, 0), False
    if c == "^":
        return st.move_to(st.line, _first_non_blank(st, st.line)), False
    if c == "$":
        return st.move_end(st.line), False
    if c == "w":
        return _word_forward(st), False
    if c == "b":
        return _word_backward(st), False
    if c == "G":
        return st.move_to(st.last_line(), 0), False
    if c == "i":
        return st.set_mode(INSERT), False
    if c == "o":
        return st.open_below(), False
    if c == "x":
        return (st, False) if BREAK == "x" else (st.delete_char(), False)
    if c == "p":
        return _put_line(st, True), False
    if c == "P":
        return _put_line(st, False), False
    if c == "u":
        return st.undo_op(), False
    if c == "n":
        return _repeat_search(st, True), False
    if c == "N":
        return _repeat_search(st, False), False
    if c == "/":
        return st.open_prompt("/"), False
    if c == ":":
        return st.open_prompt(":"), False
    if c in ("g", "d", "y"):
        return st.set_pending(c), False
    if c == "q":
        return fileops_run(st, "q")
    return st, False


def _normal_key(st, k):
    """VimMotions.trp:323-330."""
    if k[0] == "PRINT":
        return _normal_print(st, k[1])
    if k[0] == "CTRL":
        c = k[1]
        if c == "f":
            return st.move_line(st.line + st.text_rows()), False
        if c == "b":
            return st.move_line(st.line - st.text_rows()), False
        if c == "c":
            return fileops_run(st, "q")
        return st, False
    if k[0] == "DELETE":
        return st.delete_char(), False
    return _motion(st, k), False


def _insert_key(st, k):
    """VimMotions.trp:334-340."""
    t = k[0]
    if t == "PRINT":
        return st.insert_text(k[1]), False
    if t == "ENTER":
        return st.insert_text("\n"), False
    if t == "TAB":
        return st.insert_text("\t"), False
    if t == "BACKSPACE":
        return st.backspace(), False
    if t == "DELETE":
        return st.delete_char(), False
    if t == "ESC":
        return st.set_mode(NORMAL), False
    return _motion(st, k), False


def _command_key(st, k):
    """VimMotions.trp:349-358.  One mode for two prompts; only Enter looks at
    which prompt it is (`runPrompt`, VimMotions.trp:346-347)."""
    t = k[0]
    if t == "PRINT":
        return st.set_cmd(st.cmd + k[1]), False
    if t == "ENTER":
        closed = st.close_prompt()
        if st.prompt == "/":
            return _run_search(closed, st.cmd), False
        return fileops_run(closed, st.cmd)
    if t == "ESC":
        return st.close_prompt(), False
    if t == "BACKSPACE":
        if st.cmd == "":
            return st.close_prompt(), False
        return st.set_cmd(st.cmd[:-1]), False
    if t == "CTRL":
        if k[1] == "c":
            return st.close_prompt(), False
        return st, False
    return st, False


def on_key(st0, k):
    """(state', quit) for one key.  VimMotions.trp:375-384.

    The transient status message is cleared here rather than where it is set, so
    every message is shown for exactly as long as it takes the user to press the
    next key.  A prefix waiting in the state is spent here too, and only in
    normal mode: the key is looked up as a pair and the prefix is cleared
    whether or not the pair is bound."""
    st = st0 if st0.status == "" else st0.set_status("")
    if st.mode == INSERT:
        return _insert_key(st, k)
    if st.mode == COMMAND:
        return _command_key(st, k)
    if st.mode == HELP:                                            # VimMotions.trp:364
        return st.set_mode(NORMAL), False
    if st.pending == "":
        return _normal_key(st, k)
    return _pending_key(st.set_pending(""), st.pending, k)


# ---------------------------------------------------------------------------
# Help.trp -- the page `:help` draws over the buffer
# ---------------------------------------------------------------------------
# The sections are what the plugin set's members export, concatenated in the
# order Editor.trp:142 gives: VimMotions.help then FileOps.help.

HELP_SECTIONS = [
    ("MOTION",                                                     # VimMotions.trp:391-400
     [("h j k l", "move by one"),
      ("arrows", "move by one"),
      ("0 Home", "start of the line"),
      ("^", "first non-blank"),
      ("$ End", "end of the line"),
      ("w b", "next, previous word"),
      ("gg G", "first, last line"),
      ("^B ^F", "up, down a screen"),
      ("PgUp PgDn", "up, down a screen")]),
    ("EDIT",                                                       # VimMotions.trp:401-409
     [("i", "insert before the cursor"),
      ("o", "open a line below"),
      ("x Del", "delete a character"),
      ("dd", "delete the line"),
      ("yy", "yank the line"),
      ("p P", "put it below, above"),
      ("u", "undo one change"),
      ("Esc", "leave insert mode")]),
    ("SEARCH",                                                     # VimMotions.trp:410-412
     [("/text", "search forward"),
      ("n N", "next, previous match")]),
    ("SESSION",                                                    # VimMotions.trp:413-415
     [(":", "start a command"),
      ("q ^C", "quit like :q")]),
    ("COMMANDS",                                                   # FileOps.trp:126-133
     [(":w", "write the buffer"),
      (":w PATH", "write to another file"),
      (":wq :x", "write and quit"),
      (":q", "quit"),
      (":q!", "quit, discarding changes"),
      (":N :$", "go to a line, the last"),
      (":help", "this page")]),
]

HELP_TITLE = "battallion: keys and commands"                       # Help.trp:35
HELP_GUTTER = 2                                                    # Help.trp:38
HELP_INDENT = "  "                                                 # Help.trp:41
HELP_KEYGAP = "  "                                                 # Help.trp:44


def _pack(blocks, h):
    """Help.trp:65-75.  Blocks into columns of at most `h` rows, with a blank
    row between two sections that share a column.  A block always starts a
    column, so one taller than `h` gets a column of its own and overflows it."""
    cols, cur = [], []
    for b in blocks:
        if not cur:
            cur = list(b)
        elif len(cur) + 1 + len(b) <= h:
            cur = cur + [""] + list(b)
        else:
            cols.append(cur)
            cur = list(b)
    if cur:
        cols.append(cur)
    return cols


def _row_text(cells, w):
    """Help.trp:85-89.  Each cell padded to `w` only when something follows."""
    if not cells:
        return ""
    if len(cells) == 1:
        return cells[0]
    rest = _row_text(cells[1:], w)
    if rest == "":
        return cells[0]
    return cells[0].ljust(w) + rest                                 # String.padRight


def help_page(sections, cols, rows):
    """The page as exactly `rows` rows, laid out for a terminal `cols` wide.
    Help.trp:108-131."""
    entries = [e for _, es in sections for e in es]
    keyw = max([len(k) for k, _ in entries] + [0])
    descw = max([len(d) for _, d in entries] + [0])
    colw = len(HELP_INDENT) + keyw + len(HELP_KEYGAP) + descw + HELP_GUTTER
    wide = 1 if colw <= 0 else cols // colw                        # Number.intDiv, floor
    nc = 1 if wide < 1 else wide

    header = [HELP_TITLE, ""] if rows >= 5 else []
    avail = rows - len(header)

    blocks = [[t] + [HELP_INDENT + k.ljust(keyw) + HELP_KEYGAP + d for k, d in es]
              for t, es in sections]                               # Help.trp:57-58
    columns = _pack(blocks, avail)
    shown = columns[:nc] if nc > 0 else []
    left = len(columns) - nc

    def cell_at(c, i):
        return c[i] if 0 <= i < len(c) else ""

    body = [_row_text([cell_at(c, i) for c in shown], colw)
            for i in range(max(0, avail))]
    more = ("(1 more column not shown)" if left == 1
            else "(" + str(left) + " more columns not shown)")
    cut = (body[:max(0, avail - 1)] + [more]) if left > 0 else body

    out = header + cut
    if rows <= 0:
        return []
    return (out + [""] * rows)[:rows]                              # Help.exactly


# ---------------------------------------------------------------------------
# StatusLine.trp and Editor.frameOf
# ---------------------------------------------------------------------------

def status_row(st):
    """StatusLine.trp:46-57."""
    if st.mode == COMMAND:
        return st.prompt + st.cmd
    if st.mode == HELP:
        return HELP_MESSAGE
    flag = " [+]" if st.is_dirty() else ""
    name = "INSERT" if st.mode == INSERT else "NORMAL"
    pos = "%d,%d" % (st.line + 1, st.col + 1)
    base = st.path + flag + "  " + name + "  " + pos
    held = base if st.pending == "" else base + "  (" + st.pending + ")"
    return held if st.status == "" else held + "  " + st.status


TAB_STOP = 8


def after_tab(col, width):
    """Screen.trp's `afterTab`: the column a tab written at `col` moves to --
    the next tab stop, or the last column when that stop is past the edge."""
    stop = col + TAB_STOP - (col % TAB_STOP)
    return width - 1 if stop > width - 1 else stop


def screen_column(text, i, width):
    """Screen.trp's `column`: the screen column index `i` of `text` is drawn at,
    tabs taking the columns the terminal gives them and everything else one. An
    `i` past the end gives the column the cursor rests at after the whole of it,
    and a column past the right edge comes back as the last one."""
    if width <= 0:
        return 0
    col, n = 0, len(text)
    for j in range(min(i, n)):
        col = after_tab(col, width) if text[j] == "\t" else col + 1
    return width - 1 if col > width - 1 else col


def truncate(line, width):
    """Screen.trp's `truncate`: cut to the columns the line is drawn in, the cut
    falling before the first code unit with no column left."""
    if width <= 0:
        return ""
    col = 0
    for i, ch in enumerate(line):
        if ch == "\t":
            col = after_tab(col, width)
        elif col >= width:
            return line[:i]
        else:
            col += 1
    return line


def frame_of(st):
    """(rows, curRow, curCol): the lines one per terminal row, truncated as the
    renderer truncates them, and the cursor's position on the screen.
    Editor.trp:103-129."""
    h = st.text_rows()
    last = st.last_line()
    if st.mode == HELP:                                            # Editor.trp:111
        body = help_page(HELP_SECTIONS, st.cols, h)
    else:
        body = ["~" if st.top + i > last else st.line_at(st.top + i)
                for i in range(h)]
    lines = body + [status_row(st)] if st.has_status() else body   # Editor.trp:113

    def on_screen(text, c):                                        # Editor.trp:120-124
        return screen_column(text, c, st.cols)

    in_buffer = (st.line - st.top, on_screen(st.line_at(st.line), st.col))
    typed = st.prompt + st.cmd
    if st.mode == COMMAND:                                         # Editor.trp:127-130
        cur = (h, on_screen(typed, len(typed))) if st.has_status() else in_buffer
    elif st.mode == HELP:                                          # Editor.trp:126
        cur = (0, 0)
    else:
        cur = in_buffer
    return [truncate(l, st.cols) for l in lines], cur[0], cur[1]


# ---------------------------------------------------------------------------
# What the terminal makes of a row
# ---------------------------------------------------------------------------
# The renderer erases each line whole *before* writing its text (Screen.trp:93-97
# and its header at :29-36), so a cell a tab jumps over is blank rather than
# holding the previous frame's character.  That makes a row that contains a tab
# a function of the buffer alone, which is what lets one be asserted.
#
# TAB_STOP and the tab's clamp are measured, not assumed (tmux 3.5a, 20 columns):
#
#   "x"*17 + "\t" + "Z"   -> 17 x's, two blanks, Z in the last cell, so a tab
#                            whose stop is past the right edge leaves the cursor
#                            in the last column.
#
# The session runs with the terminal's automatic wrap cleared (Screen.trp:
# `disableWrap`, written with the alternate-screen switch), so a character with
# no column left is dropped at the right edge rather than continued on the next
# physical row.  That is what makes a frame's row count equal to the terminal's:
# before it, an over-wide row wrapped, the frame wrote one physical row too many,
# the alternate screen scrolled and the frame's top row went with it.


def render_row(text, cols):
    """(what the terminal shows for `text`, whether the terminal had to clip it).

    `text` has already been cut to the columns *this model* says it is drawn in
    (`truncate`), so a clip here means the terminal disagreed -- which it does
    for a character whose display width is not one, the double-width ones being
    the case neither this model nor Screen.trp measures."""
    cells = [" "] * cols
    col, clipped = 0, False
    for ch in text:
        if ch == "\t":
            col = after_tab(col, cols) if cols > 0 else 0
        elif col >= cols:
            clipped = True
        else:
            cells[col] = ch
            col += 1
    return "".join(cells).rstrip(), clipped


def screen_rows(st):
    """(rows the terminal should be showing, cursor row, cursor column, whether
    the terminal had to clip any row, how many of the rows hold a tab)."""
    rows, cur_row, cur_col = frame_of(st)
    out, over, tabs = [], False, 0
    for r in rows:
        text, o = render_row(from_u16(r), st.cols)
        out.append(text)
        over = over or o
        if "\t" in r:
            tabs += 1
    return out, cur_row, cur_col, over, tabs


# ---------------------------------------------------------------------------
# The session: bytes in, state out
# ---------------------------------------------------------------------------
# Session.trp:217-245 (kernelLoop), :168-190 (applyKeys / runKeys).  The kernel
# folds the keys of one chunk, stopping at the first key that quits and at the
# first that raises a request; a write is a round trip through the supervisor
# and its outcome goes back into the state before the remaining keys are folded.


class Session:
    """The model of one editor session.  `feed` takes terminal bytes, `flush_esc`
    is the escape timer firing (Session.trp:102, :230-232).

    `disk` is what every file the session can reach should hold, so that a
    `:w PATH` can be checked on both the copy and the original."""

    def __init__(self, lines, path, cols, rows, writable=None):
        self.st = State(lines, path, cols, rows)
        self.carry = b""
        self.quit = False
        self.disk = {path: buf_text(lines)}
        self.writable = writable      # None: every path succeeds
        self.writes = []              # (path, contents, ok), in order

    def _write(self, path, contents):
        """What the supervisor's `writeBuffer` answers (Session.trp:285-288).
        A write the runtime refuses answers (False, reason); the campaign only
        names paths inside the io-root, so every write here succeeds."""
        if self.writable is not None and path not in self.writable:
            return False, "path escapes the io-root sandbox"
        self.disk[path] = contents
        return True, "written"

    def _run_keys(self, keys):
        """applyKeys folded under runKeys, Session.trp:168-190."""
        i, quit = 0, False
        while i < len(keys):
            self.st, quit = on_key(self.st, keys[i])
            i += 1
            if quit or self.st.has_request():
                break
        if self.st.has_request():                     # runKeys, Session.trp:181
            path = self.st.write_path()
            contents = self.st.contents()
            ok, message = self._write(path, contents)
            self.writes.append((path, contents, ok))
            self.st, quit_after = fileops_on_written(self.st, ok, message)
            if quit or quit_after:
                self.quit = True
                return
            self._run_keys(keys[i:])
        elif quit:
            self.quit = True

    def feed(self, data):
        """One TTY_DATA chunk."""
        keys, rest = decode(self.carry + data)
        self.carry = rest
        self._run_keys(keys)

    def flush_esc(self):
        """The escape timer fired on a pending carry: Key.flush reads it as final
        (Session.trp:230-232)."""
        if self.carry == b"":
            return
        keys = flush(self.carry)
        self.carry = b""
        self._run_keys(keys)

    def resize(self, cols, rows):
        """Session.trp:228-229 -- Editor.onResize, then a redraw."""
        self.st = self.st.on_resize(cols, rows)

    def screen(self):
        return screen_rows(self.st)

    def file_bytes(self, path):
        """What `path` should hold on disk, or None when the session has never
        put anything there."""
        t = self.disk.get(path)
        return None if t is None else from_u16(t).encode("utf-8")
