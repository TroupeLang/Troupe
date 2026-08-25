#!/usr/bin/env python3
"""simulate.py -- a seeded user-activity simulator for battallion (commit e4024e39).

A seeded generator drives realistic keystroke bursts through a real tmux pane and
a shadow model (btmodel.py) predicts, after every action, the whole screen, the
cursor position and every file the session can write.  Every action is appended
to a JSONL replay log; on a divergence the run stops, dumps screen +
expectation, and binary-searches the shortest action prefix that still diverges.

  python3 simulate.py run  --seed 1 --file small --actions 400
  python3 simulate.py run  --seed 1 --file large --actions 400 --resize
  python3 simulate.py replay <log.jsonl> [--upto N]
  python3 simulate.py gen-files <dir>

The key bytes tmux sends for each named key are the ones probe_tmux.sh and the
key-byte probe measured (Up=1b5b41, Home=1b5b317e, DC=1b5b337e, ...).

WHAT IS ASSERTED, AND WHAT CHANGED SINCE THE FIRST CAMPAIGN.  Every body row,
the status row, the cursor and every file on disk, after every action.  Three
things the first campaign could not assert now are:

  * the modified flag, everywhere including across a save -- it is no longer a
    flag but a comparison of revisions (Api.trp:99-115), so the misreport the
    first campaign whitelisted is gone and `[+]` is checked in every state;
  * rows that contain tabs -- each line is erased whole before it is written
    (Screen.trp:29-36), so a cell a tab jumps over is blank rather than holding
    the previous frame's character, which makes the row a function of the buffer
    alone (btmodel.render_row);
  * the command-mode cursor column, which the kernel now clamps to the screen
    (Editor.trp:117-125).

What still cannot be asserted is a frame holding a character whose display width
is neither one column nor a tab's jump -- a double-width one, say.  Truncation
counts the columns a row is drawn in (Screen.trp's `truncate`) and this model
counts them the same way, so the two agree except on such a character, where the
terminal draws wider than either says and clips what does not fit; the session
runs with automatic wrap cleared, so the clip is the whole of the damage.  Those
frames are counted as `overflow_frames` and skipped.
"""

import argparse
import collections
import json
import os
import random
import shutil
import statistics
import subprocess
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import btmodel as M
from tmuxdrv import Pane, wait_stable, wait_started, ROOT, SCRATCH

HERE = os.path.dirname(os.path.abspath(__file__))

# The generated test files and the per-run directories.  BT_SCRATCH moves the
# whole tree (tmuxdrv.py); BT_WORKROOT moves this part of it alone.
WORKROOT = os.environ.get("BT_WORKROOT", os.path.join(SCRATCH, "simwork"))

# The bytes tmux sends for each key name this simulator uses -- measured, not
# assumed (see the key-byte probe in the report).
KEY_BYTES = {
    "Up": b"\x1b[A", "Down": b"\x1b[B", "Left": b"\x1b[D", "Right": b"\x1b[C",
    "Home": b"\x1b[1~", "End": b"\x1b[4~", "PageUp": b"\x1b[5~",
    "PageDown": b"\x1b[6~", "Escape": b"\x1b", "Enter": b"\r",
    "BSpace": b"\x7f", "Tab": b"\t", "DC": b"\x1b[3~", "IC": b"\x1b[2~",
    "C-f": b"\x06", "C-b": b"\x02", "C-c": b"\x03",
}

# Where `:w PATH` writes its copies.  Both are inside the io-root and neither is
# the buffer's own path, so a write to one must leave the buffer modified and
# the original file untouched (FileOps.trp:28-37).
COPY_TARGETS = ["copy-a.txt", "copy-b.txt"]


# ---------------------------------------------------------------------------
# The test files
# ---------------------------------------------------------------------------

SMALL_SOURCE = ("git", "-C", ROOT, "show", "HEAD:examples/battallion/README.md")

WORDS = ["the", "kernel", "buffer", "rope", "cursor", "viewport", "plugin",
         "status", "line", "undo", "insert", "normal", "command", "terminal",
         "escape", "frame", "column", "offset", "supervisor", "authority"]
MULTIBYTE = ["é", "ü", "ñ", "λ", "π", "Ω", "ж", "ы", "→", "·", "«", "»", "°", "µ"]
SENTENCES = [
    "the supervisor holds the terminal and the kernel holds the state.",
    "a rope shares every node the edit did not touch with its predecessor.",
    "offsets are code units and a character outside the basic plane is two.",
    "the column is remembered across vertical motions, in goalCol.",
    "une ligne collée avec des accents: é, à, ç, ü, and a Greek λ too.",
]


def make_files(d):
    """The three test files, all generated into the io-root.

    small  a repo doc excerpt: prose, ragged lines, a few long ones
    large  180 generated lines, long ones and multibyte content
    tabs   tab-indented content, kept short enough that its rendered width fits
           an 80-column terminal (a tab expands to the next stop of eight, and
           the editor counts it as one code unit -- see the report)
    """
    os.makedirs(d, exist_ok=True)
    doc = subprocess.run(SMALL_SOURCE, capture_output=True, text=True).stdout
    lines = [l for l in doc.split("\n")][:34]
    with open(os.path.join(d, "small.txt"), "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")

    rnd = random.Random(20260801)
    big = []
    for i in range(180):
        n = rnd.choice([3, 5, 8, 12, 40, 70])
        ws = [rnd.choice(WORDS) for _ in range(n)]
        if i % 7 == 0:
            ws.insert(rnd.randrange(len(ws)), rnd.choice(MULTIBYTE) * 3)
        if i % 11 == 0:
            ws.append("«" + rnd.choice(WORDS) + "»")
        big.append("%03d %s" % (i, " ".join(ws)))
    big.append("")
    with open(os.path.join(d, "large.txt"), "w", encoding="utf-8") as f:
        f.write("\n".join(big) + "\n")

    rnd = random.Random(20260802)
    tabs = ["fun render (api, st) =", "\tcase st.mode of"]
    for i in range(120):
        depth = rnd.choice([1, 1, 2, 2, 3])
        body = " ".join(rnd.choice(WORDS) for _ in range(rnd.choice([2, 3, 4, 5])))
        # depth*8 columns of indent plus the text, kept under 60 columns
        tabs.append("\t" * depth + body[:40])
        if i % 9 == 0:
            tabs.append("\t" * depth + rnd.choice(WORDS) + "\t" + rnd.choice(WORDS))
        if i % 13 == 0:
            tabs.append("")
    tabs.append("")
    with open(os.path.join(d, "tabs.txt"), "w", encoding="utf-8") as f:
        f.write("\n".join(tabs) + "\n")


FILE_NAMES = {"small": "small.txt", "large": "large.txt", "tabs": "tabs.txt"}


# ---------------------------------------------------------------------------
# The generator
# ---------------------------------------------------------------------------
# An action is {"kind": str, "sends": [...]} where a send is one of
#   ["lit", text]      literal bytes typed
#   ["key", name]      a named key
#   ["resize", c, r]
#   ["pause", ms]      how long to wait *after* this send

NAV_NORMAL = ["h", "j", "k", "l", "0", "$", "^"]
NAV_KEYS = ["Up", "Down", "Left", "Right", "Home", "End", "PageUp", "PageDown",
            "C-f", "C-b"]
# Keys that are bound to nothing in normal mode, and to nothing as the second
# half of a pair either (VimMotions.trp:289-294, :298-321).
UNBOUND = ["z", "!", "%", "&", "+", "?", "@"]


class Generator:
    def __init__(self, seed, allow_resize, sizes, resize_heavy=False):
        self.rnd = random.Random(seed)
        self.allow_resize = allow_resize
        self.sizes = sizes
        self.resize_heavy = resize_heavy

    def delay(self):
        """Inter-key delay inside a burst: 30-150 ms."""
        return self.rnd.randint(30, 150)

    def gap(self):
        """Between bursts: longer."""
        return self.rnd.randint(200, 600)

    def word(self):
        w = self.rnd.choice(WORDS)
        if self.rnd.random() < 0.12:
            w = w + self.rnd.choice(MULTIBYTE)
        return w

    # -- helpers ------------------------------------------------------------

    def _typing(self, sends, n, tabs=0.0):
        for _ in range(n):
            if tabs and self.rnd.random() < tabs:
                sends.append(["key", "Tab"])
                sends.append(["pause", self.delay()])
            w = self.word()
            for ch in w:
                sends.append(["lit", ch])
                sends.append(["pause", self.delay()])
            if self.rnd.random() < 0.15:
                sends.append(["key", "Enter"])
            else:
                sends.append(["lit", " "])
            sends.append(["pause", self.delay()])

    def pattern(self, st):
        """A search pattern: usually a run of characters taken from the buffer,
        so that it hits; sometimes one that cannot hit."""
        r = self.rnd.random()
        if r < 0.18:
            return "zq" + self.rnd.choice(WORDS)[:3]          # a miss
        for _ in range(12):
            l = self.rnd.randrange(len(st.lines))
            line = M.from_u16(st.lines[l])
            body = "".join(c for c in line if c.isalnum())
            if len(body) >= 3:
                i = self.rnd.randrange(max(1, len(body) - 3))
                return body[i:i + self.rnd.randint(2, 5)]
        return self.rnd.choice(WORDS)[:4]

    # -- the actions --------------------------------------------------------

    def next(self, st):
        """The next action, given the model state (mode-aware, as a user is)."""
        r = self.rnd.random()
        mode = st.mode

        if mode == M.INSERT:
            # In insert mode a user types, navigates with the arrows, deletes
            # backwards, or leaves.
            if r < 0.36:
                s = []
                self._typing(s, self.rnd.randint(1, 4), tabs=0.12)
                s.append(["pause", self.gap()])
                return {"kind": "type", "sends": s}
            if r < 0.50:
                s = []
                for _ in range(self.rnd.randint(1, 5)):
                    s.append(["key", self.rnd.choice(
                        ["Up", "Down", "Left", "Right", "Home", "End"])])
                    s.append(["pause", self.delay()])
                s.append(["pause", self.gap()])
                return {"kind": "nav_insert", "sends": s}
            if r < 0.60:
                s = []
                for _ in range(self.rnd.randint(1, 5)):
                    s.append(["key", "BSpace"])
                    s.append(["pause", self.delay()])
                s.append(["pause", self.gap()])
                return {"kind": "backspace_run", "sends": s}
            if r < 0.65:
                s = [["lit", self.rnd.choice(SENTENCES)], ["pause", self.gap()]]
                return {"kind": "paste", "sends": s}
            if r < 0.70:
                # Tabs typed into the buffer, then out of insert mode.
                s = []
                for _ in range(self.rnd.randint(1, 3)):
                    s.append(["key", "Tab"])
                    s.append(["pause", self.delay()])
                self._typing(s, 1)
                s.append(["key", "Escape"])
                s.append(["pause", 250])
                return {"kind": "type_tab", "sends": s}
            if r < 0.75:
                # Esc immediately followed by a motion: the escape-timer window.
                s = [["key", "Escape"], ["pause", self.rnd.randint(5, 35)],
                     ["lit", self.rnd.choice(["h", "j", "k", "l"])],
                     ["pause", self.gap()]]
                return {"kind": "esc_motion", "sends": s}
            s = [["key", "Escape"], ["pause", 250]]
            return {"kind": "esc", "sends": s}

        if mode == M.HELP:
            # The page is up; any key at all puts it away.
            s = [["lit", self.rnd.choice(["j", "x", " "])], ["pause", 300]]
            return {"kind": "help_close", "sends": s}

        # NORMAL mode.
        if self.resize_heavy and self.rnd.random() < 0.20:
            c, rr = self.rnd.choice(self.sizes)
            return {"kind": "resize", "sends": [["resize", c, rr], ["pause", 500]]}

        if r < 0.30:
            s = []
            for _ in range(self.rnd.randint(2, 6)):
                if self.rnd.random() < 0.55:
                    s.append(["lit", self.rnd.choice(NAV_NORMAL)])
                else:
                    s.append(["key", self.rnd.choice(NAV_KEYS)])
                s.append(["pause", self.delay()])
            s.append(["pause", self.gap()])
            return {"kind": "nav", "sends": s}
        if r < 0.36:
            # A walk that exercises the goal column: a horizontal motion sets it,
            # then vertical ones over ragged lines aim for it.
            s = [["lit", self.rnd.choice(["$", "0", "^"])], ["pause", self.delay()]]
            if self.rnd.random() < 0.5:
                for _ in range(self.rnd.randint(3, 12)):
                    s.append(["lit", "l"])
                    s.append(["pause", 40])
            for _ in range(self.rnd.randint(3, 10)):
                s.append(["key", self.rnd.choice(["Down", "Up"])]
                         if self.rnd.random() < 0.5
                         else ["lit", self.rnd.choice(["j", "k"])])
                s.append(["pause", self.delay()])
            s.append(["pause", self.gap()])
            return {"kind": "goal_walk", "sends": s}
        if r < 0.41:
            s = []
            for _ in range(self.rnd.randint(2, 8)):
                s.append(["lit", self.rnd.choice(["w", "b"])])
                s.append(["pause", self.delay()])
            s.append(["pause", self.gap()])
            return {"kind": "word_motion", "sends": s}
        if r < 0.47:
            # A search, sometimes followed by an n/N chain.
            pat = self.pattern(st)
            s = [["lit", "/"], ["pause", self.delay()],
                 ["lit", pat], ["pause", self.delay()],
                 ["key", "Enter"], ["pause", 300]]
            for _ in range(self.rnd.randint(0, 6)):
                s.append(["lit", self.rnd.choice(["n", "n", "n", "N"])])
                s.append(["pause", self.delay()])
            s.append(["pause", self.gap()])
            return {"kind": "search", "sends": s}
        if r < 0.50:
            # n/N with whatever pattern is remembered -- none, at the start.
            s = []
            for _ in range(self.rnd.randint(2, 8)):
                s.append(["lit", self.rnd.choice(["n", "N"])])
                s.append(["pause", self.delay()])
            s.append(["pause", self.gap()])
            return {"kind": "search_repeat", "sends": s}
        if r < 0.55:
            which = self.rnd.random()
            if which < 0.3:
                s = [["lit", "gg"], ["pause", 300]]
                kind = "goto_gg"
            elif which < 0.55:
                s = [["lit", "G"], ["pause", 300]]
                kind = "goto_G"
            elif which < 0.8:
                n = self.rnd.randint(1, len(st.lines) + 25)
                s = [["lit", ":"], ["pause", self.delay()],
                     ["lit", str(n)], ["pause", self.delay()],
                     ["key", "Enter"], ["pause", 300]]
                kind = "goto_N"
            else:
                s = [["lit", ":$"], ["pause", self.delay()],
                     ["key", "Enter"], ["pause", 300]]
                kind = "goto_last"
            return {"kind": kind, "sends": s}
        if r < 0.62:
            # dd / yy / p / P through the one register.
            s = []
            for _ in range(self.rnd.randint(1, 4)):
                s.append(["lit", self.rnd.choice(["dd", "yy", "p", "P", "yy", "dd"])])
                s.append(["pause", self.delay()])
            s.append(["pause", self.gap()])
            return {"kind": "linewise", "sends": s}
        if r < 0.68:
            s = [["lit", "i"], ["pause", self.delay()]]
            self._typing(s, self.rnd.randint(1, 5), tabs=0.10)
            s.append(["pause", self.gap()])
            return {"kind": "insert_burst", "sends": s}
        if r < 0.72:
            s = []
            for _ in range(self.rnd.randint(1, 4)):
                s.append(["lit", "x"])
                s.append(["pause", self.delay()])
            s.append(["pause", self.gap()])
            return {"kind": "delete_x", "sends": s}
        if r < 0.75:
            s = [["lit", "o"], ["pause", self.delay()]]
            self._typing(s, self.rnd.randint(1, 3), tabs=0.15)
            s.append(["key", "Escape"])
            s.append(["pause", 250])
            return {"kind": "open_line", "sends": s}
        if r < 0.80:
            s = []
            for _ in range(self.rnd.randint(1, 6)):
                s.append(["lit", "u"])
                s.append(["pause", self.delay()])
            s.append(["pause", self.gap()])
            return {"kind": "undo_run", "sends": s}
        if r < 0.84:
            s = [["lit", ":w"], ["pause", self.delay()],
                 ["key", "Enter"], ["pause", 350]]
            return {"kind": "save", "sends": s}
        if r < 0.87:
            t = self.rnd.choice(COPY_TARGETS)
            s = [["lit", ":w "], ["pause", self.delay()],
                 ["lit", t], ["pause", self.delay()],
                 ["key", "Enter"], ["pause", 400]]
            return {"kind": "save_as", "sends": s, "target": t}
        if r < 0.90:
            # A prefix abandoned: `g` or `d` or `y` then a key that completes no
            # pair, or Esc.  The prefix must be gone either way.
            pre = self.rnd.choice(["g", "d", "y"])
            if self.rnd.random() < 0.5:
                s = [["lit", pre], ["pause", 250],
                     ["lit", self.rnd.choice(UNBOUND)], ["pause", 300]]
            else:
                s = [["lit", pre], ["pause", 250],
                     ["key", "Escape"], ["pause", 350]]
            return {"kind": "prefix_abandon", "sends": s}
        if r < 0.93:
            # The help page, opened and dismissed -- sometimes with a key that
            # would otherwise have done something.
            cmd = self.rnd.choice([":help", ":h"])
            s = [["lit", cmd], ["pause", self.delay()],
                 ["key", "Enter"], ["pause", 400]]
            if self.rnd.random() < 0.6:
                s.append(["lit", self.rnd.choice(["x", "j", "G", "u", " "])])
                s.append(["pause", 350])
            return {"kind": "help", "sends": s}
        if r < 0.945:
            return {"kind": "idle",
                    "sends": [["pause", self.rnd.randint(1000, 2000)]]}
        if r < 0.96:
            which = self.rnd.random()
            if which < 0.35:
                s = [["key", "IC"], ["pause", self.gap()]]
                return {"kind": "ignored_key", "sends": s}
            if which < 0.7:
                s = [["lit", ":"], ["pause", self.delay()],
                     ["lit", "z" + self.rnd.choice(WORDS)[:3]],
                     ["pause", self.delay()],
                     ["key", "Enter"], ["pause", 300]]
                return {"kind": "bad_command", "sends": s}
            s = [["lit", ":"], ["pause", self.delay()], ["lit", "w"],
                 ["pause", self.delay()], ["key", "BSpace"],
                 ["pause", self.delay()], ["key", "BSpace"],
                 ["pause", 300]]
            return {"kind": "command_backspace", "sends": s}
        if r < 0.98 and st.is_dirty():
            s = [["lit", self.rnd.choice(["q", ":q"])], ["pause", self.delay()]]
            if s[0][1] == ":q":
                s.append(["key", "Enter"])
                s.append(["pause", 350])
            else:
                s.append(["pause", 350])
            return {"kind": "dirty_quit", "sends": s}
        if r < 0.995 and self.allow_resize:
            c, rr = self.rnd.choice(self.sizes)
            return {"kind": "resize", "sends": [["resize", c, rr], ["pause", 400]]}
        s = []
        for _ in range(self.rnd.randint(2, 5)):
            s.append(["lit", self.rnd.choice(NAV_NORMAL)])
            s.append(["pause", self.delay()])
        s.append(["pause", self.gap()])
        return {"kind": "nav", "sends": s}


# ---------------------------------------------------------------------------
# The checker
# ---------------------------------------------------------------------------

def parse_status(row):
    """The status row's fields, loosely: (name, dirty, mode, line, col, message)
    or a shorter tuple when the row is a command line or the help note."""
    if row.startswith(":") or row.startswith("/"):
        return ("cmd", row[0], row[1:])
    if row == M.HELP_MESSAGE:
        return ("help",)
    parts = row.split("  ")
    head = parts[0]
    dirty = head.endswith(" [+]")
    name = head[:-4] if dirty else head
    mode = parts[1] if len(parts) > 1 else ""
    pos = parts[2] if len(parts) > 2 else ""
    msg = "  ".join(parts[3:]) if len(parts) > 3 else ""
    return ("st", name, dirty, mode, pos, msg)


class Diverged(Exception):
    def __init__(self, kind, detail):
        super().__init__(kind)
        self.kind = kind
        self.detail = detail


class Run:
    def __init__(self, seed, filename, cols, rows, actions, allow_resize,
                 outdir, sizes, tag, resize_heavy=False):
        self.seed, self.filename = seed, filename
        self.cols, self.rows = cols, rows
        self.n_actions = actions
        self.allow_resize = allow_resize
        self.outdir = outdir
        self.sizes = sizes
        self.tag = tag
        self.resize_heavy = resize_heavy
        self.io_root = os.path.join(outdir, "io")
        self.latencies = []
        self.saves_checked = 0
        self.files_compared = 0
        self.overflow_frames = 0
        self.tab_rows = 0
        self.help_frames = 0
        self.dirty_checks = 0
        self.kinds = collections.Counter()

    # -- setup --------------------------------------------------------------

    def prepare(self):
        if os.path.isdir(self.io_root):
            shutil.rmtree(self.io_root)
        os.makedirs(self.io_root)
        src = os.path.join(WORKROOT, "files", self.filename)
        shutil.copy(src, os.path.join(self.io_root, self.filename))
        with open(os.path.join(self.io_root, self.filename), encoding="utf-8") as f:
            text = f.read()
        self.model = M.Session(M.buf_from_text(text), self.filename,
                               self.cols, self.rows)
        self.pane = Pane("btsim-%s-%d" % (self.tag, os.getpid()), self.io_root,
                         self.filename, self.cols, self.rows,
                         errlog=os.path.join(self.outdir, "stderr.log"))

    # -- one action ---------------------------------------------------------

    def do_action(self, act, check=True):
        """Play one action out, then wait for the pane to settle on the state the
        model says it should reach.

        The trailing inter-burst pause is spent *polling* rather than sleeping,
        so the latency recorded is the real keystroke-to-stable-frame time and
        not the generator's own think time; whatever is left of the pause is
        slept afterwards, which keeps the session's timing realistic."""
        sends = act["sends"]
        reals = [i for i, s in enumerate(sends) if s[0] != "pause"]
        last_real = reals[-1] if reals else -1
        t_last = None
        trailing = 0
        for i, s in enumerate(sends):
            if s[0] == "lit":
                self.pane.send_literal(s[1])
                self.model.feed(s[1].encode("utf-8"))
            elif s[0] == "key":
                self.pane.send_key(s[1])
                self.model.feed(KEY_BYTES[s[1]])
            elif s[0] == "resize":
                self.pane.resize(s[1], s[2])
                self.model.resize(s[1], s[2])
            elif s[0] == "pause":
                if i > last_real:
                    trailing += s[1]
                    continue
                time.sleep(s[1] / 1000.0)
                if s[1] >= 100:          # escapeMs is 50: the timer has fired
                    self.model.flush_esc()
            if i == last_real:
                t_last = time.time()
        self.model.flush_esc()
        want_rows, cur_row, cur_col, over, _ = self.model.screen()
        want = None if over else (want_rows, cur_col, cur_row)
        t0 = t_last or time.time()
        cap, matched = wait_stable(self.pane, want=want, t0=t0,
                                   min_ms=max(150, trailing // 3))
        if matched is not None and t_last is not None:
            self.latencies.append(matched)
        left = trailing / 1000.0 - (time.time() - t0)
        if left > 0:
            time.sleep(left)
        if check:
            self.check(act, cap)
        return cap

    def check(self, act, cap):
        rows, cx, cy, dead, status = cap
        st = self.model.st
        if self.model.quit:
            return
        if dead:
            raise Diverged("editor exited", {
                "exit_status": status,
                "screen": rows,
                "stderr_tail": tail_file(os.path.join(self.outdir, "stderr.log")),
            })
        want_rows, cur_row, cur_col, over, tabs = self.model.screen()
        if over:
            # The terminal clipped a row this model said fitted, so it is drawing
            # some character wider than the one column the model gives it. Not
            # modelled; counted and skipped (see the report).
            self.overflow_frames += 1
            return
        if st.mode == M.HELP:
            self.help_frames += 1
        self.tab_rows += tabs
        body = st.text_rows()
        for i in range(body):
            got = rows[i].rstrip() if i < len(rows) else ""
            exp = want_rows[i]
            if got != exp:
                raise Diverged("body row %d" % i, {
                    "row": i, "expected": exp, "got": got,
                    "screen": rows, "model": want_rows})
        if st.has_status():
            got = rows[body].rstrip() if body < len(rows) else ""
            exp = want_rows[body]
            self.dirty_checks += 1
            if got != exp:
                raise Diverged("status row", {
                    "expected": exp, "got": got,
                    "expected_fields": parse_status(exp),
                    "got_fields": parse_status(got),
                    "screen": rows, "model": want_rows})
        # The frame ends by positioning the cursor (Screen.trp:97).  Both the
        # buffer column and the command-line column are clamped to the screen by
        # the kernel now (Editor.trp:117-125), so the emitted column is asserted
        # as it stands.
        if (cx, cy) != (cur_col, cur_row):
            raise Diverged("cursor", {
                "expected": [cur_col, cur_row], "got": [cx, cy],
                "screen": rows, "model": want_rows})

    def check_files(self):
        """Every file the session can have written, byte for byte, plus the set
        of names: a `:w PATH` must produce the copy and leave the original as it
        was."""
        want = {p: self.model.file_bytes(p) for p in self.model.disk}
        have = sorted(os.listdir(self.io_root))
        if have != sorted(want):
            raise Diverged("files on disk", {
                "path": self.io_root, "expected_names": sorted(want),
                "got_names": have})
        for p in sorted(want):
            with open(os.path.join(self.io_root, p), "rb") as f:
                on_disk = f.read()
            if on_disk != want[p]:
                raise Diverged("file on disk", {
                    "path": p, "disk_len": len(on_disk),
                    "model_len": len(want[p]),
                    "disk_head": on_disk[:400].decode("utf-8", "replace"),
                    "model_head": want[p][:400].decode("utf-8", "replace")})
            self.files_compared += 1
        self.saves_checked += 1

    # -- the run ------------------------------------------------------------

    def run(self):
        self.prepare()
        gen = Generator(self.seed, self.allow_resize, self.sizes,
                        self.resize_heavy)
        logp = os.path.join(self.outdir, "replay.jsonl")
        log = open(logp, "w", encoding="utf-8")
        log.write(json.dumps({"header": {
            "seed": self.seed, "file": self.filename, "cols": self.cols,
            "rows": self.rows, "actions": self.n_actions,
            "resize": self.allow_resize, "commit": "e4024e39",
            "started": time.strftime("%Y-%m-%dT%H:%M:%S")}}) + "\n")
        log.flush()
        # the first frame
        cap = wait_started(self.pane)
        try:
            self.check({"kind": "startup"}, cap)
            self.check_files()
        except Diverged as d:
            log.close()
            return self.report_divergence(0, {"kind": "startup"}, d, logp)

        actions = []
        for i in range(self.n_actions):
            act = gen.next(self.model.st)
            actions.append(act)
            self.kinds[act["kind"]] += 1
            log.write(json.dumps({"i": i, "t": round(time.time(), 3),
                                  "kind": act["kind"], "sends": act["sends"]}) + "\n")
            log.flush()
            try:
                self.do_action(act)
                if act["kind"] in ("save", "save_as") or i % 50 == 49:
                    self.check_files()
            except Diverged as d:
                log.close()
                return self.report_divergence(i, act, d, logp, actions)
            if self.model.quit:
                print("  [%s] session ended at action %d (%s)"
                      % (self.tag, i, act["kind"]))
                break
        if not self.model.quit:
            try:
                self.check_files()
            except Diverged as d:
                log.close()
                return self.report_divergence(len(actions) - 1, actions[-1], d,
                                              logp, actions)
        log.close()
        self.pane.kill()
        return self.result(len(actions), 0, logp)

    def result(self, actions, divergences, logp, **extra):
        r = {"seed": self.seed, "file": self.filename,
             "size": "%dx%d" % (self.cols, self.rows),
             "actions": actions, "divergences": divergences,
             "saves": self.saves_checked, "files_compared": self.files_compared,
             "latencies": self.latencies, "tab_rows": self.tab_rows,
             "overflow_frames": self.overflow_frames,
             "help_frames": self.help_frames,
             "dirty_checks": self.dirty_checks,
             "kinds": dict(self.kinds), "log": logp}
        r.update(extra)
        return r

    def report_divergence(self, i, act, d, logp, actions=None):
        self.pane.kill()
        dump = os.path.join(self.outdir, "divergence.json")
        with open(dump, "w", encoding="utf-8") as f:
            json.dump({"action_index": i, "action": act, "kind": d.kind,
                       "detail": d.detail}, f, indent=2, ensure_ascii=False)
        print("  [%s] DIVERGENCE at action %d (%s): %s"
              % (self.tag, i, act.get("kind"), d.kind))
        det = d.detail
        for k in ("expected", "got", "row"):
            if k in det:
                print("      %-9s %r" % (k, det[k]))
        minimal = None
        if actions:
            minimal = self.minimize(actions[:i + 1])
            print("      minimized to %s actions" % minimal)
        return self.result(i + 1, 1, logp, dump=dump, diverge_kind=d.kind,
                           minimal=minimal)

    # -- minimization -------------------------------------------------------

    def replay_prefix(self, actions):
        """Replay actions[0:len] in a fresh session; True when it diverges."""
        sub = Run(self.seed, self.filename, self.cols, self.rows, len(actions),
                  self.allow_resize, self.outdir + "/min", self.sizes,
                  self.tag + "m")
        os.makedirs(sub.outdir, exist_ok=True)
        sub.prepare()
        try:
            cap = wait_started(sub.pane)
            sub.check({"kind": "startup"}, cap)
            sub.check_files()
            for a in actions:
                sub.do_action(a)
                if a["kind"] in ("save", "save_as"):
                    sub.check_files()
                if sub.model.quit:
                    return False
            sub.check_files()
            return False
        except Diverged:
            return True
        finally:
            sub.pane.kill()

    def minimize(self, actions):
        if not self.replay_prefix(actions):
            return {"reproducible": False, "prefix": len(actions)}
        lo, hi = 1, len(actions)
        while lo < hi:
            mid = (lo + hi) // 2
            if self.replay_prefix(actions[:mid]):
                hi = mid
            else:
                lo = mid + 1
        with open(os.path.join(self.outdir, "minimal.jsonl"), "w",
                  encoding="utf-8") as f:
            for a in actions[:lo]:
                f.write(json.dumps(a, ensure_ascii=False) + "\n")
        return {"reproducible": True, "prefix": lo,
                "file": os.path.join(self.outdir, "minimal.jsonl")}


def tail_file(p, n=1200):
    try:
        with open(p, encoding="utf-8", errors="replace") as f:
            return f.read()[-n:]
    except OSError:
        return ""


# ---------------------------------------------------------------------------

def summarize(res):
    lat = res["latencies"]
    s = ""
    if lat:
        lat = sorted(lat)
        p = lambda q: lat[min(len(lat) - 1, int(q * len(lat)))]
        s = "  p50=%.0fms p95=%.0fms max=%.0fms n=%d" % (
            statistics.median(lat), p(0.95), lat[-1], len(lat))
    print("  [%s] %d actions, %d divergences, %d file checks (%d comparisons)%s"
          % (res.get("file"), res["actions"], res["divergences"], res["saves"],
             res["files_compared"], s))
    return res


def cmd_run(a):
    os.makedirs(WORKROOT, exist_ok=True)
    files = os.path.join(WORKROOT, "files")
    # Per file, not per directory: probe_features.py writes its own file into
    # this directory, and a guard on the directory alone would take its presence
    # for the whole set being there.
    if any(not os.path.exists(os.path.join(files, n)) for n in FILE_NAMES.values()):
        make_files(files)
    name = FILE_NAMES[a.file]
    tag = "s%d%s%dx%d%s" % (a.seed, a.file[0], a.cols, a.rows,
                            "R" if a.resize_heavy else ("r" if a.resize else ""))
    outdir = os.path.join(WORKROOT, "runs", tag)
    if os.path.isdir(outdir):
        shutil.rmtree(outdir)
    os.makedirs(outdir)
    sizes = [(80, 24), (100, 30), (60, 18), (40, 12)]
    r = Run(a.seed, name, a.cols, a.rows, a.actions, a.resize or a.resize_heavy,
            outdir, sizes, tag, a.resize_heavy)
    t0 = time.time()
    res = r.run()
    res["wall"] = round(time.time() - t0, 1)
    summarize(res)
    with open(os.path.join(outdir, "result.json"), "w") as f:
        json.dump({k: v for k, v in res.items() if k != "latencies"}, f, indent=2)
    with open(os.path.join(outdir, "latencies.json"), "w") as f:
        json.dump(res["latencies"], f)
    return 1 if res["divergences"] else 0


def cmd_replay(a):
    """Replay a log (optionally only its first N actions) and check as it goes."""
    recs = [json.loads(l) for l in open(a.log, encoding="utf-8") if l.strip()]
    header = recs[0]["header"]
    acts = [r for r in recs[1:] if "sends" in r]
    if a.upto is not None:
        acts = acts[:a.upto]
    name = header["file"]
    outdir = os.path.join(WORKROOT, "replay")
    if os.path.isdir(outdir):
        shutil.rmtree(outdir)
    os.makedirs(outdir)
    r = Run(header["seed"], name, header["cols"], header["rows"], len(acts),
            header["resize"], outdir, [], "rp")
    r.prepare()
    cap = wait_started(r.pane)
    i = -1
    try:
        r.check({"kind": "startup"}, cap)
        r.check_files()
        for i, act in enumerate(acts):
            r.do_action(act)
            if act["kind"] in ("save", "save_as"):
                r.check_files()
            if r.model.quit:
                print("session ended at action %d" % i)
                break
        print("replay clean: %d actions" % len(acts))
        rc = 0
    except Diverged as d:
        print("DIVERGENCE at action %d: %s" % (i, d.kind))
        print(json.dumps(d.detail, indent=2, ensure_ascii=False)[:3000])
        rc = 1
    r.pane.kill()
    return rc


def cmd_genfiles(a):
    make_files(a.dir)
    print("wrote small.txt, large.txt and tabs.txt into %s" % a.dir)
    return 0


def main():
    p = argparse.ArgumentParser()
    sub = p.add_subparsers(dest="cmd", required=True)
    q = sub.add_parser("run")
    q.add_argument("--seed", type=int, required=True)
    q.add_argument("--file", choices=sorted(FILE_NAMES), default="small")
    q.add_argument("--actions", type=int, default=400)
    q.add_argument("--cols", type=int, default=80)
    q.add_argument("--rows", type=int, default=24)
    q.add_argument("--resize", action="store_true")
    q.add_argument("--resize-heavy", action="store_true",
                   help="a resize in one normal-mode action out of five")
    q.set_defaults(fn=cmd_run)
    q = sub.add_parser("replay")
    q.add_argument("log")
    q.add_argument("--upto", type=int, default=None)
    q.set_defaults(fn=cmd_replay)
    q = sub.add_parser("gen-files")
    q.add_argument("dir")
    q.set_defaults(fn=cmd_genfiles)
    a = p.parse_args()
    sys.exit(a.fn(a))


if __name__ == "__main__":
    main()
