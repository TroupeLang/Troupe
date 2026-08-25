#!/usr/bin/env python3
"""probe_features.py -- drive the real editor through every rule the model was
recalibrated for, and check each one against the model.

The campaign's generator reaches these behaviours by chance; this reaches every
one of them on purpose, on a file whose content is chosen so the answers are
easy to read.  A rule that is wrong in the model shows up here as a divergence
with the action that produced it, before a 400-action run buries it.

  python3 probe_features.py
"""

import os
import shutil
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import btmodel as M
from simulate import Run, Diverged, WORKROOT
from tmuxdrv import wait_started

FILE = "probe.txt"

# Ragged lines, so the goal column has something to be clamped by; a word that
# occurs on the first and the last line, so a search wraps in both directions;
# leading blanks for `^`; tabs for the row rendering.
CONTENT = "\n".join([
    "alpha beta gamma",                 # 1
    "x",                                # 2
    "   indented line with beta",       # 3
    "",                                 # 4
    "a much longer line than the ones around it, with beta in it",  # 5
    "y",                                # 6
    "\tone\ttwo",                       # 7
    "\t\tdeep",                         # 8
    "zeta",                             # 9
    "last line has beta too",           # 10
]) + "\n"


def K(name, ms=300):
    return ["key", name], ["pause", ms]


def act(kind, *sends):
    out = []
    for s in sends:
        out.append(s)
    return {"kind": kind, "sends": out}


def lit(s, ms=250):
    return [["lit", s], ["pause", ms]]


def key(n, ms=300):
    return [["key", n], ["pause", ms]]


def A(kind, *groups):
    sends = []
    for g in groups:
        sends.extend(g)
    return {"kind": kind, "sends": sends}


ACTIONS = [
    # -- the startup hint is cleared by the first key -----------------------
    A("hint_cleared", lit("l")),

    # -- goal column: $ sets goalEnd, j walks the ends ----------------------
    A("goal_dollar", lit("gg"), lit("$"), lit("j"), lit("j"), lit("j"), lit("j")),
    # a column set by l, kept across a short line and restored on a long one
    A("goal_column", lit("gg"), lit("l"), lit("l"), lit("l"), lit("l"), lit("l"),
      key("Down"), key("Down"), key("Down"), key("Down"), key("Up"), key("Up")),
    # ^ finds the first non-blank; 0 does not
    A("caret", lit(":3"), key("Enter"), lit("^"), lit("0"), lit("^")),

    # -- the linewise jumps -------------------------------------------------
    A("goto_G", lit("G")),
    A("goto_gg", lit("gg")),
    A("goto_N", lit(":5"), key("Enter")),
    A("goto_past_end", lit(":999"), key("Enter")),
    A("goto_zero", lit(":0"), key("Enter")),
    A("goto_last", lit(":$"), key("Enter")),

    # -- the pending prefix -------------------------------------------------
    A("prefix_shows", lit("g")),                       # status shows "(g)"
    A("prefix_unbound", lit("z")),                     # gz: nothing, prefix gone
    A("prefix_esc", lit("d"), key("Escape", 400)),
    A("prefix_y_then_g", lit("y"), lit("g")),          # yg: nothing
    A("prefix_arrow", lit("g"), key("Down", 400)),     # a non-print completion

    # -- word motions -------------------------------------------------------
    A("words_forward", lit("gg"), lit("w"), lit("w"), lit("w"), lit("w"), lit("w")),
    A("words_backward", lit("b"), lit("b"), lit("b"), lit("b")),

    # -- search -------------------------------------------------------------
    A("search_hit", lit("gg"), lit("/"), lit("beta"), key("Enter", 400)),
    A("search_next", lit("n"), lit("n")),
    A("search_wrap_down", lit("n"), lit("n")),         # past the last hit
    A("search_wrap_up", lit("N"), lit("N"), lit("N"), lit("N"), lit("N")),
    A("search_miss", lit("/"), lit("qqqq"), key("Enter", 400)),
    A("search_empty_repeats", lit("/"), key("Enter", 400)),
    A("search_esc", lit("/"), lit("zet"), key("Escape", 400)),
    A("search_backspace_out", lit("/"), key("BSpace"), key("BSpace", 400)),

    # -- the register -------------------------------------------------------
    A("yank", lit("gg"), lit("yy")),                   # status: line yanked
    A("put_below", lit("p")),
    A("put_above", lit("P")),
    A("delete_line", lit("dd")),                       # yanks what it deletes
    A("put_the_deleted", lit("p")),
    A("undo_keeps_register", lit("u"), lit("u"), lit("p")),
    A("dd_last_line", lit("G"), lit("dd")),
    A("put_no_register_after_none", lit("gg"), lit("P")),

    # -- insert mode, tabs --------------------------------------------------
    A("insert_tab", lit("gg"), lit("i"), key("Tab"), lit("ab"), key("Tab"),
      lit("cd"), key("Escape", 400)),
    A("open_line_tab", lit("o"), key("Tab"), lit("zz"), key("Escape", 400)),

    # -- help ---------------------------------------------------------------
    A("help_open", lit(":help"), key("Enter", 500)),
    A("help_any_key", lit("x", 400)),                  # dismisses, does not delete
    A("help_short", lit(":h"), key("Enter", 500)),
    A("help_close_G", lit("G", 400)),

    # -- saving -------------------------------------------------------------
    A("save", lit(":w"), key("Enter", 500)),           # clean afterwards
    A("undo_across_save", lit("u"), lit("u")),         # modified again
    A("save_as", lit(":w copy-a.txt"), key("Enter", 600)),   # still modified
    A("save_as_second", lit(":w copy-b.txt"), key("Enter", 600)),
    A("save_own", lit(":w"), key("Enter", 500)),
    A("quit_clean_refused_not", lit("l")),             # just settle

    # -- commands that are not commands -------------------------------------
    A("bad_command", lit(":"), lit("zork"), key("Enter", 400)),
    A("empty_command", lit(":"), key("Enter", 400)),
    A("q_with_argument", lit(":q now"), key("Enter", 400)),
    A("command_backspace_out", lit(":"), lit("w"), key("BSpace"),
      key("BSpace", 400)),

    # -- resizing -----------------------------------------------------------
    # The goal column survives a resize (Api.trp:443-447): `$` sets goalEnd, the
    # window changes, and the vertical motion after it still walks the ends.
    A("resize_goal", lit("gg"), lit("$"), [["resize", 40, 10], ["pause", 500]],
      lit("j"), lit("j")),
    A("resize_narrow", [["resize", 24, 8], ["pause", 500]], lit("G"), lit("$")),
    # A command line longer than the terminal: the kernel clamps the cursor to
    # the last column now (Editor.trp:117-125), which the first campaign
    # recorded as an off-screen column instead.
    A("cmdline_clamped", lit(":"), lit("wwwwwwwwwwwwwwwwwwwwwwwwwwwwww", 400)),
    A("cmdline_cancel", key("Escape", 400)),
    A("resize_back", [["resize", 60, 14], ["pause", 500]]),

    # -- quitting a modified buffer is refused ------------------------------
    A("make_dirty", lit("gg"), lit("i"), lit("Z"), key("Escape", 400)),
    A("q_refused", lit("q", 600)),
    A("colon_q_refused", lit(":q"), key("Enter", 600)),
    A("still_alive", lit("l")),

    # -- astral characters: two code units, edited as one -------------------
    # `deleteChar` and `backspace` read the surrogate pair and remove both
    # halves (Api.trp:77-81), while every column counts units, so `$` on a line
    # of three astral characters is column 7 one-based.
    A("astral_type", lit("gg"), lit("i"), lit("\U0001d6cc\U0001d6cc\U0001d6cc"),
      key("Escape", 400)),
    A("astral_end", lit("$")),
    A("astral_backspace", lit("i"), key("BSpace"), key("BSpace", 500)),
    A("astral_delete_key", key("Escape"), lit("0"), key("DC", 400)),
    A("astral_x", lit("0"), lit("x", 400)),

    # -- a terminal with one text row, and one with no status line ----------
    A("two_rows", [["resize", 40, 2], ["pause", 600]], lit("j"), lit("j")),
    A("two_rows_command", lit(":"), lit("5"), key("Enter", 400)),
    A("one_row", [["resize", 40, 1], ["pause", 600]], lit("j")),
    A("one_row_command", lit(":"), lit("3"), key("Enter", 500)),
    A("rows_back", [["resize", 60, 14], ["pause", 600]]),
]


def main():
    files = os.path.join(WORKROOT, "files")
    os.makedirs(files, exist_ok=True)
    with open(os.path.join(files, FILE), "w", encoding="utf-8") as f:
        f.write(CONTENT)
    outdir = os.path.join(WORKROOT, "probe")
    shutil.rmtree(outdir, ignore_errors=True)
    os.makedirs(outdir)
    r = Run(0, FILE, 60, 14, len(ACTIONS), False, outdir, [], "probe")
    r.prepare()
    cap = wait_started(r.pane)
    rc = 0
    try:
        r.check({"kind": "startup"}, cap)
        r.check_files()
        print("  startup ok: %r" % cap[0][r.model.st.text_rows()].rstrip())
        for i, a in enumerate(ACTIONS):
            r.do_action(a)
            if any(s[0] == "resize" for s in a["sends"]):
                # tmux may refuse a size; the model would then be measuring a
                # terminal that is not there, so the disagreement is caught here
                # rather than showing up as a divergence in the frame.
                from tmuxdrv import tmux
                got = tmux("display-message", "-p", "-t", r.pane.session,
                           "#{pane_width}x#{pane_height}").strip()
                want = "%dx%d" % (r.pane.cols, r.pane.rows)
                if got != want:
                    print("  SKIP: tmux gave %s for a requested %s" % (got, want))
                    return 2
            if a["kind"].startswith("save"):
                r.check_files()
            st = r.model.st
            print("  %2d %-26s %s" % (i, a["kind"],
                                      M.from_u16(M.status_row(st))))
            if r.model.quit:
                print("  session ended at %s" % a["kind"])
                break
        else:
            r.check_files()
            print("PASS: %d probe actions, no divergence" % len(ACTIONS))
    except Diverged as d:
        print("DIVERGENCE at action %d (%s): %s"
              % (i, ACTIONS[i]["kind"], d.kind))
        import json
        print(json.dumps(d.detail, indent=2, ensure_ascii=False)[:4000])
        rc = 1
    r.pane.kill()
    print("tab rows asserted: %d   help frames: %d   overflow frames: %d"
          % (r.tab_rows, r.help_frames, r.overflow_frames))
    print("status rows asserted: %d   file comparisons: %d"
          % (r.dirty_checks, r.files_compared))
    return rc


if __name__ == "__main__":
    sys.exit(main())
