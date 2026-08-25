#!/usr/bin/env python3
"""probe_edges.py -- hand-written scenarios for the corners the seeded generator
does not reach, checked against the same shadow model.

Each scenario runs in its own tmux pane and is checked after every action; a
scenario that diverges is reported and the probe carries on with the next one.

  python3 probe_edges.py [name ...]
"""

import json
import os
import shutil
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import btmodel as M
from simulate import Run, Diverged, WORKROOT, make_files
from tmuxdrv import wait_started


def L(t):
    return ["lit", t]


def K(n):
    return ["key", n]


def P(ms=250):
    return ["pause", ms]


def act(kind, *sends):
    return {"kind": kind, "sends": list(sends) + [P(300)]}


# ---------------------------------------------------------------------------
# The scenarios.  Each is (name, filename, cols, rows, [actions], note).
# ---------------------------------------------------------------------------

def scenarios():
    s = []

    # A command line longer than the terminal is wide.  Editor.trp:95 gives the
    # command-mode cursor as (h, size cmd + 1) with no clamp, while the normal-
    # mode branch two lines below clamps to cols - 1 (Editor.trp:97).
    s.append(("long-command-narrow", "small.txt", 12, 10, [
        act("open_cmd", L(":")),
        act("type_cmd", L("wqabcdefghijkl")),
        act("cancel", K("Escape"), P(300)),
    ], "command-mode cursor past the terminal's right edge"))

    # The minimal form of the same: a 12-column terminal and a command line one
    # character longer than fits.
    s.append(("cursor-escape-minimal", "small.txt", 12, 6, [
        act("cmd", L(":"), P(120), L("wwwwwwwwwww"), P(400)),
    ], "MINIMAL: 12 columns, ':' then 11 w's -- emitted cursor column 12"))

    # A tab in a line: the terminal moves to the next tab stop without erasing
    # what it passes over, and `\x1b[K` only erases from where the cursor ends
    # up (Screen.trp:76-81).
    s.append(("tab-stale-cells", "edges.txt", 40, 8, [
        act("insert_tab", L("i"), P(150), K("Tab"), P(400)),
    ], "MINIMAL: 'i' then Tab on a non-empty first line"))

    # The same defect without typing anything: a tab-indented file, scrolled so
    # that the rows are redrawn over other rows' text.
    s.append(("tab-file-scroll", "tabs.txt", 40, 6, [
        act("down", L("j"), P(120), L("j"), P(120), L("j"), P(120),
            L("j"), P(120), L("j"), P(300)),
        act("down2", L("j"), P(120), L("j"), P(300)),
    ], "a tab-indented file scrolled: rows redraw over the previous rows"))

    # The same on a wide terminal, for contrast: 100 characters of command line.
    s.append(("long-command-wide", "small.txt", 40, 10, [
        act("open_cmd", L(":")),
        act("type_cmd", L("abcdefghijklmnopqrstuvwxyz0123456789abcd")),
        act("cancel", K("Escape"), P(300)),
    ], "command line longer than the width, wide terminal"))

    # The dirty flag rides in the undo snapshot (Api.trp:187, :236) and so is
    # restored across a save point.
    s.append(("undo-across-save", "small.txt", 60, 12, [
        act("insert", L("i"), P(120), L("Z"), P(120), K("Escape"), P(300)),
        act("save", L(":w"), P(120), K("Enter"), P(400)),
        act("undo", L("u"), P(300)),
    ], "dirty flag after undoing back across a :w"))

    # A terminal of one row keeps that row for the text and has no status line
    # (Api.trp:102-107, Editor.trp:92).
    s.append(("one-row", "small.txt", 40, 1, [
        act("down", L("j"), P(150), L("j"), P(150)),
        act("insert", L("i"), P(120), L("Q"), P(200), K("Escape"), P(300)),
    ], "no status line at one row"))

    # x at the end of a line does nothing (Api.trp:209); $ on an empty line; o
    # on the last line; undo of o.
    s.append(("edges-normal", "edges.txt", 40, 10, [
        act("to_end", L("$"), P(150)),
        act("x_at_end", L("x"), P(200)),
        act("empty_line", L("j"), P(120), L("$"), P(200)),
        act("open_last", L("G_no_op"), P(50)),      # G is not bound: ignored
        act("open_below", L("o"), P(150), L("abc"), P(200), K("Escape"), P(300)),
        act("undo_open", L("u"), P(200), L("u"), P(200), L("u"), P(200),
            L("u"), P(300)),
    ], "x at end of line, $ on an empty line, o then undo"))

    # Backspace at the start of a line joins it to the one above
    # (Api.trp:213-221); Delete at the end of a line does not join
    # (Api.trp:204-211).
    s.append(("join-and-delete", "edges.txt", 40, 10, [
        act("down_home", L("j"), P(120), L("0"), P(150)),
        act("insert", L("i"), P(150)),
        act("bs_join", K("BSpace"), P(300)),
        act("esc", K("Escape"), P(300)),
        act("dc_at_end", L("$"), P(150), K("DC"), P(300)),
    ], "backspace joining lines, delete at end of line"))

    # An astral character: the buffer counts two code units for it (Api.trp:54-58)
    # and the renderer truncates by code unit (Screen.trp:28-32), while the
    # terminal draws it two cells wide.  Recorded, not asserted as a defect.
    s.append(("astral", "edges.txt", 40, 10, [
        act("insert", L("i"), P(150), L("A"), P(150), L("\U0001f600"), P(300),
            L("B"), P(300)),
        act("bs", K("BSpace"), P(200), K("BSpace"), P(400)),
    ], "an astral character in the buffer (known width limitation)"))

    # A double-width character in the basic plane: one code unit, two cells.
    # Screen.trp:28-32 says the mvp does not measure display width; this records
    # what that costs.
    s.append(("cjk-width", "edges.txt", 20, 8, [
        act("insert", L("i"), P(150), L("\u4f60\u597d\u4e16\u754c"), P(400)),
        act("esc", K("Escape"), P(300)),
    ], "wide characters: one code unit, two terminal cells"))

    # A status row exactly as wide as the terminal: Screen.trp:34-37 says
    # truncation leaves the bottom-right cell alone, truncate() cuts to `width`.
    s.append(("status-at-width", "edges.txt", 22, 8, [
        act("nav", L("j"), P(150), L("$"), P(200)),
        act("insert", L("i"), P(150), L("xyz"), P(300), K("Escape"), P(300)),
    ], "a status row filling the last cell of the last row"))

    # Page motions at the ends of the buffer, and the viewport following.
    s.append(("paging", "edges.txt", 40, 6, [
        act("pgdn", K("PageDown"), P(200), K("PageDown"), P(200),
            K("PageDown"), P(250)),
        act("pgup", K("PageUp"), P(200), K("PageUp"), P(250)),
        act("ctrl_f", K("C-f"), P(200), K("C-b"), P(250)),
    ], "PageUp/PageDown past the ends of the buffer"))

    # Keys arriving faster than frames are drawn: twenty sends with no pause
    # between them, which the pty may coalesce into one chunk.  Key.trp's
    # invariant 3 says carrying is transparent, so the decoded keys must be the
    # same however the bytes are cut up.
    s.append(("fast-burst", "edges.txt", 40, 10, [
        {"kind": "burst", "sends":
            [L("i")] + [L(c) for c in "abcdefghijklmnopqrst"] + [P(600)]},
        {"kind": "burst2", "sends":
            [K("Escape")] + [L(c) for c in "hhhhjjjjkkkkllll"] + [P(600)]},
    ], "twenty keys with no delay between them"))

    # A run of resizes with no pause between them: several SIGWINCHes before the
    # kernel has answered the first.
    s.append(("resize-storm", "edges.txt", 40, 10, [
        {"kind": "storm", "sends": [["resize", 30, 8], ["resize", 60, 20],
                                    ["resize", 20, 6], ["resize", 70, 14],
                                    ["resize", 45, 11], P(900)]},
        act("after", L("j"), P(150), L("$"), P(300)),
    ], "five resizes with no pause between them"))

    # A file with no trailing newline has no final empty line; one that ends in
    # a newline has one (Rope.trp:48-49).  Both are checked, and a save must put
    # the text back byte for byte.
    s.append(("no-trailing-newline", "nonl.txt", 40, 8, [
        act("to_end", L("j"), P(120), L("j"), P(120), L("$"), P(200)),
        act("append", L("i"), P(150), L("!"), P(250), K("Escape"), P(300)),
        {"kind": "save", "sends": [L(":w"), P(150), K("Enter"), P(500)]},
    ], "a file that does not end in a newline"))

    # An empty file: one line, of length zero.
    s.append(("empty-file", "empty.txt", 40, 8, [
        act("motions", L("$"), P(120), L("j"), P(120), L("k"), P(120),
            L("0"), P(200)),
        act("type", L("i"), P(150), L("hi"), P(250), K("Escape"), P(300)),
        {"kind": "save", "sends": [L(":w"), P(150), K("Enter"), P(500)]},
        act("undo", L("u"), P(200), L("u"), P(300)),
    ], "an empty file"))

    # A key the decoder reads as unknown, and an aborted CSI sequence.
    s.append(("unknown-keys", "edges.txt", 40, 10, [
        act("insert_key", K("IC"), P(250)),
        act("tab", L("i"), P(150), K("Tab"), P(300), K("Escape"), P(300)),
    ], "CSI 2~ (unbound) and a tab in the buffer"))

    return s


EDGES = ("alpha beta\n"
         "\n"
         "gamma delta epsilon\n"
         "short\n"
         "a line that is quite a lot longer than a narrow terminal is wide\n"
         "zeta\n"
         "eta\n"
         "theta\n"
         "iota\n"
         "kappa\n")


def run_scenario(name, filename, cols, rows, actions, note):
    outdir = os.path.join(WORKROOT, "probes", name)
    shutil.rmtree(outdir, ignore_errors=True)
    os.makedirs(outdir)
    r = Run(0, filename, cols, rows, len(actions), False, outdir, [], "p" + name[:6])
    r.prepare()
    print("-- %s  (%dx%d, %s)  %s" % (name, cols, rows, filename, note))
    try:
        cap = wait_started(r.pane)
        r.check({"kind": "startup"}, cap)
        for i, a in enumerate(actions):
            if a["kind"].startswith("undo"):
                r.did_undo = True
            if a["kind"] == "save":
                r.did_save = True
            r.do_action(a)
            if a["kind"] == "save":
                r.check_file()
        print("   clean")
        ok = True
    except Diverged as d:
        print("   DIVERGENCE after action %d (%s): %s" % (i, actions[i]["kind"], d.kind))
        for k in ("expected", "got", "row"):
            if k in d.detail:
                print("     %-9s %r" % (k, d.detail[k]))
        with open(os.path.join(outdir, "divergence.json"), "w") as f:
            json.dump({"scenario": name, "action_index": i,
                       "action": actions[i], "kind": d.kind,
                       "detail": d.detail}, f, indent=2, ensure_ascii=False)
        print("     dump: %s" % os.path.join(outdir, "divergence.json"))
        ok = False
    # what the pane and the model ended up looking like, for the record
    cap = r.pane.capture()
    want, cr, cc = r.model.screen()
    if r.tab_rows:
        print("   FINDING  %d row(s) held a tab and could not be asserted:"
              % r.tab_rows)
        for i, w in enumerate(want[:r.model.st.text_rows()]):
            if "\t" in w:
                print("     buffer row %d : %r" % (i, M.from_u16(w)))
                print("     screen row %d : %r" % (i, cap[0][i]))
    if r.cursor_escapes:
        print("   FINDING  the kernel emitted a cursor column off the screen: %s"
              % json.dumps(r.cursor_escapes[-1]))
    with open(os.path.join(outdir, "final.txt"), "w", encoding="utf-8") as f:
        f.write("screen:\n" + "\n".join(cap[0]) + "\n\ncursor: %d %d\n" % (cap[1], cap[2]))
        f.write("\nmodel:\n" + "\n".join(M.from_u16(x) for x in want)
                + "\n\ncursor: %d %d\n" % (cc, cr))
    r.pane.kill()
    return ok


TABS = ("a plain first line with no tab in it at all\n"
        "\tone\n"
        "\ttwo\n"
        "\tthree\n"
        "a second plain line, longer than the ones above it\n"
        "\tfour\n"
        "\tfive\n"
        "\tsix\n"
        "\tseven\n"
        "\teight\n")


def main():
    files = os.path.join(WORKROOT, "files")
    if not os.path.isdir(files):
        make_files(files)
    with open(os.path.join(files, "edges.txt"), "w", encoding="utf-8") as f:
        f.write(EDGES)
    with open(os.path.join(files, "tabs.txt"), "w", encoding="utf-8") as f:
        f.write(TABS)
    with open(os.path.join(files, "nonl.txt"), "w", encoding="utf-8") as f:
        f.write("first\nsecond\nthird without a newline")
    with open(os.path.join(files, "empty.txt"), "w", encoding="utf-8") as f:
        f.write("")
    want = set(sys.argv[1:])
    bad = 0
    for sc in scenarios():
        if want and sc[0] not in want:
            continue
        if not run_scenario(*sc):
            bad += 1
    print("\n%d scenario(s) diverged" % bad)
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
