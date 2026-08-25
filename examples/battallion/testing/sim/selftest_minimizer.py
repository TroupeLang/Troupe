#!/usr/bin/env python3
"""selftest_minimizer.py -- does the checker actually catch a wrong model, and
does the minimizer find the shortest prefix that shows it?

A harness that never fires proves nothing, so this injects a known fault into
the model, plays a fixed action list in which exactly one action exposes it, and
asserts that

  * the run diverges at that action and not before, and
  * the minimizer binary-searches down to exactly that prefix.

Three faults, one per newly-modelled rule, so that the rules the recalibration
added are covered and not only the ones the first campaign checked:

  x      `x` deletes nothing                        (an edit)
  goal   vertical motions do not keep the goal column, which is what the editor
         did before 760691eb -- so this is also the check that the model is not
         still describing the old editor
  dirty  a write to another path clears the modified state, i.e. `:w PATH`
         mistaken for `:saveas`

  python3 selftest_minimizer.py            # all three, each in its own process
  python3 selftest_minimizer.py goal       # one
"""

import json
import os
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)

FILE = "probe.txt"


def lit(s, ms=300):
    return [["lit", s], ["pause", ms]]


def key(n, ms=350):
    return [["key", n], ["pause", ms]]


def A(kind, *groups):
    sends = []
    for g in groups:
        sends.extend(g)
    return {"kind": kind, "sends": sends}


# Each entry: the action list, and the index at which the fault must show.
FAULTS = {
    # `x` on "alpha beta gamma": the first two actions only move.
    "x": ([A("nav", lit("l")), A("nav", lit("l")), A("delete_x", lit("x", 500))], 2),
    # `$` on line 1 (16 units) then two `j`s: the first lands on "x" (1 unit),
    # where the true goal and the broken one agree, and the second lands on a
    # 26-unit line, where they do not.
    "goal": ([A("nav", lit("gg")), A("nav", lit("$")),
              A("nav", lit("j")), A("nav", lit("j", 500))], 3),
    # A modified buffer written to another path is still modified.
    "dirty": ([A("nav", lit("gg")),
               A("insert_burst", lit("i"), lit("Z"), key("Escape", 400)),
               A("save_as", lit(":w copy-a.txt"), key("Enter", 700))], 2),
}


def run_one(fault):
    import btmodel as M
    from simulate import Run, Diverged, WORKROOT
    from tmuxdrv import wait_started
    from probe_features import CONTENT

    if M.BREAK != fault:
        print("BTSIM_BREAK=%s is not in effect; the self-test proves nothing"
              % fault)
        return 2
    actions, want_index = FAULTS[fault]
    files = os.path.join(WORKROOT, "files")
    os.makedirs(files, exist_ok=True)
    with open(os.path.join(files, FILE), "w", encoding="utf-8") as f:
        f.write(CONTENT)
    outdir = os.path.join(WORKROOT, "selftest-" + fault)
    shutil.rmtree(outdir, ignore_errors=True)
    os.makedirs(outdir)
    r = Run(0, FILE, 60, 14, len(actions), False, outdir, [], "self" + fault)
    r.prepare()
    wait_started(r.pane)
    hit = None
    for i, a in enumerate(actions):
        try:
            r.do_action(a)
            if a["kind"].startswith("save"):
                r.check_files()
        except Diverged as d:
            hit = (i, d)
            break
    r.pane.kill()
    if hit is None:
        print("FAIL[%s]: the injected fault did not produce a divergence" % fault)
        return 1
    i, d = hit
    print("[%s] diverged at action %d (%s): %s"
          % (fault, i, actions[i]["kind"], d.kind))
    print("     expected %r" % d.detail.get("expected"))
    print("     got      %r" % d.detail.get("got"))
    if i != want_index:
        print("FAIL[%s]: expected the divergence at action %d" % (fault, want_index))
        return 1
    m = r.minimize(actions[:i + 1])
    print("[%s] minimizer: %s" % (fault, json.dumps(m)))
    if not m["reproducible"] or m["prefix"] != want_index + 1:
        print("FAIL[%s]: expected a reproducible minimal prefix of %d actions"
              % (fault, want_index + 1))
        return 1
    print("PASS[%s]: the checker fired at action %d and the minimizer found the "
          "%d-action prefix" % (fault, i, m["prefix"]))
    return 0


def main():
    if len(sys.argv) > 1:
        return run_one(sys.argv[1])
    rc = 0
    for fault in ("x", "goal", "dirty"):
        env = dict(os.environ, BTSIM_BREAK=fault)
        p = subprocess.run([sys.executable, os.path.abspath(__file__), fault],
                           env=env)
        rc = rc or p.returncode
    print("=== self-test %s" % ("PASS" if rc == 0 else "FAIL"))
    return rc


if __name__ == "__main__":
    sys.exit(main())
