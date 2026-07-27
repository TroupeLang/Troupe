"""runall -- the Python side of the cross-language comparison.

Emits measurement rows on stdout and nothing else, in the same format and with
the same argument conventions as the Troupe-side runall.trp: `suite=NAME` picks
the benchmark set, benchmark names select a subset of it, `reps=N` sets the
repetition count, `mode=list` prints the names instead of measuring, and an
unknown name exits 2 rather than silently measuring a subset.

    python3 runall.py
    python3 runall.py suite=awfy
    python3 runall.py suite=awfy reps=5 towers
    python3 runall.py suite=awfy mode=list

Suites:

    kernels  the two hand-written cross-language kernels (default)
    awfy     the vendored Are-We-Fast-Yet micros (see awfy/PROVENANCE.md)

Normally invoked through ../run.sh, which interleaves the languages and writes
the row-file headers.
"""

import sys

import bench
import kernels
import awfy_kernels

SUITES = {"kernels": kernels.ALL, "awfy": awfy_kernels.ALL}


def main(argv):
    reps = 3
    suite = "kernels"
    mode = "run"
    names = []
    for a in argv:
        if a.startswith("reps="):
            reps = int(a[len("reps="):])
        elif a.startswith("suite="):
            suite = a[len("suite="):]
        elif a.startswith("mode="):
            mode = a[len("mode="):]
        else:
            names.append(a)

    if suite not in SUITES:
        print("unknown suite: " + suite + " (have: "
              + ", ".join(sorted(SUITES)) + ")", file=sys.stderr)
        return 2
    descriptors = SUITES[suite]

    known = [d["name"] for d in descriptors]

    # `mode=list` prints the benchmark names, one per line, and measures
    # nothing. ../run.sh asks both languages for their list and refuses to run
    # if the two disagree, which catches a kernel added on one side only.
    # It is an option rather than a bare word because AWFY has a benchmark
    # actually called `list`, and a bare `list` would shadow it -- which it did,
    # silently replacing that benchmark's rows with the name listing.
    if mode == "list":
        for n in known:
            print(n)
        return 0

    unknown = [a for a in names if a not in known]
    if unknown:
        print("unknown benchmarks: " + ", ".join(unknown), file=sys.stderr)
        return 2

    selected = [d for d in descriptors if not names or d["name"] in names]
    for d in selected:
        bench.collect_rows(d["name"], d["sizes"], reps, d["bench"], d["check"])
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
