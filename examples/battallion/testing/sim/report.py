#!/usr/bin/env python3
"""report.py -- the run table, the action mix and the latency distribution.

  python3 report.py
"""

import collections
import json
import os
import statistics
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from simulate import WORKROOT

RUNS = os.path.join(WORKROOT, "runs")


def pct(xs, q):
    xs = sorted(xs)
    return xs[min(len(xs) - 1, int(q * len(xs)))]


def main():
    rows = []
    allat = []
    kinds = collections.Counter()
    for tag in sorted(os.listdir(RUNS)):
        d = os.path.join(RUNS, tag)
        rp = os.path.join(d, "result.json")
        if not os.path.exists(rp):
            continue
        res = json.load(open(rp))
        lat = json.load(open(os.path.join(d, "latencies.json")))
        rows.append((tag, res, lat))
        allat += lat
        for line in open(os.path.join(d, "replay.jsonl"), encoding="utf-8"):
            r = json.loads(line)
            if "kind" in r:
                kinds[r["kind"]] += 1

    print("| run          | file      | size  | actions | diverg. | file checks "
          "| comparisons | status rows | tab rows | p50 | p95 | max | wall |")
    print("|--------------|-----------|-------|---------|---------|-------------"
          "|-------------|-------------|----------|-----|-----|-----|------|")
    for tag, res, lat in rows:
        print("| %-12s | %-9s | %-5s | %7d | %7d | %11d | %11d | %11d | %8d "
              "| %3.0f | %3.0f | %3.0f | %4s |"
              % (tag, res["file"], res["size"], res["actions"],
                 res["divergences"], res["saves"], res.get("files_compared", 0),
                 res.get("dirty_checks", 0), res.get("tab_rows", 0),
                 statistics.median(lat) if lat else 0,
                 pct(lat, 0.95) if lat else 0, max(lat) if lat else 0,
                 res.get("wall", "")))
    print()
    if allat:
        print("all runs: n=%d  p50=%.0fms  p90=%.0fms  p95=%.0fms  p99=%.0fms  max=%.0fms"
              % (len(allat), statistics.median(allat), pct(allat, 0.90),
                 pct(allat, 0.95), pct(allat, 0.99), max(allat)))
    ref = [r for r in rows if r[0] == "s1s80x24"]
    if ref and ref[0][2]:
        lat = ref[0][2]
        print("reference run (s1s80x24, alone on the machine): "
              "n=%d  p50=%.0fms  p95=%.0fms  max=%.0fms"
              % (len(lat), statistics.median(lat), pct(lat, 0.95), max(lat)))
    print()
    total = sum(kinds.values())
    print("action mix (%d actions):" % total)
    for k, n in kinds.most_common():
        print("  %-18s %5d  %4.1f%%" % (k, n, 100.0 * n / total))
    print()
    tot = collections.Counter()
    for _, res, _ in rows:
        for k in ("saves", "files_compared", "dirty_checks", "tab_rows",
                  "help_frames", "overflow_frames", "actions"):
            tot[k] += res.get(k, 0)
    print("totals: %d actions, %d status rows (each an assertion of the modified "
          "flag), %d rows holding a tab, %d help frames, %d file-set checks, "
          "%d byte comparisons"
          % (tot["actions"], tot["dirty_checks"], tot["tab_rows"],
             tot["help_frames"], tot["saves"], tot["files_compared"]))
    print()
    for tag, res, lat in rows:
        if res.get("overflow_frames"):
            print("%s: %d frames skipped -- the terminal clipped a row the model "
                  "said fitted, so it draws some character wider than one column"
                  % (tag, res["overflow_frames"]))
        if res["divergences"]:
            print("%s: DIVERGENCE %s  minimal=%s  dump=%s"
                  % (tag, res.get("diverge_kind"), res.get("minimal"),
                     res.get("dump")))
    return 0


if __name__ == "__main__":
    sys.exit(main())
