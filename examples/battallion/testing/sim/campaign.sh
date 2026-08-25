#!/bin/bash
# campaign.sh -- the eleven runs the simulator is asked for.
#
#   3 seeds x 400 actions x 2 files at 80x24        (six runs)
#   one session at 10x40                            (seed 4)
#   one session with mid-session resizes            (seed 5)
#   one session with a resize in a fifth of the
#   normal-mode actions                             (seed 6)
#   one session on the tab-indented file            (seed 7)
#   one long session, 1200 actions, with resizes    (seed 8)
#
#   bash campaign.sh seq   every run one after another (slow; the latency
#                          figures are then free of contention)
#   bash campaign.sh par   seed 1 / small alone first -- that run is the
#                          unperturbed latency reference -- then four streams
#
# Each run writes its own directory under $BT_SCRATCH/simwork/runs/<tag>/ holding
# replay.jsonl, result.json, latencies.json and, on a divergence, divergence.json
# and minimal.jsonl.  `python3 report.py` turns them into the run table.
#
# Before a campaign: compile the editor from the working tree, whose
# examples/battallion/out/ the pins then resolve against (tmuxdrv.py's header),
#
#   bin/troupec examples/battallion/bt.trp -m --output=$BT_SCRATCH/bt-current.js
#
# and check the model against the editor with
#
#   python3 probe_features.py       every recalibrated rule, on purpose
#   python3 selftest_minimizer.py   three injected model faults, each minimized
cd "$(dirname "$0")" || exit 1
MODE=${1:-par}
N=${N:-400}
LONG=${LONG:-1200}
L="${BT_WORKROOT:-${BT_SCRATCH:-/tmp/battallion-sim}/simwork}"
mkdir -p "$L"

run() { python3 simulate.py run "$@"; }

# The test files, written once here rather than raced for by the first four runs
# that find them missing.
python3 simulate.py gen-files "$L/files" || exit 1

if [ "$MODE" = seq ]; then
  for s in 1 2 3; do
    for f in small large; do run --seed "$s" --file "$f" --actions "$N"; done
  done
  run --seed 4 --file small --actions "$N" --cols 10 --rows 40
  run --seed 5 --file large --actions "$N" --resize
  run --seed 6 --file large --actions "$N" --resize-heavy
  run --seed 7 --file tabs  --actions "$N"
  run --seed 8 --file large --actions "$LONG" --resize
  exit 0
fi

# The latency reference: alone on the machine.
run --seed 1 --file small --actions "$N" 2>&1 | tee "$L/reference.log"

(
  run --seed 2 --file small --actions "$N"
  run --seed 1 --file large --actions "$N"
  run --seed 7 --file tabs  --actions "$N"
) > "$L/stream1.log" 2>&1 &

(
  run --seed 3 --file small --actions "$N"
  run --seed 2 --file large --actions "$N"
  run --seed 4 --file small --actions "$N" --cols 10 --rows 40
) > "$L/stream2.log" 2>&1 &

(
  run --seed 3 --file large --actions "$N"
  run --seed 5 --file large --actions "$N" --resize
  run --seed 6 --file large --actions "$N" --resize-heavy
) > "$L/stream3.log" 2>&1 &

(
  run --seed 8 --file large --actions "$LONG" --resize
) > "$L/stream4.log" 2>&1 &

wait
echo "=== all streams done"
