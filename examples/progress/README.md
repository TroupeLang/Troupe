# progress

A single-line CLI progress bar for a batch whose size is known in advance. `Progress.trp` is a
program-relative module; `progress-demo.trp` drives it three ways.

```
Rendering [##############..........................]  41/115  35%  0:00  ETA 0:02
```

The line carries the label, the bar, the count, the percentage, the elapsed time, and an ETA.

## The state is the caller's

The bar has no hidden state. `start` returns a state, `step` returns the next state, `finish`
closes the line off. The state is an ordinary record, so it rides in a fold accumulator:

```sml
import List
import "./Progress"

let val auth = authority
    val p0        = Progress.start auth "Rendering" (List.length docs)
    val (p, out)  = List.foldl (fn (d, (p, acc)) => (Progress.step p, render d :: acc))
                               (p0, []) docs
    val _         = Progress.finish p
in List.reverse out end
```

A state carries the authority it draws with, so only `start` takes one.

## API

| Function                     | Result  | Meaning                                                           |
|------------------------------|---------|-------------------------------------------------------------------|
| `start auth label total`     | state   | Begins a bar of `total` items under `defaults`, and draws it at zero |
| `startWith auth opts total`  | state   | The same under the options record `opts`                          |
| `step st`                    | state   | One item done                                                     |
| `stepBy n st`                | state   | `n` items done; the count is clamped to `[0, total]`              |
| `finish st`                  | unit    | Final draw if the line is behind the count, then ends the line    |
| `count st`                   | int     | Items counted so far                                              |
| `total st`                   | int     | Items the bar was started for                                     |
| `redraws st`                 | int     | How many times the bar has been drawn, `start` included           |
| `defaults`                   | options | `{ label = "", width = 40, plain = false }`                       |

Options are a record with three fields, written as an update of `defaults`:

```sml
Progress.startWith auth { Progress.defaults with label = "Rendering", width = 20 } 115
```

| Field   | Meaning                                                                        |
|---------|--------------------------------------------------------------------------------|
| `label` | Printed before the bar; `""` prints no prefix and no space                     |
| `width` | Cells between the brackets. Values below 1 are raised to 1                     |
| `plain` | `true` selects line-per-update output (see [Output modes](#output-modes))       |

`finish` returns unit rather than a state: the line is closed and the state is done with. Read
`count` or `redraws` off the last state the fold produced, as `progress-demo.trp` does.

## Output modes

| Mode                 | Output                                                           | Correct on |
|----------------------|------------------------------------------------------------------|------------|
| bar (`plain = false`) | One line rewritten in place, each draw prefixed with `\r`       | A terminal |
| plain (`plain = true`) | Each draw is its own complete line, ended with `\n`            | A file     |

Whether stdout is a terminal cannot be queried from Troupe, so the caller selects the mode. A
`\r`-redrawn bar in a redirected file is one physical line holding every draw; pass `plain = true`
when the output is going anywhere but a terminal.

The terminal width cannot be queried either, hence `width` as an option rather than a value the
module works out for itself. In bar mode a draw is padded with spaces out to the length of the
previous draw, because `\r` on its own leaves a shorter line's tail on screen.

## Redrawing

A step redraws only when the output would differ from what is on the line already: when the number
of filled cells changes, when the displayed second changes, or on the step that reaches the total.
4000 steps over a 20-cell bar that completes inside a second therefore draw 21 times, once per cell
plus the initial draw — `redraws` reports it.

## The ETA

The ETA is extrapolated from the run's own observed rate: elapsed time per item counted so far,
scaled by the items remaining. Until there are at least 3 items counted and at least 250 ms
elapsed, the field reads `ETA --:--` rather than a number extrapolated from too small a sample. Once
the count reaches the total the field is dropped.

Elapsed time and ETA are formatted `m:ss`, or `h:mm:ss` from an hour on.

## Degenerate totals

| Total | Behaviour                                                                            |
|-------|--------------------------------------------------------------------------------------|
| 0     | Nothing to wait for: the bar is full and the percentage 100 from the start. No division by zero, and `step` cannot move the count off 0 |
| 1     | Two draws: `0/1 0%` at `start`, `1/1 100%` at the first `step`. Further steps are clamped away and draw nothing |

A negative `total` is treated as 0.

## Running the demo

From the repository root:

```sh
./bin/troupec --update-deps examples/progress/progress-demo.trp
./local.sh examples/progress/progress-demo.trp -- bar
./local.sh examples/progress/progress-demo.trp -- plain
./local.sh examples/progress/progress-demo.trp -- edge
```

| Mode    | What it runs                                                                       |
|---------|------------------------------------------------------------------------------------|
| `bar`   | 115 items at 25 ms each, one line redrawn in place                                 |
| `plain` | 60 items at 35 ms each in line-per-update mode; try it with `> /tmp/progress.log`   |
| `edge`  | A total of 0, a total of 1 stepped twice, and 4000 items over a 20-cell bar        |

`--update-deps` is only needed the first time and after any edit to `Progress.trp`: the module is
pinned by content hash in `progress-demo.deps.json`, and a compile against a stale pin fails.

## Using the module from another program

```sml
import "./Progress"
```

The path is relative to the importing file, so a program elsewhere in the tree names it by its own
relative path (`import "../progress/Progress"`), then establishes the pin with
`troupec --update-deps` on that program. See [docs/MODULES.md](../../docs/MODULES.md).
