# CLAUDE.md

Agent-specific directives for working in this repository. This file contains **only** directives
for how to work here. For human-facing reference material, see:

- [README.md](README.md) — overview, project components, repository layout
- [docs/INSTALL.md](docs/INSTALL.md) — dependencies and installation
- [docs/DEVELOPMENT.md](docs/DEVELOPMENT.md) — build/test commands, running programs, source maps
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) — compilation pipeline, runtime, IFC, file extensions
- [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md) — test-suite layout, adding a built-in
- [docs/MODULES.md](docs/MODULES.md) — program-relative modules, content-addressed identity, pins
- [docs/OPERATORS.md](docs/OPERATORS.md) — user-defined infix operators and declared fixity
- [docs/VARIANTS.md](docs/VARIANTS.md) — `datatype` declarations and syntactic variants
- [docs/FFI.md](docs/FFI.md) — native modules: `require native`, manifests, per-host availability
- [docs/NETWORKING.md](docs/NETWORKING.md) — P2P runtime

## Build before running

Before running Troupe programs or tests, check for stale builds and rebuild what changed. Build
commands are documented in [docs/DEVELOPMENT.md](docs/DEVELOPMENT.md) ("Building and running").

| Changed                   | Command         | Symptom of a stale build                                 |
|---------------------------|-----------------|----------------------------------------------------------|
| Haskell (`compiler/`)     | `make compiler` | `troupec: command not found`, parse errors in valid code |
| TypeScript (`rt/src/`)    | `make rt`       | `Cannot find module` for runtime files                   |
| Troupe libraries (`lib/`) | `make lib`      | `Cannot find module` for library files                   |
| Everything                | `make all`      | —                                                        |

- Libraries also need rebuilding (`make lib`) after the compiler is rebuilt.
- After any `git pull`, `git checkout`, or `git merge`, run `make all`.
- `rt/built/` holds generated code — ignore it for source-code analysis.

## Make

Always invoke `/usr/bin/make` instead of `make`, to avoid a zsh function conflict.

## Ground claims in the source — no arguing from extrapolation

Any claim about how the compiler or runtime behaves — how a construct is compiled, what a
pass does or does not optimize, what an operation costs — must be verified before it is
asserted:

- Compile a **minimal probe program** and read the artifact the claim is about.
  `bin/troupec -v probe.trp -o probe.js` refreshes the stage dumps in `out/` (relative to the
  working directory) alongside the emitted JS: `out.syntax`, `out.opreassoc`, `out.nopats`,
  `out.lowered`, `out.alpha`, `out.cps`, `out.cpsopt`, `out.ir`, `out.iropt`, `out.rawout`,
  `out.rawopt`, `out.stack`. `out.rawopt` is absent under `--no-rawopt`.
- Read the **pass that implements the behavior**, not just its name, a summary, or a
  downstream artifact two stages away.
- For performance claims, **measure** (`examples/variants/bench.trp`, or a timing probe);
  predictions about which variant is faster are routinely wrong here.
- Attach provenance to every such claim: the `file:line` read or the probe/dump inspected.
  A claim without provenance must be labeled as unverified inference — never presented with
  the same confidence as a verified fact. When challenged ("are you sure?"), the answer is a
  probe or a source read, not a restatement of the argument.

## Running tests

- **`make ci` is the suite.** It runs what CI runs, in CI's order. Anything short of it is a
  subset, and the subsets do not announce what they leave out:

  | Command          | Covers                                                                          |
  |------------------|---------------------------------------------------------------------------------|
  | `./bin/golden`   | the golden tests only — no Haskell suite, so no IR conformance corpus           |
  | `make test`      | the above plus every `stack test` suite, multinode, hostile-peer, result-socket  |
  | `make ci`        | the above plus `test/examples`, `test/prop-rt` and `test/prop-differential`      |

  A green `./bin/golden` reads like success — 1166 tests, several minutes — while leaving the
  conformance corpus, the property suites, the network tests and every example unrun. Run
  `make ci` before committing anything that touches the compiler or the runtime, and before any
  push.
- **`tests/` is not all the code.** `examples/` holds 145 programs — the editor, the savina
  benchmarks, the CLI demos — and no test suite compiles them. A library change or a moved module
  hash breaks them silently; `make test/examples`, inside `make ci`, is what notices.
- Running the suite takes time. Run it once, redirect output to a temp file, and read that file
  for failures and status instead of re-running from scratch.
- When a golden test `t.trp` fails, run `./local.sh t.trp` to see the actual output before
  drawing any conclusions.
- When a golden output changes, first confirm the new result is *correct* (understand **why** it
  changed) before regenerating goldens.

## Creating tests

- Do **not** put new tests into folders that already contain `.golden` files without explicit
  permission.
- Do **not** create `.golden` files by hand — `bin/golden` auto-generates them on first run for
  non-network tests (multinode tests need none).
- For multinode tests, do not create ids, aliases, or trustmaps; coordinate them through the
  corresponding `config.json`. Follow `tests/rt/multinode-tests/README.md` and the scripts in
  `scripts/`.
- Put temporary/throwaway tests in `tests/_unautomated/claude`.
- A test that reads stdin may require a corresponding `.input` file. Golden tests are sensitive to
  output formatting.
- When adding a new language primitive, add one or more brief tests demonstrating syntax, expected
  behavior, and error messages; propose where in the corpus they belong.
- Write tests in real Troupe syntax (consult the user guide and the existing positive test corpus).
  `troupec` can be used to check a test for syntactic validity. Remember the standard library may
  already provide useful functionality.

## Information flow claims

Do not make untested claims about IFC relationships between levels. Use `debugpc()` to inspect the
current pc and blocking labels for correct information.

## Compiler work

- **Never add a compiler flag without explicit consent.** `troupec`'s option list is user-facing
  surface, and a flag added to serve a test or a harness stays there for good. This covers
  development-only and test-only flags — those are the ones that accumulate. Propose the flag, say
  what needs it, and wait for a decision. If a check needs the compile pipeline, reach for a
  library entry point and a test suite before reaching for a flag.
- `troupec -v` writes per-stage generated files into `out/` under the working directory; inspect
  these to debug codegen.
- `--no-rawopt` disables Raw optimizations and can surface corner-case compiler bugs.
- The Troupe parser must have no shift/reduce or reduce/reduce conflicts.
- For changes that span the whole compiler pipeline, prefer approaches that keep the compiler
  working at each changed phase, so changes can be tested modularly.

## Code and commit quality

AI-generated commits must be held to the highest quality bar.

- **Never commit an unverified change.** Verify code changes by `make ci` passing (see
  [Running tests](#running-tests) — `./bin/golden` on its own is a subset and is not verification),
  behavioral changes by observing the new behavior directly, and documentation by review. A change
  that merely builds or type-checks is **not** verified.
- If a change cannot be verified — the tool that exercises it hangs, is broken, or is unavailable —
  **do not commit it.** Leave it in the working tree and surface the situation; never commit on
  faith.
- Do not bundle unverified changes together with verified ones to slip them in. Prefer several
  small, individually-verified commits over one large commit whose parts have not all been checked.
- Do not introduce cosmetic changes (trailing-space removal, reformatting) as part of an unrelated
  goal — it clutters diffs.
- When choosing between a clean-but-laborious approach and a quick partial one, in this codebase we
  almost always do the clean thing that is *right*.

## Conventions

- Use absolute paths in runtime code.
- Backticks in info-flow label syntax interact badly with the shell; save example programs that use
  backticks to files rather than creating them via `echo`.
- Use `--localonly` for local testing to skip slow p2p initialization.

### Markdown tables

Align columns for readability in raw view:

```markdown
| Column One     | Column Two | Column Three        |
|----------------|------------|---------------------|
| `short`        | `value`    | Description here    |
| `longer_entry` | `val`      | Another description |
```

## Writing documentation

- Write factually. State what something is and how to use it; do not editorialize.
- Avoid value judgments and color the reader did not ask for: words like *convenient*, *useful*,
  *powerful*, *simply*, *just*, *easy*, *nice*, *day-to-day*; hedging like *hopefully* or *should be
  fine*; and hype.
- Drop self-justifying asides about why a choice was made unless the rationale is actionable for the
  reader.
- Prefer a fact to an opinion: "skips p2p initialization" over "a convenient script that skips p2p
  initialization."

## Estimates

Give estimates as **degree of autonomy**, not weeks (which make little sense for agent-assisted
development).

## Auto-structuring large plans

Before finalizing any implementation plan, assess its complexity and structure accordingly:

| Complexity | Criteria                        | Structure                                |
|------------|---------------------------------|------------------------------------------|
| Small      | <3 tasks, single focus          | Inline in conversation                   |
| Medium     | 3-4 tasks, 2-3 areas            | Single plan file with sections           |
| Large      | 4+ tasks, multiple areas/phases | Multi-file structure in `_dev_planning/` |

Planning files live in `_dev_planning/`, which `.gitignore` excludes from the repository.

**For large plans**, automatically create the following structure without being asked:

```
_dev_planning/<feature-name>/
  index.md             # Overview, progress tracking, step links
  step-1-<name>.md     # Self-contained step file
  step-2-<name>.md
  ...
```

**Requirements for multi-file plans:**

1. **index.md**: progress table with status indicators, links to all steps, decision log.
2. **Step files**: each must be self-contained with enough context to execute in a fresh session.
3. **Progress tracking**: use checkboxes and status indicators (Pending/In Progress/Complete/Blocked).

There is no template directory. `_dev_planning/property-testing/` is an existing plan in this
layout; use it as the reference when creating a new one.

## How to communicate pre-existing failures

On occassion you may detect a failure while testing something else. Do not hide this under the rug. We strive for high code quality in the project. Do not simply report them in a by-the-way manner. Such findings are important to the health of the project and need to be properly highlighted with high alertness.