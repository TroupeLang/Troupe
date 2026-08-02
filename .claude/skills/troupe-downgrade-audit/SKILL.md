---
name: troupe-downgrade-audit
description: Audit the downgrading in a Troupe program — every declassify, endorse, blockdecl/blockdown, certified receive region and attenuated authority — and justify or reject each one. Invoke when asked why a downgrade exists or whether it is needed, when reviewing new or changed code that declassifies, endorses, lowers a blocking label, opens a ranged-receive region or mints an authority, when asked whether a program leaks or whether its authority is too broad, or when writing the information-flow section of a design record. Also invoke before adding a downgrade, to check whether restructuring removes the need for it.
---

# Auditing downgrades in a Troupe program

A Troupe program's type system and runtime checks prevent unintended flows unless the program says
otherwise. Everything the program says otherwise with — every downgrade — is the manual security
argument, and this skill is the method for producing it: enumerate the sites, categorize them,
justify each, and report the ones that fail.

The principles below are derived from Aslan Askarov and Andrei Sabelfeld, *Security-typed languages
for implementation of cryptographic protocols: A case study*, ESORICS 2005 — a case study of a
4500-line mental-poker implementation in Jif whose fourteen declassification points fall into four
groups with independent justifications. Page numbers cite that paper.

**Rules of engagement**: this is an audit. Do not change code. Report findings; a remedy is a design
decision for the owner.

---

## Step 1 — Inventory

Downgrading in Troupe comes in four kinds. Three are the runtime's own downgrade kinds
(`rt/src/DowngradeEnums.mts`); the fourth moves no label but decides who may move one.

| Category | What moves |
|-----------|------------|
| **VALUE** | a value's confidentiality and/or integrity label |
| **BLOCKING** | the thread's blocking label — the progress and termination channel |
| **MAILBOX** | the mailbox clearance: a receive region's ceiling, raised at the open and restored at the close |
| **AUTHORITY** | nothing; a weaker authority is minted from a stronger one, or an ambient one is bound |

Run all five greps from the repository root. Set `SCOPE` to the paths under audit plus every library
they import (the array form and `"${SCOPE[@]}"` are needed for both zsh and bash).

```bash
SCOPE=(examples/yourprog lib)

# 1. VALUE downgrades
grep -rnE '\b(declassify|endorse|downgrade)\b *\(' --include="*.trp" "${SCOPE[@]}"

# 2. BLOCKING downgrades (note the `…to` variants)
grep -rnE '\b(blockdecl|blockendorse|blockdown)(to)? *[\(a-zA-Z_]' --include="*.trp" "${SCOPE[@]}"

# 3. MAILBOX downgrades — these say nothing about declassification in their own names
grep -rnE '\b(enableRangedReceive|disableRangedReceive|consumeWithAuthority|raisembox|lowermbox)\b' \
     --include="*.trp" "${SCOPE[@]}"

# 4. AUTHORITY minting and binding
grep -rnE '\battenuate *\(|\b(val|fun) +[A-Za-z_0-9]+ *= *authority\b' --include="*.trp" "${SCOPE[@]}"

# 5. Library wrappers that re-export any of the above under a name that hides it
grep -rnE '\( *"[A-Za-z_0-9]+" *, *(listen|unlisten|nextEventAtLevel|freadlnAtLevel|freadlnPini|pdeclassify|pendorse|pdowngrade|declassifyDeep|endorseDeep|downgradeDeep|invokeAtLevelPini|blockprotect|blockupto)\b' \
     --include="*.trp" "${SCOPE[@]}"
```

Grep 5's name list is the current set of such wrappers in `lib/`; extend it when a new library
function's body turns out to contain a primitive from greps 1–4.

**Grep 3 is the one that is routinely missed.** `enableRangedReceive (lo, hi, auth)` raises the
receive ceiling to `hi`; restoring it to `lo` is a release, and the runtime *decides that release at
the open*, through the same `okToDowngrade` every `declassify` runs — `rt/src/Thread.mts` (search
`enableRangedReceive`, and the `okToDowngrade (DowngradeKind.MAILBOX, DowngradeDimension.BOTH)`
call in its body). It has two outcomes and the difference matters to the audit:

- **certified** (`ok = true`): the region can be closed, and `disableRangedReceive` needs no
  authority — the capability *is* the certificate;
- **confined** (`ok = false`): the enable does not fail. The region is pushed like any other, can
  never be closed, and receiving inside it works identically with a permanent taint.

**Then close the loop.** Grep 5 gives the library functions whose bodies contain a primitive; grep
the program for calls to *those names* (`Tty.listen`, `IfcUtil.freadlnAtLevel`, …). A downgrade
reached through two wrappers is still a downgrade site and belongs in the inventory at the call.

**Then verify against the compiled artifact**, because a grep over `.trp` cannot distinguish code
from a doc comment, and comments in this codebase discuss downgrades at length:

```bash
bin/troupec path/to/prog.trp -o /tmp/audit.js
grep -c 'declassify\|endorse\|blockdecl\|blockdown\|blockendorse\|attenuate\|RangedReceive' \
     path/to/out/*.js
```

An emitted module with a count of zero performs no downgrade, whatever its comments say.

**Inventory the absences too.** A boundary where a program *could* have downgraded and does not is
part of the argument, not outside it: a library helper provided and unused, a process partition that
removed the need, an authority deliberately withheld. Record each with the principle it satisfied
instead (usually **D6**).

---

## Step 2 — The principles, as a checklist

Nine checks. Each cites its passage and states the question it makes you ask of a site.

- **D1 — The downgrade points are the security argument.**
  *"This reduces the manual security analysis of the system down to inspection and justification of
  `declassify` statements in code"* (p. 12).
  → Is every site enumerated, across all four categories, and does each carry a justification an
  outside reader can check? A site whose outcome the code does not even record is unexamined.

- **D2 — Four dimensions per site: what, who, where, when.**
  *"For each declassification Table 1 states what is declassified, who declassifies data, where in
  the program, and when declassification may occur"* (p. 12) — the paper's *dimensions of
  information release*, after Sabelfeld and Sands.
  → Can you answer all four for this site? In Troupe, "what" must name the *dimension* too: a value
  release that leaves the blocking label raised has released in one dimension and not the other.

- **D3 — Category before justification.**
  *"different kinds of declassifications need to be treated differently"* (p. 18); the paper's
  fourteen points form four groups *"with independent reasons for justifying each of them"* (p. 13).
  → Which category is this site in, and does that category's argument actually apply? Reusing a
  neighbouring site's justification is the error the grouping exposes.

- **D4 — Release the minimum, not the container it arrived in.**
  The SuccessFlag pattern: only a boolean escapes a high `try…catch`, so *"no detailed data, such as
  the call stack, is passed to the caller"* (p. 16).
  → Is what is released the smallest thing that meets the need?

- **D5 — Require authority; do not grant it, and show no more than the release needs.**
  *"The ability to grant a class or a method authority is useful but also a potentially dangerous
  feature since this authority may be misused for inappropriate declassification"* (p. 12); the
  ReqAuth pattern prefers requiring the caller to hold authority over granting it (p. 16, p. 24);
  the Declassifier pattern is a module of release routines that *"has no authority"* (p. 16).
  → Does the code holding the downgrade store an authority, or take one as a parameter? Is the
  authority shown no wider than the release requires? In Troupe, `attenuate` is how a caller narrows
  it — including across a process boundary, since an authority is downgrade power and is
  serializable.

- **D6 — Some downgrades are artifacts of ordering, and restructuring removes them.**
  The EffectOrder pattern: *"Some declassifications in a program may be avoided if the code is
  rearranged so that low operations (such as input) precede operations that affect the `pc` label"*
  (p. 16).
  → Would reordering, re-siting or re-partitioning make this downgrade unnecessary? In Troupe this
  scales up from statements to processes: a consumer placed inside the tainted region needs no
  release at all.

- **D7 — A compound value does not downgrade in one step.**
  *"Because arrays and parameterized classes are mutable data containers they cannot be completely
  declassified with a single `declassify` statement. Each field … needs to be relabeled separately"*
  (p. 15); a single label on a mutable container permits a *laundering attack*, *"code that exploits
  a vulnerability in a protection mechanism in order to leak more information than intended"* (p. 7).
  → Is a downgrade applied to a structure in one step? Check the interior. In Troupe one
  `declassify` on a datatype value lowers the constructor node and leaves the payload where it was —
  measure it, do not assume it. The remedy is the member-wise rebuild `IfcUtil.deep` implements;
  note that `deep` does not traverse records.

- **D8 — A condition on *when* needs a mechanism, not a convention.**
  Group IV releases keys only after the game ends; *"Jif's declassification mechanism is not powerful
  enough to support such temporal properties. Therefore, we introduce a so called seal"* (p. 13) — a
  flag that changes at most once and whose integrity is asserted by every method that assumes the
  earlier phase (p. 16).
  → Does the justification depend on the program being in a particular phase? What enforces the
  phase — an object whose state is checked, or only where the call happens to sit in the source?
  Troupe's ranged-receive capability is this shape in the runtime: the certification is decided at
  the open and carried to the close by an object that is invalid if it refused.

- **D9 — Not performing a declassification is not the same as not having the flow.**
  A parameterized signature avoids a declassify, but *"This, however, does not eliminate the flow but
  makes it invisible"* (p. 17); and a declared signature that does not match the code is trusted
  regardless — *"It is the author of a signature who is responsible for its correctness"* (p. 14).
  → Does this site release something, or has a labelling decision merely arranged for the checker
  not to see a release? In Troupe, look at builtins and library functions that return at the
  caller's `pc` rather than at a source's level: each is a claim that the value reports configuration
  rather than observation, and each such claim needs a stated rule behind it.

---

## Step 3 — Justify each site

Write one entry per site, in this shape. An entry that cannot be completed is a finding.

> **`<file>:<line>` — `<the call>`**
> **Category**: VALUE | BLOCKING | MAILBOX | AUTHORITY.
> **What** moves: `<the value or label>`, from `<level>` to `<level>`.
> **Who**: `<the authority>` — held by this code, or shown by the caller?
> **Where**: `<the program point, and why here and not elsewhere>`.
> **When**: `<the point in the program's lifetime, and what enforces it>`.
> **Needed because**: `<what breaks without it — the concrete failure, not "the types complain">`.
> **This much and no more because**: `<why the target level, and why this granularity>`.
> **Alternative rejected**: `<the other coherent design, and the reason it was not taken>`.

Two entries that are easy to get wrong:

- **A MAILBOX site's "what"** is the receive ceiling, not a value, and its "when" is split: the
  decision happens at the open, the release at the close.
- **A "needed because"** must name the observed failure. In Troupe the common ones are: a plain
  `receive` silently never matches a message whose presence is above `pc`; a handler binding a
  labelled payload exceeds its guard's taint limit and is skipped with no diagnostic. Both block
  forever rather than erroring — cite the probe that shows it.

---

## Step 4 — Probing discipline

**Never assert an information-flow relationship you have not tested.** This is a repository rule, not
a preference. Predictions about which label a value ends up carrying are routinely wrong here.

Tools, all usable from a `.trp` program:

| Call | Reports |
|------|---------|
| `debugpc ()` | the current pc and blocking label |
| `_pc ()` / `_bl ()` | the pc / the blocking label, as values |
| `levelOf x` | the label on `x` |
| `printWithLabels x` | the value with its labels |
| `x raisedTo lev` | constructs a labelled value to probe against |

Discipline:

- Put probes in `tests/_unautomated/claude/<topic>/`. Never add to a directory containing `.golden`
  files without explicit permission.
- Run with `./local.sh <probe>.trp --localonly` from the repository root (`--localonly` skips p2p
  startup). Create `out/` first if it does not exist.
- End every probe file with an `OBSERVED <date> (<branch> at <sha>, <flags>)` comment holding the
  actual output. A probe without its output is not evidence.
- **Probe the configurations, not just the default.** A downgrade decision can depend on runtime
  flags: `--stdiolev` sets the channel level, and NMIFC enforcement (`--no-nmifc` to disable) is on
  by default and applies robust-declassification and transparent-endorsement checks to every
  downgrade, the ranged-receive certification included. A site sound at the default channel level can
  refuse at a level whose integrity is untrusted.
- Measure the interior of a structure separately from the structure (D7), and the blocking label
  separately from the value (D2). Two probes, not one.
- If a probe hangs, that *is* a result: it usually means a receive whose interval never covers the
  message's presence level. Record it and fix the probe, and consider whether the program under audit
  has the same shape.

---

## Step 5 — What a finding looks like

Report loudly, with the probe. Four shapes, three of them the paper's own warnings:

1. **A downgrade a restructuring would remove** (D6). The code releases at a boundary that would not
   exist if the consumer sat on the other side of it. State the restructuring; do not perform it.
2. **Authority broader than the release** (D5). A module that stores an authority rather than taking
   one; a function declared to act for a principal so that any caller may release through it; an
   unattenuated authority passed where a narrow one certifies the same operation. The paper's words:
   granting authority *"open[s] up possibilities for declassification by any caller"* and *"may be
   misused for inappropriate declassification"* (p. 16, p. 12).
3. **A laundering shape** (D7). One downgrade applied to a compound value, with the interior left
   behind or reachable through an alias at the old level — *"code that exploits a vulnerability in a
   protection mechanism in order to leak more information than intended"* (p. 7). In Troupe this
   usually presents first as a release that does not work: the consumer is still tainted by what it
   binds.
4. **A flow made invisible rather than released** (D9). A label assignment or a wrapper's return
   level that removes the need for a `declassify` without removing the flow — *"does not eliminate
   the flow but makes it invisible"* (p. 17). Ask for the rule behind the assignment; if there is
   none written down, that is the finding.

And one shape specific to Troupe:

5. **An unread certification bit.** `enableRangedReceive` returns `(ok, cap)` and never fails. Code
   that discards `ok` and later calls `disableRangedReceive cap` unconditionally will take a thread
   error whenever the certification refused — and whatever cleanup sits after that close never runs.
   This is D1: a downgrade decision the code does not record is a decision nobody audited.

Report each finding with: the site, the principle it fails, the probe and its output, the
configurations in which it bites, and the consequence. Not a remedy — that is the owner's call.

---

## Worked example: battallion

`examples/battallion` is a terminal text editor whose three session processes all receive terminal
events at the interval `[⊥, channel level]`, and it performs no value downgrade and no blocking-label
downgrade anywhere — verified in the compiled artifact, where the whole editor holds one `attenuate`
and two `enableRangedReceive` calls and zero `declassify`. It satisfied **D6** by construction: the
design record offered a "downgrade at the decoder" architecture, and the editor took the other one,
because every consumer of a keystroke is a process whose entire state is what the terminal said, so
there is no lower level for the data to reach and nothing to release. The full inventory, the
per-site justifications, and the one finding — the supervisor discards `enableRangedReceive`'s
certification bit and closes its region unconditionally, so a channel level with untrusted integrity
kills it before it restores the terminal — are in
`_dev_planning/text-editor/downgrading-principles.md` in the main checkout, with probes in
`tests/_unautomated/claude/downgrade-audit/`.
