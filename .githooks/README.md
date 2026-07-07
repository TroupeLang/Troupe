# Git hooks

Repository-tracked hooks. Activate them in your clone with:

```sh
git config core.hooksPath .githooks
```

## commit-msg — reject planning-doc item IDs

`commit-msg` (via `check-plan-ids.sh`) rejects commit messages that reference
internal planning-doc item IDs such as `4a`, `Step 4c`, `step4g-...`, `R1`, or
`C8`. Commit messages must be self-contained: describe the change itself, not
the plan item it came from. Rename an ID-named branch before merging so the
merge message does not leak the ID either.

Escape hatch for a message where such a token is genuinely intended (e.g. a
version string): `ALLOW_PLAN_IDS=1 git commit ...` or `git commit --no-verify`.

`claude-guard-commit.sh` is the Claude Code PreToolUse counterpart (wired in
`.claude/settings.json`); it applies the same check to `git commit` commands
before they run. Both share the rule in `check-plan-ids.sh`.
