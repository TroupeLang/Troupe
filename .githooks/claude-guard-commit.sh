#!/usr/bin/env bash
#
# claude-guard-commit.sh — Claude Code PreToolUse hook (Bash tool).
#
# Blocks, in-session and before it runs, any `git commit` whose command text
# leaks internal planning-doc item IDs (see check-plan-ids.sh). This is the
# early, agent-facing counterpart to the repo's commit-msg git hook, which
# remains the authoritative check for every commit path.
#
# Reads the hook JSON payload on stdin; extracts .tool_input.command. If it is
# a git commit and its text trips the shared checker, exits 2 to block (stderr
# is surfaced to the agent).

set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
payload=$(cat)

# Pull out the command string. Prefer jq; fall back to a tolerant grep/sed.
if command -v jq >/dev/null 2>&1; then
  cmd=$(printf '%s' "$payload" | jq -r '.tool_input.command // empty')
else
  cmd=$(printf '%s' "$payload" \
    | sed -n 's/.*"command"[[:space:]]*:[[:space:]]*"\(.*\)".*/\1/p')
fi

# Only police git commits.
case "$cmd" in
  *"git commit"*) ;;
  *) exit 0 ;;
esac

if printf '%s' "$cmd" | "$here/check-plan-ids.sh" >/dev/null 2>&1; then
  exit 0
fi

{
  echo "Blocked: this git commit command references internal planning-doc item"
  echo "IDs (e.g. 4a, Step 4c, step4g-..., R1, C8). Commit messages must be"
  echo "self-contained — describe the change, not the plan item. Rewrite the"
  echo "message (and rename any ID-named branch before merging)."
} >&2
exit 2
