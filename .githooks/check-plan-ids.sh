#!/usr/bin/env bash
#
# check-plan-ids.sh — reject commit messages that leak internal planning-doc
# item IDs (e.g. "4a", "Step 4c", "step4g-...", "R1", "C8"). Such IDs are
# meaningless to anyone reading the history without the private planning docs;
# commit messages must be self-contained and describe the change itself.
#
# Usage:
#   check-plan-ids.sh <message-file>   # reads the message from a file
#   check-plan-ids.sh                  # reads the message from stdin
#
# Exit status: 0 = clean, 1 = a planning-doc ID was found.
#
# Escape hatch (for the rare message where such a token is genuinely meant,
# e.g. a version string): set ALLOW_PLAN_IDS=1 in the environment, or pass
# --no-verify to git.

set -euo pipefail

# Read the candidate message.
if [ "${1:-}" != "" ] && [ -f "${1:-}" ]; then
  msg=$(cat -- "$1")
else
  msg=$(cat)
fi

if [ "${ALLOW_PLAN_IDS:-0}" = "1" ]; then
  exit 0
fi

# Strip lines that are not part of the human-authored message body:
#   - git comment lines (start with #)
#   - the everything-below-the-scissors region of `git commit --verbose`
#   - the Co-Authored-By / Signed-off-by trailers
# so those never trigger a false positive.
body=$(printf '%s\n' "$msg" \
  | sed '/^# ------------------------ >8 ------------------------$/,$d' \
  | grep -v '^#' \
  | grep -viE '^(Co-Authored-By|Signed-off-by):' || true)

# Forbidden patterns. macOS/BSD grep lacks reliable \b, so word boundaries are
# expressed with [^[:alnum:]] guards.
#   [Ss]tep[ _-]?[0-9]        -> "Step 4", "step 4c", "step4g", "step4-v1"
#   \([0-9]+[a-z]\)           -> "(4b)", "(4d)", "(4a)"
#   <bnd>[0-9]+[a-h]<bnd>     -> a bare item ref like "4a".."4h"
#   <bnd>[RC][0-9]+<end>      -> "R1", "C8"-style planning IDs. The trailing
#                               guard excludes a hyphen so identifiers like
#                               "R7-string-throw" (a legitimate token) are not
#                               mistaken for a planning ID.
pattern='[Ss]tep[ _-]?[0-9]|\([0-9]+[a-z]\)|(^|[^[:alnum:]])[0-9]+[a-h]([^[:alnum:]]|$)|(^|[^[:alnum:]])[RC][0-9]+($|[^[:alnum:]-])'

hits=$(printf '%s\n' "$body" | grep -nE "$pattern" || true)

if [ -n "$hits" ]; then
  {
    echo "commit message rejected: it references internal planning-doc item IDs."
    echo
    echo "Offending line(s):"
    printf '%s\n' "$hits" | sed 's/^/  /'
    echo
    echo "Commit messages must be self-contained. Describe the change itself,"
    echo "not the plan item (e.g. write 'add pattern-match compilation tests',"
    echo "not 'Step 4f' or 'step4f-pattern-match'). Rename ID-named branches"
    echo "before merging so the merge message does not leak the ID either."
    echo
    echo "If a flagged token is genuinely intended, re-run with ALLOW_PLAN_IDS=1"
    echo "or 'git commit --no-verify'."
  } >&2
  exit 1
fi

exit 0
