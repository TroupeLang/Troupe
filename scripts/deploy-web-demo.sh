#!/bin/bash
#
# Publishes build/web-demo (make web-demo) to the repository GitHub Pages
# serves it from: one commit, force-pushed, so the deploy branch holds the
# current site and nothing else.
#
#   scripts/deploy-web-demo.sh <repository url> [branch]
#
# The repository's Pages setting must point at the branch and the root.
set -eu

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
SITE="$ROOT/build/web-demo"
REPO=${1:?usage: deploy-web-demo.sh <repository url> [branch]}
BRANCH=${2:-main}

if [ ! -f "$SITE/index.html" ]; then
  echo "deploy-web-demo: $SITE is not built (make web-demo)" >&2
  exit 1
fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
git -C "$WORK" init -q -b "$BRANCH"
cp -R "$SITE"/. "$WORK/"
git -C "$WORK" add -A
git -C "$WORK" -c user.name="$(git -C "$ROOT" config user.name)" \
                -c user.email="$(git -C "$ROOT" config user.email)" \
    commit -q -m "Troupe web demo, from $(git -C "$ROOT" rev-parse --short HEAD) on $(git -C "$ROOT" branch --show-current)"
git -C "$WORK" push -q --force "$REPO" "$BRANCH:$BRANCH"
echo "deployed $(du -sh "$SITE" | cut -f1) to $REPO ($BRANCH)"
