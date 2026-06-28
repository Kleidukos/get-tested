#!/usr/bin/env bash
set -eu

TAG="${1:-get-tested-head}"

git fetch

files_changed=$(git --no-pager diff --name-only origin/main)
if ! echo "$files_changed" | grep -qE '^(action\.yml|setup-get-tested/action\.yml)$'; then
  echo "No relevant files changed, skipping tag update."
  exit 0
fi

git tag -d "$TAG" 2>/dev/null || true
git push origin ":refs/tags/$TAG" 2>/dev/null || true
git tag "$TAG"
git push -f origin "$TAG"
