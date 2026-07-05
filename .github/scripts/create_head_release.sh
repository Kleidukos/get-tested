#!/usr/bin/env bash
#set -eu

TAG="${1:-get-tested-head}"

git fetch

files_changed=$(git --no-pager diff --name-only origin/$TAG)
if [[ $? -eq 0 ]] && ! echo "$files_changed" | grep -qE '^(action\.yml|setup-get-tested/action\.yml)$'; then
    echo "No relevant files changed, skipping tag update."
    echo "new_release_needed=false" | tee "$GITHUB_OUTPUT"
    exit 0
fi

gh release delete "$TAG" --yes --cleanup-tag || true
git push origin ":refs/tags/$TAG" 2>/dev/null || true
git tag "$TAG"
git push -f origin "$TAG"

echo "new_release_needed=true" | tee "$GITHUB_OUTPUT"
