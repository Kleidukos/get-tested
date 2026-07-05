#!/usr/bin/env bash
set -eu
TAG="${1:-get-tested-head}"
git fetch --tags

if git rev-parse --verify --quiet "refs/tags/$TAG" >/dev/null; then
  files_changed=$(git --no-pager diff --name-only "refs/tags/$TAG")
  diff_status=$?

  if [[ $diff_status -ne 0 ]]; then
    echo "Failed to diff against $TAG, aborting."
    echo "new_release_needed=false" | tee "$GITHUB_OUTPUT"
    exit 1
  fi

  if ! echo "$files_changed" | grep -qE '^(action\.yml|setup-get-tested/action\.yml)$'; then
    echo "No relevant files changed, skipping tag update."
    echo "new_release_needed=false" | tee "$GITHUB_OUTPUT"
    exit 0
  fi
else
  echo "Tag $TAG does not exist yet, creating first release."
fi

gh release delete "$TAG" --yes --cleanup-tag || true
git push origin ":refs/tags/$TAG" 2>/dev/null || true
git tag "$TAG"
git push -f origin "$TAG"
echo "new_release_needed=true" | tee "$GITHUB_OUTPUT"
