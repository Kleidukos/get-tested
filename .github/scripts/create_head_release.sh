#!/usr/bin/env bash
set -eu
TAG="${1:-get-tested-head}"
DRY_RUN="${DRY_RUN:-false}"
git fetch --tags

if git rev-parse --verify --quiet "refs/tags/$TAG" >/dev/null; then
  files_changed=$(git --no-pager diff --name-only "refs/tags/$TAG")

  if ! echo "$files_changed" | grep -qE '^(action\.yml|setup-get-tested/action\.yml|get-tested\.cabal|cabal\.([a-z]+\.)?project)$|^(src|app)/'; then
    echo "No relevant files changed, skipping tag update."
    echo "new_release_needed=false" | tee "$GITHUB_OUTPUT"
    exit 0
  fi
else
  echo "Tag $TAG does not exist yet, creating first release."
fi

echo "new_release_needed=true" | tee "$GITHUB_OUTPUT"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "Dry run: would move $TAG to $GITHUB_SHA and create a fresh draft prerelease."
  exit 0
fi

# A draft release does not create the git tag, so push it explicitly.
gh release delete "$TAG" --yes --cleanup-tag || true
git push origin ":refs/tags/$TAG" 2>/dev/null || true
git tag "$TAG"
git push -f origin "$TAG"

gh release create "$TAG" release_files/* \
  --draft \
  --prerelease \
  --title "$TAG" \
  --notes "Rolling head build of get-tested from main. Publish to make it available to consumers."
