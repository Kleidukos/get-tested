git fetch

files_changed=$(git --no-pager diff --name-only origin/main)

if echo "$files_changed" | grep -E '^(action.yml)|(setup-get-tested/action.yml)$'; then
    git tag -d get-tested-head-tmp
    git tag get-tested-head-tmp
    git push -f get-tested-head-tmp
fi
