#!/usr/bin/env bash
set -eu

HEAD_TAG="get-tested-head"

git fetch 2> /tmp/git_fetch_log

if [[ $? -ne 0 ]]; then
    gcat /tmp/git_fetch_log
fi

tag=$(git --no-pager tag -l | grep -v "$HEAD_TAG" | sort -V | tail -1)

if [[ -z "$tag" ]]; then
    echo "no release tags found, nothing to validate"
    exit 0
fi

action_yml="./action.yml"

commit_hash=$(grep 'uses:.*setup-get-tested@' "$action_yml" \
    | grep -v '#' \
    | awk -F '@' '{ print $2 }' \
    | tail -1)

if [[ -z "$commit_hash" ]]; then
    echo "cannot find commit hash in $action_yml"
    exit 1
fi

if ! git --no-pager diff --quiet "$tag" "$commit_hash" -- setup-get-tested/action.yml; then
    echo "please update the commit hash in $action_yml"
    exit 1
fi
