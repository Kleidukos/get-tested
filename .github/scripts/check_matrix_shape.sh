#!/usr/bin/env bash
set -euo pipefail

mode="$1"
input=$(cat)
jq '.' <<< "$input"

case "$mode" in
  matrix)
    expected_os="$2"
    fields="$3"
    if ! jq -e --argjson os "$expected_os" --argjson fields "$fields" '
      (.include | type == "array" and length > 0)
      and all(.include[]; . as $entry | all($fields[]; . as $field | $entry | has($field)))
      and ([.include[].os] | unique == ($os | sort))
    ' <<< "$input" > /dev/null; then
      printf 'error: expected an include list with the fields %s and the runners %s\n' "$fields" "$expected_os" >&2
      exit 1
    fi
    ;;
  versions)
    if ! jq -e 'type == "array" and length > 0 and all(.[]; type == "string")' <<< "$input" > /dev/null; then
      printf 'error: expected a non-empty list of GHC versions\n' >&2
      exit 1
    fi
    ;;
  *)
    printf 'error: unknown mode %s, expected matrix or versions\n' "$mode" >&2
    exit 2
    ;;
esac
