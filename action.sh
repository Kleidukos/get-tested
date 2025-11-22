#!/usr/bin/env bash

set -euo pipefail

windows_version="${windows_version:-}"
macos_version="${macos_version:-}"
ubuntu_version="${ubuntu_version:-}"
newest="${newest:-}"
oldest="${newest:-}"

requested_version="${requested_version:-}"
cabal_file="${cabal_file?cabal_file not set}"

source ./common.sh

echo "::debug::windows: ${windows_version}"
if [[ "${windows_version}" != "" ]]; then
        echo "::debug::Windows explicit enabled: $${windows_version}"
        "windows=--windows-version=${windows_version}"
elif [[ "${windows_version}" == "true" ]]; then
        printf "::warning title=Legacy option \`windows\`:: Windows fallback enabled: %s" "$windows_version"
        windows="--windows"
else
        windows=""
fi

echo "::debug::macOS: ${macos_version}"
if [[ "${macos_version}" != "" ]]; then
        echo "::debug::macOS explicit version enabled: ${macos_version}"
        macos="--macos-version=${macos_version}"
elif [[ "${macos_version}" == "true" ]]; then
        printf "::warning title=Legacy option \`macos\`: :macOS fallback enabled: %s" "$macos_version"
        macos="--macos"
else
        macos=""
fi

echo "::debug::ubuntu: ${ubuntu_version}"
if [[ "${ubuntu_version}" != "" ]]; then
        echo "::debug::Ubuntu explicit version enabled: ${ubuntu_version}"
        ubuntu="--ubuntu-version=${ubuntu_version}"
elif [[ "${ubuntu_version}" == "true" ]]; then
        printf "::warning title=Legacy option \`ubuntu\`::Ubuntu fallback enabled: %s" "${ubuntu_version}"
        ubuntu="--ubuntu"
else
        ubuntu=""
fi

echo "::debug::Is --newest enabled: ${newest}"
echo "::debug::Is --oldest enabled: ${oldest}"

if [[ "${oldest}" == "true" && "${newest}" == "true" ]]; then
        echo "::error title=Incompatible options::You cannot use the 'oldest' and 'newest' options together!"
        exit 1
fi

if [[ "${oldest}" == "true" ]]; then
        echo "::debug::oldest version enabled"
        relative_version="--oldest"
elif [[ "${newest}" == "true" ]]; then
        echo "::debug::newest version enabled"
        relative_version="--newest"
else
        relative_version=""
        echo "::debug::No relative version selected"
fi

minium_modern_version="0.1.9.0"
if [[ "$windows" == "" ]] && [[ "$macos" == "" ]] && [[ "$ubuntu" == "" ]]; then
        echo "At least one runner needed, please check the README.md in the get-tested repo"
        exit 1
fi

# shellcheck disable=SC2086
if is_tag_newer "$requested_version" "$minium_modern_version"; then
        ./get-tested "$cabal_file" $windows $macos $ubuntu $relative_version
else
        ./get-tested "$cabal_file" $windows $macos $ubuntu $relative_version >>"$GITHUB_OUTPUT"
fi
