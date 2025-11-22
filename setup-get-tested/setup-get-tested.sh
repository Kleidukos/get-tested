#!/usr/bin/env bash

set -euo pipefail
source ./common.sh

inputs_directory="${inputs_directory:?Missing inputs directory}"

tmpdir="$(mktemp -d "${RUNNER_TEMP}/setup-get-tested.XXXXXX")"

os="$(uname | tr '[:upper:]' '[:lower:]')"
case "$os" in
msys* | mingw* | cygwin*)
        os="windows"
        # On Windows runners, convert D:\... to /d/...
        tmpdir="$(cygpath -u "$tmpdir")"
        directory_posix="$(cygpath -u "$inputs_directory")"
        outfile="get-tested.zip"
        ;;
*)
        directory_posix="$inputs_directory"
        outfile="get-tested.tar.gz"
        ;;
esac

requested_version="${requested_version:-latest}"
minimum_modern_version="0.1.7.1"
if is_tag_newer "$requested_version" "$minimum_modern_version"; then
        requested_old_version=false
else
        requested_old_version=true
fi
trap 'rm -rf ''${tmpdir}''' EXIT
if [[ "$os" == "darwin" ]]; then
        pattern='get-tested-*-macOS-arm64.tar.gz'
elif [[ "$os" == "linux" ]] && [[ "$requested_old_version" == true ]]; then
        pattern='get-tested-*-linux-amd64'
elif [[ "$os" == "linux" ]]; then
        pattern='get-tested-*-Linux-static-x86_64.tar.gz'
else
        pattern='get-tested-*-Windows-x86_64.zip'
fi
echo "::debug::chose '$pattern' for ::'$os'::"

gh_release_flags=(
        --repo Kleidukos/get-tested
        --pattern "$pattern"
        --output "${tmpdir}/${outfile}"
)
if [[ "$requested_version" == "get-tested-head" ]]; then
        gh release download "get-tested-head" "${gh_release_flags[@]}"
elif [[ "$requested_version" != "latest" ]] && [[ "$requested_version" != "" ]]; then
        gh release download "v${requested_version}" "${gh_release_flags[@]}"
else
        gh release download "${gh_release_flags[@]}"
fi

mkdir -p "$directory_posix"
if [[ "$os" == "windows" ]]; then
        unzip -q -o "${tmpdir}/${outfile}" -d "$directory_posix"
elif [[ "$os" == "linux" ]] && [[ $requested_old_version == true ]]; then
        cp "${tmpdir}/${outfile}" "$directory_posix/get-tested"
else
        tar -xzf "${tmpdir}/${outfile}" -C "$directory_posix"
fi

path="${directory_posix}/get-tested"
chmod a+x "$path"
echo "path=${path}" >>"${GITHUB_OUTPUT}"
