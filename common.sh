#!/usr/bin/env bash

is_tag_newer () {
    requested_version="$1"
    minimum_modern_version="$2"
    if [[ "$requested_version" == "" ]] || [[ "$requested_version" == "get-tested-head" ]] || [[ "$requested_version" == "latest" ]] || dpkg --compare-versions "$requested_version" ge "$minimum_modern_version"; then
        return 0
    else
        return 1
    fi
}
