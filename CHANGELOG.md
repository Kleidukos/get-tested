# CHANGELOG

## 0.1.10.0 -- 2026-09-01

* Support multiple runner versions per OS, e.g. `ubuntu-version: '24.04,latest'` by @webdevred in https://github.com/Kleidukos/get-tested/pull/94
* Return a simple list of versions when the `versions-only` parameter is provided by @webdevred in https://github.com/Kleidukos/get-tested/pull/100
* Refactor the release workflow and add `get-tested-head` rolling prerelease automation by @webdevred in https://github.com/Kleidukos/get-tested/pull/106

## 0.1.9.1 -- 2025-11-10

* Added a message to the README.md about needing to choose at least one OS by @webdevred https://github.com/Kleidukos/get-tested/pull/85
* Backwards compatibility changes for v0.1.7.1 by @webdevred https://github.com/Kleidukos/get-tested/pull/84
* Make setup-get-tested backwards compatible with older naming scheme which was used before v0.1.7.0 by @webdevred https://github.com/Kleidukos/get-tested/pull/81

## 0.1.9.0 -- 2025-10-29

* Require `text-display >=1` and updated dependency bounds by @mmhat in https://github.com/Kleidukos/get-tested/pull/66
* Minor fixes for quoting and best practices in action.yml by @webdevred in https://github.com/Kleidukos/get-tested/pull/68
* Add more information to the generated GitHub build matrix by @mmhat in https://github.com/Kleidukos/get-tested/pull/64
* Test and release for macOS and windows by @webdevred in https://github.com/Kleidukos/get-tested/pull/70
* Add a check for get-tested-head when downloading release by @webdevred in https://github.com/Kleidukos/get-tested/pull/74
* Quiet extraction logs and improve debug output by @webdevred in https://github.com/Kleidukos/get-tested/pull/77

## 0.1.8.1 -- 2024-09-13

* Fix the setup action

## 0.1.8.0 -- 2024-09-06

* Provide the `--newest` and `--oldest` flags to pick the newest or oldest-supported GHC. ([55](https://github.com/Kleidukos/get-tested/pull/55))

## 0.1.5.0 -- 2023-09-25

* Print GHC versions as an array in the absence of OS flag ([#15](https://github.com/Kleidukos/get-tested/pull/15))
