# get-tested

A CLI tool that retrieves the `tested-with` stanza of a cabal file and formats
it in such a way that GitHub Actions can use it.

## Usage

The inputs of the action (under the `with:` stanza) are the following:

*  `cabal-file:` \
   The path to your cabal file, e.g. somefolder/myproject.cabal.
   \
   _Required:_ true

*  `version:` \
   The version of the get-tested tool that is used.
   \
   _Required:_ false \
   _Default:_ The latest release

*  `windows:` \
   **(deprecated)** Enable Windows runner, latest version.
   \
   _Required:_ false \
   _Default:_ false

*  `windows-version:` \
   Enable Windows runner. If both `windows` and `windows-version` inputs are
   set, the explicit version will take priority.
   \
   _Required:_ false \
   _Default:_ Not set

*  `macos:` \
   **(deprecated)** Enable macOS runner, latest version.
   \
   _Required:_ false \
   _Default:_ false

*  `macos-version:` \
   Enable macOS runner. If both `macos` and `macos-version` inputs are set, the
   explicit version will take priority.
   \
   _Required:_ false \
   _Default:_ Not set

*  `ubuntu:` \
   **(deprecated)** Enable Ubuntu runner, latest version.
   \
   _Required:_ false \
   _Default:_ false

*  `ubuntu-version:` \
   Enable Ubuntu runner. If both `ubuntu` and `ubuntu-version` inputs are set,
   the explicit version will take priority.
   \
   _Required:_ false \
   _Default:_ Not set

*  `newest:` \
   Enable only the newest GHC version found in the cabal file
   \
   _Required:_ false \
   _Default:_ false

*  `oldest:` \
   Enable only the oldest GHC version found in the cabal file
   \
   _Required:_ false \
   _Default:_ false

**Important:**
- You **must** specify at least one platform to run tests.
- This means at least one of the following six inputs must be provided:
  - `windows` or `windows-version`
  - `macos` or `macos-version`
  - `ubuntu` or `ubuntu-version`
 - Deprecated flags (`windows`, `macos`, `ubuntu`) use the latest version.
 - If both a deprecated flag and an explicit version are provided, the explicit version **takes priority**.

See below for an example:

```yaml
jobs:
  generate-matrix:
    name: "Generate matrix from cabal"
    outputs:
      matrix: ${{ steps.set-matrix.outputs.matrix }}
    runs-on: ubuntu-latest
    steps:
      - name: Extract the tested GHC versions
        id: set-matrix
        uses: kleidukos/get-tested@v0.1.9.1
        with:
          cabal-file: get-tested.cabal
          ubuntu-version: "latest"
          macos-version: "latest"
          version: 0.1.9.1
  tests:
    name: ${{ matrix.ghc }} on ${{ matrix.os }}
    needs: generate-matrix
    runs-on: ${{ matrix.os }}
    strategy:
      matrix: ${{ fromJSON(needs.generate-matrix.outputs.matrix) }}
```

![](./showcase.png)
