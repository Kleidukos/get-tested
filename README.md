# get-tested

A CLI tool that retrieves the `tested-with` stanza of a cabal file and formats it in such a way that GitHub Actions can use it.

## Usage

The inputs of the action (under the `with:` stanza) are the following:

*  cabal-file:
    The path to your cabal file, e.g. somefolder/myproject.cabal. required;
*  version: Version of the tool. required;
*  windows: Enable Windows runner. not required, defaults to false;
*  macos: Enable macOS runner. not required, defaults to false;
*  ubuntu: Enable Ubuntu runner. not required, defaults to false.

One of the three OS inputs is required however. See below for an example:

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
        uses: kleidukos/get-tested@v0.1.6.0
        with:
          cabal-file: get-tested.cabal
          ubuntu: true
          version: 0.1.6.0
  tests:
    name: ${{ matrix.ghc }} on ${{ matrix.os }}
    needs: generate-matrix
    runs-on: ${{ matrix.os }}
    strategy:
      matrix: ${{ fromJSON(needs.generate-matrix.outputs.matrix) }}
```

![](./showcase.png)
