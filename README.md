# get-tested

A CLI tool that retrieves the `tested-with` stanza of a cabal file and formats it in such a way that GitHub Actions can use it.

## Usage

Put this in your GitHub Action file

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
