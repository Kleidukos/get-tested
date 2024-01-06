# get-tested

A CLI tool that retrieves the `tested-with` stanza of a cabal file and formats it in such a way that GitHub Actions can use it.

## Usage

Put this in your GitHub Action file

```yaml
jobs:
  generateMatrix:
    name: "Generate matrix from cabal"
    runs-on: ubuntu-latest
    steps:
      - name: Extract the tested GHC versions
        uses: turion/get-tested@v0.1.5.0
  tests:
    name: ${{ matrix.ghc }} on ${{ matrix.os }}
    needs: generateMatrix
    runs-on: ${{ matrix.os }}
    strategy:
      matrix: ${{ fromJSON(needs.generateMatrix.outputs.matrix) }}
```

![](./showcase.png)
