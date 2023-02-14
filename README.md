# get-tested

A CLI tool that retrieves the `tested-with` stanza of a cabal file and formats it in such a way that GitHub Actions can use it.

## Usage

Put this in your GitHub Action file

```yaml
jobs:                                                                                                                   
  generateMatrix:                                                                                                       
    name: "Generate matrix from cabal"                                                                                  
    runs-on: ubuntu-latest                                                                                              
    outputs:                                                                                                            
      matrix: ${{ steps.set-matrix.outputs.matrix }}                                                                    
    steps:                                                                                                              
      - name: Checkout base repo                                                                                        
        uses: actions/checkout@v2                                                                                       
      - name: Extract the tested GHC versions                                                                           
        id: set-matrix                                                                                                  
        run: |                                                                                                          
          wget https://github.com/Kleidukos/get-tested/releases/download/v0.1.3.0/get-tested-0.1.3.0-linux-amd64 -O get-tested
          chmod +x get-tested                                                                                           
          ./get-tested --ubuntu --macos my-project.cabal >> $GITHUB_OUTPUT                                            
  tests:                                                                                                                
    name: ${{ matrix.ghc }} on ${{ matrix.os }}                                                                         
    needs: generateMatrix                                                                                               
    runs-on: ${{ matrix.os }}                                                                                           
    strategy:                                                                                                           
      matrix: ${{ fromJSON(needs.generateMatrix.outputs.matrix) }}
```

![](./showcase.png)
