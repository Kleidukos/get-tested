#!/usr/bin/env bash

GETTESTED_PATH="distribution/get-tested"

case "$(uname -s)" in
        Linux*) 
          strip $GETTESTED_PATH
          upx -9 $GETTESTED_PATH
          ;;
        Darwin*)
            echo "upx crashes on macOS Ventura and above" ;;
esac

