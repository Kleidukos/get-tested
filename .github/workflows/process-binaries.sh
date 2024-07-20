#!/usr/bin/env bash

CONFER_PATH="distribution/confer"

case "$(uname -s)" in
        Linux*) 
          strip $CONFER_PATH
          upx -9 $CONFER_PATH
          ;;
        Darwin*)
            echo "upx crashes on macOS Ventura and above" ;;
esac

