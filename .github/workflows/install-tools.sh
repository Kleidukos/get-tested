#!/usr/bin/env bash

case "$(uname -s)" in
        Linux*) sudo apt install upx-ucl;;
        Darwin*) exit 0;;
esac
