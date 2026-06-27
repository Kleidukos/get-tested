set -eu
echo "=== ./out contents ==="
ls -lhR ./out || true
mkdir -p release_files
for p in ./out/*; do
  if [ -d "$p" ]; then
    artname=$(basename "$p")
    for f in "$p"/*; do
      if [ -f "$f" ]; then
        case "$artname" in
          *Windows* | *windows*)
            cp "$f" "get-tested.exe"
            zip "release_files/${artname}.zip" "get-tested.exe"
            rm "get-tested.exe"
            ;;
          *Linux* | *linux* | *macOS* | *macos*)
            cp "$f" "get-tested"
            tar -czf "release_files/${artname}.tar.gz" "get-tested"
            rm "get-tested"
            ;;
          *)
            base=$(basename "$f")
            cp "$f" "release_files/${artname}-${base}"
            ;;
        esac
      fi
    done
  elif [ -f "$p" ]; then
    base=$(basename "$p")
    cp "$p" "release_files/${base}"
  fi
done
echo "=== release_files contents ==="
ls -lh release_files || true
