#!/usr/bin/env bash
set -euo pipefail

missing=0

for src in src/*.lisp; do
  base="$(basename "$src" .lisp)"
  doc="docs/${base}.md"
  if [[ -f "$doc" ]]; then
    echo "OK: $doc"
  else
    echo "MISSING DOC: $doc (for $src)" >&2
    missing=1
  fi
done

if [[ $missing -ne 0 ]]; then
  echo "ERROR: missing docs for one or more src/*.lisp files." >&2
  exit 1
fi

echo "OK: all docs present. Remember to update them when behavior changes."
