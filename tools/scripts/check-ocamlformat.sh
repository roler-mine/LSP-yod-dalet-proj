#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/../.."

mapfile -t files < <(git ls-files "apps/lsp-server/**/*.ml" "apps/lsp-server/**/*.mli")

if [ "${#files[@]}" -eq 0 ]; then
  exit 0
fi

opam exec -- ocamlformat --enable-outside-detected-project --check "${files[@]}"
