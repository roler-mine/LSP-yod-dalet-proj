#!/usr/bin/env bash
# Module overview: Verifies that OCaml sources match the repository ocamlformat settings.

set -euo pipefail

cd "$(dirname "$0")/../.."

mapfile -t files < <(git ls-files "apps/lsp-server/**/*.ml" "apps/lsp-server/**/*.mli")

if [ "${#files[@]}" -eq 0 ]; then
  exit 0
fi

opam exec -- ocamlformat --enable-outside-detected-project --check "${files[@]}"
