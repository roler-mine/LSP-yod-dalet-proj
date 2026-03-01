# Repository Structure

This repository follows a source-first layout: hand-written source files are tracked, build outputs are generated locally.

## Top Level

- `README.md`: project overview and quick-start commands
- `.gitignore`: source-only tracking policy
- `.editorconfig`: shared formatting defaults
- `package.json`: root orchestration scripts for build/test/package
- `docs/`: repository and architecture docs
- `apps/vscode-extension/`: VS Code extension project
- `apps/lsp-server/`: OCaml language server project
- `tools/scripts/`: shared build and cleanup scripts

## `apps/vscode-extension/` (VS Code Client)

- `src/`: TypeScript extension source
- `syntaxes/`: TextMate grammar definition
- `runtime/server/win32-x64/`: bundled runtime server location for Windows x64
- `runtime/server/win32-arm64/`: bundled runtime server location for Windows arm64
- `package.json`: extension metadata, commands, and settings
- `out/`: compiled JavaScript output (generated, not tracked)

## `apps/lsp-server/` (OCaml Server)

- `lib/syntax/`: AST, lexer/parser, parse + diagnostics pipeline
- `lib/lsp/`: JSON-RPC framing and LSP request handling
- `lib/workspace/model/`: document model + URI/path helpers
- `lib/workspace/index/`: text indexing + workspace filesystem index
- `lib/workspace/core/`: workspace state and shared semantic foundation
- `lib/workspace/features/`: user-facing LSP features (nav, diagnostics, symbols, tokens, hints)
- `lib/README.md`: module-by-module server library map
- `bin/`: server entrypoint (`Main.ml`)
- `examples/`: manual language fixtures and sample files
- `test/`: executable test targets under dune
- `_build/`: dune artifacts (generated, not tracked)

## `tools/scripts/`

- `build-server.js`: builds server with `opam exec -- dune build @install` and copies to runtime path
- `verify-runtime-binaries.js`: validates required bundled binaries for release packaging
- `clean-generated.ps1`: removes local build/package artifacts

## Suggested Editing Flow

1. Language parsing/analysis: `apps/lsp-server/lib/syntax/`
2. Workspace/nav behavior: `apps/lsp-server/lib/workspace/`
3. LSP wire-level behavior: `apps/lsp-server/lib/lsp/lsp_server.ml`
4. VS Code integration UX: `apps/vscode-extension/src/extension.ts`
