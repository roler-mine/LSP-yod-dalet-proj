# Repository Structure

This repository follows a source-first layout: hand-written source files are tracked, build outputs are generated locally.

## Top Level

- `README.md`: project overview and quick-start commands
- `CONTRIBUTING.md`: contributor workflow and review checklist
- `.gitignore`: source-only tracking policy
- `.editorconfig`: shared formatting defaults
- `docs/`: repository and architecture docs
- `scripts/`: local maintenance utilities
- `extension_proj/`: VS Code extension project
- `server_proj/`: OCaml language server project

## `extension_proj/` (VS Code Client)

- `src/`: TypeScript extension source
- `syntaxes/`: TextMate grammar definition
- `scripts/build-server.js`: builds server and bundles binary for VSIX
- `server/`: local bundle target for packaged server binaries
- `package.json`: extension metadata, commands, and settings
- `out/`: compiled JavaScript output (generated, not tracked)

## `server_proj/` (OCaml Server)

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

## `docs/`

- `REPO_STRUCTURE.md`: this file

## `scripts/`

- `clean-generated.ps1`: removes local build/package artifacts

## Suggested Editing Flow

1. Language parsing/analysis: `server_proj/lib/syntax/`
2. Workspace/nav behavior: `server_proj/lib/workspace/`
3. LSP wire-level behavior: `server_proj/lib/lsp/lsp_server.ml`
4. VS Code integration UX: `extension_proj/src/extension.ts`
