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
- `src/extension.ts`: bootstrap + wiring entrypoint
- `src/jovial_config.ts`: typed config reader + `initialize` payload builder
- `src/commands.ts`: command registration + restart-on-config-change listeners
- `src/startup_status.ts`: startup-phase notifications and status-bar rendering
- `src/syntax_tree_ui.ts`: AST/CST/syntax-tree viewer UI flow
- `src/watched_file_queue.ts`, `src/workspace_paths.ts`: extracted pure utilities
- `test/`: extension unit tests plus VS Code integration tests with a fake stdio LSP server
- `syntaxes/`: TextMate grammar definition
- `runtime/server/win32-x64/`: bundled runtime server location for Windows x64
- `runtime/server/win32-arm64/`: bundled runtime server location for Windows arm64
- `package.json`: extension metadata, commands, and settings
- `out/`: compiled JavaScript output (generated, not tracked)

## `apps/lsp-server/` (OCaml Server)

- `lib/config/`: environment parsing and typed runtime/config records
- `lib/syntax/`: AST, lexer/parser, parse + diagnostics pipeline
- `lib/lsp/`: JSON-RPC framing, typed request parsing, and response serialization
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
- `check-ocamlformat.mjs`: cross-platform OCaml format check entrypoint used by root scripts and CI
- `clean-generated.ps1`: removes local build/package artifacts

## Suggested Editing Flow

1. Language parsing/analysis: `apps/lsp-server/lib/syntax/`
2. Workspace/nav behavior: `apps/lsp-server/lib/workspace/`
3. LSP wire-level behavior: `apps/lsp-server/lib/lsp/`
4. VS Code integration UX and runtime wiring: `apps/vscode-extension/src/extension.ts`
5. VS Code commands/config/startup UI: `apps/vscode-extension/src/commands.ts`, `apps/vscode-extension/src/startup_status.ts`, `apps/vscode-extension/src/jovial_config.ts`
