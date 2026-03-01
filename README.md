# JOVIAL J73 Language Support

Monorepo for a VS Code client extension and an OCaml LSP server for JOVIAL J73.

## Repository At A Glance

- `apps/vscode-extension/`: VS Code extension (TypeScript)
- `apps/lsp-server/`: language server (OCaml + dune + menhir)
- `docs/`: repository and contributor docs
- `tools/scripts/`: local build and maintenance utilities

Detailed layout: [`docs/repo/structure.md`](docs/repo/structure.md)

## Quick Start (Development)

Prerequisites:

- Node.js + npm
- OCaml toolchain (`opam`, `dune`)
- VS Code

Install extension dependencies:

```powershell
cd apps/vscode-extension
npm ci
```

Build and test server:

```powershell
cd ..\lsp-server
opam exec -- dune runtest
```

Build extension:

```powershell
cd ..\vscode-extension
npm run compile
```

Run extension in VS Code (development):

1. Open `apps/vscode-extension` in VS Code.
2. Press `F5` to launch Extension Development Host.

## Package VSIX

Developer loop (bundles host-arch server binary, then packages):
```powershell
cd apps/vscode-extension
npm run package:vsix:dev
```

Release/universal packaging (requires both runtime binaries present):
```powershell
cd C:\path\to\repo
npm run build:server:win-x64
npm run build:server:win-arm64
npm run package:vsix:release
```

Notes:
- `build:server:win-arm64` must run on a Windows ARM64 host/runner to produce a real ARM64 binary.
- `package:vsix:release` validates binary architecture and will fail if `win32-arm64` is actually x64.

Bundled runtime locations:
- `apps/vscode-extension/runtime/server/win32-x64/jovial-lsp.exe`
- `apps/vscode-extension/runtime/server/win32-arm64/jovial-lsp.exe`

Install packaged VSIX:
```powershell
code --install-extension .\apps\vscode-extension\jovial-lsp-client-0.0.1.vsix
```

## Implemented LSP Features

- Parse + semantic diagnostics
- Go to Definition / Implementation
- Find References
- Hover
- Rename (with prepare-rename)
- Completion
- Basic code actions
- Inlay hints
- Semantic tokens
- Document symbols
- Incremental sync
- AST/CST viewer command

## Configuration

- `jovial.server.path`: optional server executable override
- `jovial.server.preferBundled`: prefer bundled runtime binaries when available
- `jovial.server.args`: extra CLI args for server process
- `jovial.autostart`: auto-start server on file open
- `jovial.trace`: LSP trace level (`off`, `messages`, `verbose`)
- `jovial.lsif.fastPath`: optional LSIF-like cache for non-open-file nav (default: off to avoid indexing stalls)
- `Jovial: Refresh LSIF Cache`: manual command to rebuild LSIF cache when `jovial.lsif.fastPath` is enabled

## Repository Hygiene

- Generated outputs are intentionally ignored (`_build/`, `out/`, VSIX files).
- To clean local generated artifacts:

```powershell
powershell -ExecutionPolicy Bypass -File .\tools\scripts\clean-generated.ps1
```
