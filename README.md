# JOVIAL J73 Language Support

Monorepo for a VS Code client extension and an OCaml LSP server for JOVIAL J73.

## Repository At A Glance

- `extension_proj/`: VS Code extension (TypeScript)
- `server_proj/`: language server (OCaml + dune + menhir)
- `docs/`: repository and contributor docs
- `scripts/`: local maintenance utilities

Detailed layout: [`docs/REPO_STRUCTURE.md`](docs/REPO_STRUCTURE.md)

## Quick Start (Development)

Prerequisites:

- Node.js + npm
- OCaml toolchain (`opam`, `dune`)
- VS Code

Build server:

```powershell
cd server_proj
opam exec -- dune build @install
```

Build extension:

```powershell
cd extension_proj
npm ci
npm run compile
```

Run extension in VS Code:

1. Open `extension_proj` in VS Code.
2. Press `F5` to launch Extension Development Host.

## Package VSIX (Bundled Server)

```powershell
cd extension_proj
npm ci
npm run package:vsix
code --install-extension .\jovial-lsp-client-0.0.1.vsix
```

`package:vsix` builds the OCaml server, bundles it into `extension_proj/server/`, compiles the extension, and packages the VSIX.

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
- `jovial.server.args`: extra CLI args for server process
- `jovial.autostart`: auto-start server on file open
- `jovial.trace`: LSP trace level (`off`, `messages`, `verbose`)
- `jovial.lsif.fastPath`: faster non-open-file navigation via workspace index cache

## Repository Hygiene

- Generated outputs are intentionally ignored (`_build/`, `out/`, VSIX files).
- To clean local generated artifacts:

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\clean-generated.ps1
```

For contribution flow and project conventions, see [`CONTRIBUTING.md`](CONTRIBUTING.md).
