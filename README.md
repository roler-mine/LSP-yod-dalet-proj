# JOVIAL J73 Language Support (VS Code Extension + OCaml LSP Server)

This repository contains:

- `extension_proj`: VS Code extension client (TypeScript)
- `server_proj`: JOVIAL language server (OCaml)
- `docs`: project docs (repository structure and architecture notes)

## Repository Layout

- `extension_proj/src`: extension source code
- `extension_proj/syntaxes`: TextMate grammar
- `server_proj/lib/syntax`: AST, lexer/parser, parse/preprocess pipeline
- `server_proj/lib/lsp`: JSON-RPC/LSP protocol handlers
- `server_proj/lib/workspace`: documents, indices, semantic analysis, navigation
- `server_proj/bin`: server entrypoint
- `server_proj/examples`: sample JOVIAL inputs
- `docs/REPO_STRUCTURE.md`: folder-by-folder guide

## Distribution

- The extension is not published on VS Code Marketplace.
- Use one of:
1. Local development host (`F5` in VS Code extension development)
2. VSIX package install

## Implemented Features

- Parse + semantic diagnostics
- Go to Definition
- Go to Implementation
- Find References
- Hover
- Rename (with prepare-rename)
- Autocomplete (`textDocument/completion`)
- Basic quick fixes (`textDocument/codeAction`)
- Inlay hints
- Semantic tokens
- Document symbols (outline / breadcrumbs)
- Incremental sync for near-live editing
- AST/CST viewer command with graph rendering
1. AST nodes are color-coded by node kind
2. Clicking an AST node jumps to the related source range

## Server Path Setup (Important)

The extension setting is `jovial.server.path`.

### Recommended path in this repo (Windows)

Use the checked-in build binary:

`server_proj\\_build\\default\\bin\\Main.exe`

Workspace settings example (`.vscode/settings.json`):

```json
{
  "jovial.server.path": "server_proj\\_build\\default\\bin\\Main.exe",
  "jovial.server.args": [],
  "jovial.autostart": true
}
```

### Alternate server artifact in this repo

If you use the install-style artifact:

`server_proj\\_build\\install\\default\\bin\\jovial-lsp.exe`

### Absolute path example (Windows)

```json
{
  "jovial.server.path": "C:\\repos\\LSP-yod-dalet-proj\\server_proj\\_build\\default\\bin\\Main.exe"
}
```

### If `jovial.server.path` is empty

The extension auto-probes common paths (repo/workspace aware), including:

- `server_proj\\_build\\default\\bin\\Main.exe`
- `server_proj\\_build\\install\\default\\bin\\jovial-lsp.exe`
- `server\\Main.exe`

On non-Windows it probes non-`.exe` variants as well.

### Path resolution behavior

When `jovial.server.path` is set, the client accepts:

1. Absolute file path
2. `file://` URI
3. Relative path (resolved from detected repo root first, then workspace folders)

You can also use `${workspaceFolder}` / `${workspaceFolder:name}` / `${env:VAR}`.

## Installation

### A) Use existing binary (no OCaml toolchain)

1. Build/install the extension (`extension_proj`), or run it in development host.
2. Set `jovial.server.path` to:
   `server_proj\\_build\\default\\bin\\Main.exe`
3. Run `Jovial: Restart Language Server`.
4. Open a `.jov`, `.j73`, or `.jvl` file.

### B) Build the server yourself (OCaml)

Prerequisites:

- OCaml >= 5.0
- `opam`
- `dune`

Build:

```powershell
cd server_proj
opam install . --deps-only -y
opam exec -- dune build @install
```

Common outputs:

- `server_proj/_build/default/bin/Main.exe`
- `server_proj/_build/install/default/bin/jovial-lsp.exe`

Then set `jovial.server.path` to the binary you want to run.

### C) Build extension

```powershell
cd extension_proj
npm ci
npm run compile
```

Package:

```powershell
npx @vscode/vsce package
```

Install VSIX:

```powershell
code --install-extension .\jovial-lsp-client-0.0.1.vsix
```

## Configuration Reference

- `jovial.server.path`: server executable path
- `jovial.server.args`: extra args passed to server
- `jovial.autostart`: start server automatically
- `jovial.trace`: LSP trace (`off`, `messages`, `verbose`)

## Troubleshooting

### Server not starting

1. Verify `jovial.server.path` points to an existing executable.
2. Run `Jovial: Restart Language Server`.
3. Check output channel `Jovial LSP` for:
   `Resolved server path: ...`

### Features not updating

1. Save the file.
2. Restart language server.
3. Reopen workspace if indexing data became stale.

## Notes

- Language file extensions contributed by default: `.jov`, `.j73`, `.jvl`
- The extension can watch `.j` files, but `.j` is not currently in contributed language extensions.
