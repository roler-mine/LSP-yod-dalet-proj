# JOVIAL J73 LSP (VS Code Extension + OCaml Server)

Language support for JOVIAL J73 built as:

- A VS Code extension client (`extension_proj`)
- A Language Server Protocol (LSP) server in OCaml (`server_proj`)

This README covers capabilities, limitations, and installation paths for environments with and without OCaml.

## Distribution Status

The VS Code extension is not published on the VS Code Marketplace, and this project currently assumes local distribution only.

Install methods:

- VSIX package install
- Local extension development host

## Repository Layout

- `extension_proj`: VS Code extension (TypeScript)
- `server_proj`: LSP server (OCaml, Menhir, ocamllex)

## Capabilities

### Editor and Language Features

When the extension is connected to a running server, it provides:

- Parse and semantic diagnostics (`publishDiagnostics`)
- Go to Definition
- Go to Implementation
- Find References
- Hover information
- Rename (including prepare-rename support)
- Inlay hints
- Semantic tokens (scope-aware symbols plus lexer-based keywords, strings, numbers, and operators)
- Document Symbols (outline/breadcrumb data)
- Incremental document sync for near-live editing (`textDocumentSync` change kind: incremental)

### JOVIAL-Specific Tooling

- Directive and syntax handling for JOVIAL constructs used by this project (including `COMPOOL`/`ICOMPOOL` and `DEFINE` flows implemented in parser and analysis)
- Workspace scanning and rescanning for cross-file indexing
- Import-aware analysis for compool usage (diagnostics and navigation based on indexed workspace data)

### Tree and Debug Views

VS Code commands provided by the extension:

- `Jovial: Dump AST`
- `Jovial: Dump CST (Token Stream)`
- `Jovial: Show AST/CST Viewer`
- `Jovial: Restart Language Server`

Server execute-command endpoints:

- `jovial.dumpAst`
- `jovial.dumpCst`
- `jovial.debugReport`
- `jovial.rescanWorkspace`

### Syntax Coloring

- TextMate grammar for JOVIAL keywords, directives, operators, identifiers, strings, and comments
- Language ID: `jovial`
- Declared file extensions: `.jov`, `.j73`, `.jvl`

## Limitations

Current code-level limitations to be aware of:

- No completion provider yet (`textDocument/completion` is not implemented)
- No formatting provider yet (`textDocument/formatting` is not implemented)
- No code actions provider yet (`textDocument/codeAction` is not implemented)
- Semantic tokens currently do not emit comment tokens; comment coloring is still handled by TextMate grammar
- The VS Code client assumes the first workspace folder for some path resolution behavior
- Auto-restart is disabled when the server process exits unexpectedly (manual restart command is used)
- `.j` files are watched by the extension file watcher, but `.j` is not currently declared in extension language file extensions
- Parser and semantic rules are still evolving; some edge-case source forms may be incomplete or conservative

## Installation Paths

### 1) Quick setup for this repo (no OCaml, Windows)

Use this if you are working in this repository and want the fastest local setup.

1. Install the extension locally (VSIX or development host).
2. Set `jovial.server.path` to the checked-in prebuilt server binary:
   `${workspaceFolder}\\server_proj\\_build\\default\\bin\\Main.exe`
3. Run `Jovial: Restart Language Server`.
4. Open a `.jov`, `.j73`, or `.jvl` file and verify diagnostics and navigation.

Recommended workspace setting (`.vscode/settings.json`):

```json
{
  "jovial.server.path": "${workspaceFolder}\\server_proj\\_build\\default\\bin\\Main.exe",
  "jovial.server.args": [],
  "jovial.autostart": true
}
```

### 2) Use a custom prebuilt server binary (no OCaml)

Use this if you have a server binary outside this repo.

1. Install the extension locally (VSIX or development host).
2. Set `jovial.server.path` to your binary path.
3. Run `Jovial: Restart Language Server`.

Example user setting (`Preferences: Open User Settings (JSON)`):

```json
{
  "jovial.server.path": "C:\\tools\\jovial-lsp\\jovial-lsp.exe",
  "jovial.server.args": [],
  "jovial.autostart": true
}
```

### 3) Build server from source (OCaml environment)

Use this path if you are developing parser, analysis, or server logic.

Prerequisites:

- OCaml >= 5.0
- `opam`
- `dune` >= 3.11
- Dependencies declared in `server_proj/jovial-lsp.opam` (Menhir, yojson, jsonrpc, lsp, logs, fmt)

Build:

```powershell
cd server_proj
opam install . --deps-only -y
opam exec -- dune build @install
```

Typical outputs:

- `server_proj/_build/default/bin/Main.exe`
- `server_proj/_build/install/default/bin/jovial-lsp.exe` (Windows)
- `server_proj/_build/install/default/bin/jovial-lsp` (Linux/macOS when built there)

Then set `jovial.server.path` to the executable you built and restart the language server.

### 4) Build extension from source

```powershell
cd extension_proj
npm ci
npm run compile
```

Package VSIX:

```powershell
npx @vscode/vsce package
```

Install VSIX:

```powershell
code --install-extension .\jovial-lsp-client-0.0.1.vsix
```

### 5) Path and environment notes

- `jovial.server.path` can be absolute, or relative to `${workspaceFolder}`.
- Windows executable path ends with `.exe`; Linux/macOS does not.
- If you use WSL, SSH, or Dev Containers, `jovial.server.path` must point to a binary in that remote environment.
- The extension is not available on Marketplace; use VSIX or local development install.

## Configuration Reference

Extension settings:

- `jovial.server.path`: path to server executable (default empty; required unless bundled binary exists)
- `jovial.server.args`: extra process args for server
- `jovial.trace`: `off | messages | verbose`
- `jovial.autostart`: auto-start server when JOVIAL file opens

## Using the Server With Other Editors

The server runs over stdio and can be used by any LSP-capable editor or client:

- Process executable: `Main.exe` (repo local build) or `jovial-lsp` / `jovial-lsp.exe` (installed artifact)
- Transport: stdio
- No standalone CLI mode for direct human interaction

## Troubleshooting

### Server not starting

- Check `jovial.server.path` is valid and points to an executable file.
- On Linux or macOS, ensure executable bit is set (`chmod +x jovial-lsp`).
- Check the `Jovial LSP` output channel in VS Code for startup errors.

### Features missing in a file

- Confirm file extension is one of `.jov`, `.j73`, `.jvl`.
- If you use `.j`, add a manual language association in VS Code settings.
- Restart server after major workspace or config changes.

### Navigation or import behavior seems stale

- Trigger `Jovial: Restart Language Server`.
- Save files so workspace indexing sees the latest on-disk changes.

## Status

This is an active, evolving implementation. Parser and analysis behavior will continue to improve as more JOVIAL forms and edge cases are covered.

(this readme was written by AI and reviewed by a human)
