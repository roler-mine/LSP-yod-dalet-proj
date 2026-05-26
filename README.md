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
opam exec -- dune runtest --force --verbose
```

Build extension:

```powershell
cd ..\vscode-extension
npm run lint
npm run format:check
npm run check
npm run test:unit
npm run test:integration:pinned
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

Release/universal packaging (requires all bundled runtime binaries present):
```powershell
cd C:\path\to\repo
npm run build:server:win-x64
npm run build:server:win-arm64
npm run build:server:linux-x64
npm run build:server:linux-arm64
npm run package:vsix:release
```

Notes:
- `build:server:win-arm64` must run on a Windows ARM64 host/runner to produce a real ARM64 binary.
- `build:server:linux-x64` and `build:server:linux-arm64` run natively on Linux, or from Windows through WSL when a Linux OCaml toolchain is installed.
- `package:vsix:release` validates binary architecture and will fail if `win32-arm64` is actually x64.

Bundled runtime locations:
- `apps/vscode-extension/runtime/server/win32-x64/jovial-lsp.exe`
- `apps/vscode-extension/runtime/server/win32-arm64/jovial-lsp.exe`
- `apps/vscode-extension/runtime/server/linux-x64/jovial-lsp`
- `apps/vscode-extension/runtime/server/linux-arm64/jovial-lsp`

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
- `jovial.workspaceDiagnostics.mode`: typed workspace diagnostics mode sent through `initialize`
- `jovial.startup.priorityMode`: startup scheduling mode (`balanced` or `infoFirst`)
- `jovial.workspace.profileMode`: workspace sizing profile sent through `initialize`
- `jovial.workspace.rootModel`: root selection mode sent through `initialize`
- `jovial.workspace.manualRootFiles`: manual root-file list for `rootModel=manual`
- `jovial.background.indexBudgetMs`: idle background work budget sent through `initialize`
- `jovial.background.diagBatchSize`: background diagnostic publish batch size sent through `initialize`
- `jovial.server.parseMaxFileBytes`: parse-size guard sent through `initialize`
- `jovial.server.pressureSoftMb`: soft memory pressure threshold sent through `initialize`
- `jovial.server.pressureCriticalMb`: critical memory pressure threshold sent through `initialize`
- `jovial.implementation.*`: optional target profile values such as dialect, word/byte size, default float/fixed precision, max-size hints, and configured system subroutines
- `jovial.features.*`: per-feature booleans for diagnostics, nav, hover, symbols, completion, code actions, inlay hints, and semantic tokens
- `Jovial: Refresh LSIF Cache`: manual command to rebuild LSIF cache when `jovial.lsif.fastPath` is enabled

The supported runtime config path is now `initializationOptions.jovial` with
seven fixed groups:

- `jovial.workspace`
- `jovial.background`
- `jovial.features`
- `jovial.server`
- `jovial.startup`
- `jovial.performance`
- `jovial.implementation`

Environment variables remain as fallbacks and advanced escape hatches for
manual runs, tests, and low-level tuning.

Startup behavior:

- `balanced`: diagnostics and navigation warm together.
- `infoFirst`: hover, goto, references, and other navigation features are prioritized first; diagnostics publish after navigation readiness is reached.
- Open-file hover and navigation readiness targets are 1500 ms by default. Files over 15 MB use the huge-file path and a 10000 ms readiness target while full background indexing continues.
- Treat readiness targets as recorded service goals, not universal pass/fail guarantees for every machine and workspace shape. Full 20 GiB mixed-stress runs on lower-spec laptops can miss the 1500 ms target while still producing responsive provisional hover, navigation, semantic-token, and visible-diagnostic results; compare those runs against machine-local benchmark baselines.

Architecture and performance docs:

- Staged readiness, query authority, module summaries, persistent cache
  authority, fallback scans, CodeLens confidence, and debug-report reading:
  `docs/architecture/staged-workspace-architecture.md`
- Benchmark harness usage and report schema:
  `docs/performance/benchmark-harness.md`
- Symbol-resolution explanation and CodeLens confidence details:
  `docs/architecture/symbol-resolution-debugging.md`

## Testing And CI

- Extension unit tests: `npm --prefix ./apps/vscode-extension run test:unit`
- Extension integration tests: `npm --prefix ./apps/vscode-extension run test:integration:pinned`
- Extension lint/format/typecheck: `npm --prefix ./apps/vscode-extension run lint`, `format:check`, `check`
- Server smoke/stability: `npm run test:server`
- Cross-project local check: `npm run check`

The extension integration runner uses the pinned VS Code version in
`apps/vscode-extension/package.json` (`config.vscodeTestVersion`) and launches a
downloaded test host with temporary user-data and extension directories. The
plain `test:integration` script aliases this pinned path. Override with
`VSCODE_TEST_VERSION=<version>` for another downloaded build, or
`VSCODE_TEST_EXECUTABLE_PATH=<path-to-Code>` only when deliberately bypassing
the downloaded host.

Main CI currently gates:

- server OCaml format check
- server smoke tests
- server stability tests
- extension lint
- extension Prettier format check
- extension typecheck
- extension unit tests
- extension integration tests

Performance suites stay scheduled/manual rather than PR-blocking.

## Repository Hygiene

- Generated outputs are intentionally ignored (`_build/`, `out/`, VSIX files).
- To clean local generated artifacts:

```powershell
powershell -ExecutionPolicy Bypass -File .\tools\scripts\clean-generated.ps1
```
