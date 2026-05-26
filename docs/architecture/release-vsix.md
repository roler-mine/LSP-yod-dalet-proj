# VSIX Release Packaging

This repository packages a single VSIX that contains the bundled runtime server binaries for the supported Windows and Linux x64 targets:

- `runtime/server/win32-x64/jovial-lsp.exe`
- `runtime/server/linux-x64/jovial-lsp`

ARM64 runtimes can still be packaged with `npm --prefix apps/vscode-extension run package:vsix:release:all` after staging all four runtime binaries.

## CI Requirements

- Hosted Windows runner for `win32-x64`
- Linux x64 runner, or Windows x64 runner with WSL and a Linux OCaml toolchain, for `linux-x64`
- Node.js and npm for extension packaging
- OCaml/opam/dune toolchain for server builds

## Release Flow

1. Build and upload `win32-x64` server binary artifact.
2. Build and upload `linux-x64` server binary artifact.
3. Download both artifacts into `apps/vscode-extension/runtime/server/...`.
4. Run `npm --prefix apps/vscode-extension run package:vsix:release`.
5. Publish VSIX artifact and SHA256 checksums.
