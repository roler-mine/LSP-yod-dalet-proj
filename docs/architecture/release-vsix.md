# VSIX Release Packaging

This repository packages a single VSIX that contains the bundled runtime server binaries for the supported Windows and Linux targets:

- `runtime/server/win32-x64/jovial-lsp.exe`
- `runtime/server/win32-arm64/jovial-lsp.exe`
- `runtime/server/linux-x64/jovial-lsp`
- `runtime/server/linux-arm64/jovial-lsp`

## CI Requirements

- Hosted Windows runner for `win32-x64`
- Self-hosted Windows ARM64 runner for `win32-arm64`
- Linux x64 runner, or Windows x64 runner with WSL and a Linux OCaml toolchain, for `linux-x64`
- Linux ARM64 runner, or Windows ARM64 runner with WSL and a Linux OCaml toolchain, for `linux-arm64`
- Node.js and npm for extension packaging
- OCaml/opam/dune toolchain for server builds

## Release Flow

1. Build and upload `win32-x64` server binary artifact.
2. Build and upload `win32-arm64` server binary artifact.
3. Build and upload `linux-x64` server binary artifact.
4. Build and upload `linux-arm64` server binary artifact.
5. Download all artifacts into `apps/vscode-extension/runtime/server/...`.
6. Run `npm --prefix apps/vscode-extension run package:vsix:release`.
7. Publish VSIX artifact and SHA256 checksums.
