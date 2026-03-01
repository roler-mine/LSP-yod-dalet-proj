# VSIX Release Packaging

This repository packages a single Windows VSIX that contains both runtime server binaries:

- `runtime/server/win32-x64/jovial-lsp.exe`
- `runtime/server/win32-arm64/jovial-lsp.exe`

## CI Requirements

- Hosted Windows runner for `win32-x64`
- Self-hosted Windows ARM64 runner for `win32-arm64`
- Node.js and npm for extension packaging
- OCaml/opam/dune toolchain for server builds

## Release Flow

1. Build and upload `win32-x64` server binary artifact.
2. Build and upload `win32-arm64` server binary artifact.
3. Download both artifacts into `apps/vscode-extension/runtime/server/...`.
4. Run `npm --prefix apps/vscode-extension run package:vsix:release`.
5. Publish VSIX artifact and SHA256 checksums.
