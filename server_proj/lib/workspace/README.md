# Workspace Folder Layout

`workspace/` is split by purpose:

- `model/`: `Document`, URI/path handling
- `index/`: text line index + background filesystem compool index
- `core/`: workspace state and shared semantic/navigation foundation
- `features/`: feature endpoints used by the LSP layer (navigation, diagnostics, symbols, semantic tokens, inlay hints, LSIF-lite export)

Entry module:

- `core/workspace.ml` (exposed as module `Workspace`)
