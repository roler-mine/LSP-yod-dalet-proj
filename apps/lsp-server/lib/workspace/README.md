# Workspace Folder Layout

`workspace/` is split by purpose:

- `model/`: `Document`, URI/path handling
- `index/`: text line index + background filesystem compool index
- `core/`: workspace state and shared semantic/navigation foundation
- `features/`: feature endpoints used by the LSP layer (navigation, diagnostics, symbols, semantic tokens, inlay hints, LSIF-lite export)

Entry module:

- `core/workspace.ml` (exposed as module `Workspace`)

## didChange Semi-Check Tuning

For incremental `textDocument/didChange`, the server can skip full-file semantic validation
for small ranged edits and keep parsing/import checks fast-path responsive.

- `JOVIAL_DIDCHANGE_SEMI_CHECK` (default `true`)
- `JOVIAL_DIDCHANGE_SEMI_MAX_CHANGES` (default `6`)
- `JOVIAL_DIDCHANGE_SEMI_MAX_LINES` (default `30`)
- `JOVIAL_DIDCHANGE_SEMI_MAX_TEXT_CHARS` (default `1200`)
- `JOVIAL_DIDCHANGE_SEMI_FORCE_FULL_EVERY` (default `20`)

## Large-File didChange Latency Tuning

For very large open files, small ranged edits can be applied as a text/index-only update
to keep nav requests responsive while semantic rebuild is deferred to periodic full passes.

- `JOVIAL_DIDCHANGE_DEFER_PARSE` (default `true`)
- `JOVIAL_DIDCHANGE_DEFER_MIN_DOC_CHARS` (default `120000`)
- `JOVIAL_DIDCHANGE_DEFER_MAX_CHANGES` (default `8`)
- `JOVIAL_DIDCHANGE_DEFER_MAX_INSERTED_CHARS` (default `1800`)
- `JOVIAL_DIDCHANGE_DEFER_FORCE_FULL_EVERY` (default `24`)
