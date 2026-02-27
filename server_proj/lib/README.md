# `server_proj/lib` Module Map

This folder is organized by server responsibility.

## `syntax/` - Language Front-End

- `ast.*`: core JOVIAL AST and location types
- `lexer.mll` + `parser.mly`: tokenizer/parser
- `parse.*`: parse pipeline and parse result conversion
- `parse_diags.*`: parser diagnostics shaping
- `preprocess.*`: preprocessor directives/import extraction

## `workspace/` - Semantic Workspace Engine

- `model/`: document model and URI/path helpers
- `index/`: text index and workspace filesystem index
- `core/`: workspace state + shared resolution/navigation foundations
- `features/`: user-facing LSP features (definition, references, hover, diagnostics, tokens, symbols, hints)

## `lsp/` - Protocol/Transport Layer

- `lsp_io.*`: JSON-RPC message framing
- `lsp_conv.*`: AST/location to LSP conversion helpers
- `lsp_server.*`: request routing and feature handlers
