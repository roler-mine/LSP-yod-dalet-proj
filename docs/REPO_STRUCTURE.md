# Repository Structure

This repo is organized by responsibility:

## Root

- `README.md`: setup, usage, and troubleshooting
- `.gitignore`: repo-wide ignore rules
- `docs/`: project docs (architecture, contributor guidance)
- `extension_proj/`: VS Code extension client
- `server_proj/`: OCaml language server

## VS Code Client (`extension_proj`)

- `src/`: extension source (`extension.ts`)
- `syntaxes/`: TextMate grammar
- `out/`: compiled JS output
- `package.json`: extension manifest + settings + commands

## LSP Server (`server_proj`)

- `lib/syntax/`: AST + lexer/parser + parse/preprocess modules
- `lib/lsp/`: LSP server, protocol conversion, and framing I/O
- `lib/workspace/`: document model, indexing, semantic checks, and code nav
- `bin/`: server executable entrypoint (`Main.ml`)
- `examples/`: sample JOVIAL files used for testing behavior
- `_build/`: Dune build artifacts (includes prebuilt binary in this repo)

## Suggested Editing Flow

1. Parser/semantic/index logic: `server_proj/lib`
2. LSP transport/request wiring: `server_proj/lib/lsp/lsp_server.ml`
3. VS Code UX/UI/integration: `extension_proj/src/extension.ts`
