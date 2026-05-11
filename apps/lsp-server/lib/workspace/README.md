# Workspace Folder Layout

`workspace/` is split by purpose:

- `model/`: `Document`, URI/path handling
- `index/`: text line index + background filesystem compool index
- `core/`: workspace state and shared semantic/navigation foundation
- `features/`: feature endpoints used by the LSP layer (navigation, diagnostics, symbols, semantic tokens, inlay hints, LSIF-lite export)

Entry module:

- `core/workspace.ml` (exposed as module `Workspace`)

## Client-Supplied Settings

The VS Code client now sends its primary runtime settings in LSP `initialize`
options under `initializationOptions.jovial`. Environment variables remain
supported as fallbacks for CLI/manual runs and advanced tuning.

Supported typed groups:

- `initializationOptions.jovial.workspace`
- `initializationOptions.jovial.background`
- `initializationOptions.jovial.features`
- `initializationOptions.jovial.server`
- `initializationOptions.jovial.startup`

Current user-facing typed settings:

- `workspace.diagnosticsMode`
- `workspace.profileMode`
- `workspace.rootModel`
- `workspace.manualRootFiles`
- `background.indexBudgetMs`
- `background.diagBatchSize`
- Diagnostics are core language support and are not disabled by feature profiles.
- `features.definition`
- `features.declaration`
- `features.typeDefinition`
- `features.implementation`
- `features.references`
- `features.documentSymbols`
- `features.workspaceSymbols`
- `features.hover`
- `features.signatureHelp`
- `features.rename`
- `features.completion`
- `features.codeActions`
- `features.inlayHints`
- `features.semanticTokens`
- `server.parseMaxFileBytes`
- `server.pressureSoftMb`
- `server.pressureCriticalMb`
- `startup.priorityMode`

## Settings Flow

The startup and feature-toggle path is intentionally normalized:

1. `lib/lsp/lsp_request.ml` parses `initialize` options from `initializationOptions.jovial`.
2. `lib/config/lsp_runtime_settings.ml` keeps loop/runtime scheduling overrides separate from workspace semantics.
3. `lib/config/workspace_settings.ml` merges environment defaults with client feature flags and startup priority.
4. `lib/workspace/core/workspace_state.ml` snapshots the normalized settings into workspace state during initialization.
5. `lib/lsp/lsp_response.ml` advertises only the capabilities enabled by the normalized feature flags.
6. `lib/lsp/lsp_server.ml` gates request handlers with the same feature flags and defers diagnostics when `startup.priorityMode = infoFirst`.

`infoFirst` keeps hover, goto, references, symbols, and related navigation responsive first, while diagnostics wait until navigation startup readiness is reached. `balanced` keeps the previous mixed startup behavior.

Normal LSP features now flow through typed workspace APIs and the transport
layer serializes them in `lib/lsp/lsp_response.ml`. Standard requests should no
longer depend on `Workspace.*_json_for` exports.

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

## Large-File didOpen + Startup Readiness

- `JOVIAL_DIDOPEN_DEFER_PARSE` (default `true`)
- `JOVIAL_DIDOPEN_DEFER_MIN_DOC_CHARS` (default `120000`)
- `JOVIAL_DIDOPEN_ALWAYS_PROVISIONAL` (default `false`)
- `JOVIAL_DIDOPEN_DISABLE_FOREGROUND_TICK` (default `true`)
- `JOVIAL_STARTUP_TARGET_MS` (default `15000`)
- `JOVIAL_STARTUP_AGGRESSIVE_WINDOW_MS` (default `3000`)
- `JOVIAL_STARTUP_AGGRESSIVE_BG_BUDGET_MS` (default `20`)
- `JOVIAL_STARTUP_FAIR_TICK_MS` (default `2`)
- `JOVIAL_DIAG_MIN_FAIR_TICK_MS` (default `1`)
- `JOVIAL_DIAG_WARMUP_SUPPRESS_XMODULE` (default `true`)

## Background Processing and Workspace Diagnostics

- `JOVIAL_WORKSPACE_DIAGS_MODE` (default `errors`, supported: `off|errors|all`)
- `JOVIAL_BG_TICK_BUDGET_MS` (default `8`)
- `JOVIAL_BG_DIAG_BATCH_SIZE` (default `64`)
- `JOVIAL_BG_IDLE_SLEEP_MS` (default `20`)
- `JOVIAL_BG_SEED_PATHS_PER_TICK` (default `128`)
- `JOVIAL_BG_LARGE_FILE_BYTES` (default `800000`)
- `JOVIAL_BG_LARGE_PARSE_IDLE_QUIET_MS` (default `150`)
- `JOVIAL_WORKSPACE_PROFILE_MODE` (default `auto`, supported: `auto|small|medium|large`)
- `JOVIAL_NAV_SOFT_BUDGET_MS` (default `1800`)
- `JOVIAL_NAV_QUICK_SCAN_FILES` (default `48`)
- `JOVIAL_NAV_QUICK_SCAN_PER_FILE_BYTES` (default `262144`)
- `JOVIAL_NAV_QUICK_SCAN_TOTAL_BYTES` (default `1572864`)
- `JOVIAL_NAV_SOURCE_SCAN_FILES` (legacy fallback env; default `48` if `JOVIAL_NAV_QUICK_SCAN_FILES` is unset)
- `JOVIAL_NAV_MISS_IMPORT_SCAN_MAX_CHARS` (default `262144`)
- `JOVIAL_NAV_MISS_HIGH_ENQUEUE_CAP` (default `24`)
- `JOVIAL_OPEN_DIAG_REVALIDATE_BATCH_SIZE` (default `8`)
- `JOVIAL_ROOT_MODEL` (default `auto`, supported: `auto|heuristic|manual`)
- `JOVIAL_ROOT_HEURISTIC_FALLBACK` (default `true`)
- `JOVIAL_ROOT_MANUAL_FILES` (default empty; `;`-separated absolute paths)
- `JOVIAL_GRAPH_REQUEUE_COOLDOWN_MS` (default `400`)
- `JOVIAL_ROOT_CLOSURE_MAX_DEPTH` (default `4`)
- `JOVIAL_ROOT_CLOSURE_TARGET_FILES` (default `256`)
- `JOVIAL_SKELETON_PREFIX_BYTES` (default `262144`)
- `JOVIAL_SCHED_OPEN_DOC_MIN_SHARE_PCT` (default `50`)
- `JOVIAL_WATCH_COALESCE_TTL_MS` (default `250`)

## Crash-Resilience Controls

- `JOVIAL_INBOX_MAX_ITEMS` (default `2048`)
- `JOVIAL_CLOSED_DOC_LRU_MAX` (default `256`)
- `JOVIAL_PARSE_FILE_MAX_BYTES` (default `16777216`)
- `JOVIAL_PRESSURE_SOFT_MB` (default `512`)
- `JOVIAL_PRESSURE_CRITICAL_MB` (default `768`)
- `JOVIAL_PRESSURE_CHECK_INTERVAL_MS` (default `250`)
- `JOVIAL_LSIF_DOC_LOAD_BUDGET` (default `400`)
- `JOVIAL_LSIF_DOC_LOAD_BUDGET_SOFT` (default `64`)
