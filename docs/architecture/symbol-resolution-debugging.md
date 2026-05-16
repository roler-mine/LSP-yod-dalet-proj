# Symbol Resolution Debugging

For the broader staged workspace model, see
`docs/architecture/staged-workspace-architecture.md`.

CodeLens reference counts carry confidence in their title:

- `12 references` means the count is exact and authoritative.
- `~12 references` means the count is provisional while indexing or cache validation is still warming.
- `12+ references` means the request hit a budget and the count is a lower bound.
- `references pending` means the server could not compute a useful count inside the current budget.

Use `Jovial: Explain Symbol Resolution` in VS Code to inspect the symbol under
the active cursor. The command is read-only and returns a compact JSON report
with the symbol name/key, cursor position, readiness, authority, resolution path,
fallback usage, cache source, and target definition when one is known.

The underlying server command is `jovial.explainSymbolResolution` and accepts
`[uri, line, character]` arguments.

## Debug Reports

The read-only server command `jovial.debugReport` accepts `[uri, maxTokens]`.
CodeLens actions call this command through their `jovial.debugReport` command
payloads.

Start with these fields:

- `documentReadiness`: one of `lexicalOnly`, `skeletonReady`,
  `localAstReady`, `workspaceSemanticReady`, or `crossModuleSemanticReady`.
- `authority`: `provisional` or `authoritative`.
- `reasons`: why the answer is not fully authoritative, for example
  `workspaceIndexWarming`, `semanticStoreMiss`, or `softBudgetExceeded`.
- `navigationStartupReady`: whether the workspace has crossed the interactive
  navigation readiness boundary.
- `moduleSummaryCache`: cache load status, entry counts, provisional vs
  metadata-validated summaries, reverse importer count, and the current
  document's summary authority.
- `crossModule`: semantic hits, summary hits, fallback scan attempts,
  provisional result count, and authoritative result count.
- `counters`: raw perf counters and timing samples.

Useful readings:

- `summaryHitCount` increasing while `fallbackScanCount` stays flat means the
  summary path is replacing broad scans.
- `fallbackScanCount` plus `file_text_load` timings means a lookup still needed
  a file-backed fallback path.
- `metadataValidatedCount < entryCount` means cache data was hydrated but not
  all summaries have been validated against current file metadata yet.
- `authority=provisional` with `readiness=skeletonReady` is expected during
  large-workspace startup.
