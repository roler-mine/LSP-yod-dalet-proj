# Staged Workspace Architecture

This server is intentionally staged. Large workspaces should become useful
before every source file has been fully parsed, while every answer still carries
enough readiness and authority metadata to avoid pretending a warmup result is
complete.

## Readiness Tiers

Query and diagnostic paths use these readiness labels:

- `lexicalOnly`: text and token-level information are available, but parse or
  skeleton data is missing, stale, skipped, or cancelled.
- `skeletonReady`: lightweight declarations, imports, and quick-nav data are
  available. Results are useful for early hover/navigation but are provisional.
- `localAstReady`: the opened document has a current AST and local semantics.
- `workspaceSemanticReady`: workspace-level semantic/index data has reached the
  interactive readiness boundary.
- `crossModuleSemanticReady`: imported/cross-module semantic data is complete
  enough for authoritative cross-module answers.

Authority is separate from readiness:

- `authoritative`: the server believes the result is complete for the current
  scope and budget.
- `provisional`: the result came from skeletons, summaries, warming indexes,
  fallback paths, cancellation, memory pressure, or another incomplete source.

Startup reports also expose two user-facing stages:

- `diagHoverReady`: hover and diagnostics can respond without blocking on the
  whole workspace.
- `fullyNavigable`: interactive navigation is ready. On large profiles this does
  not mean every background queue is drained; background parsing, summary
  validation, and full quick-nav catch-up may continue.

The JSON path to watch is `readiness.stages.*` and
`readiness.components.*` in benchmark/debug reports. Important component fields
include `quickNavIndexReady`, `quickNavIndexed`, `quickNavTotal`,
`openDocsConverged`, `openDocsAuthoritative`, `backgroundDrainRequired`, and
`fullyNavigable`.

Readiness targets are scheduling goals recorded in reports. They are useful for
regression detection on the same machine and workspace shape, but extreme mixed
pressure runs on lower-spec machines can legitimately report
`readyWithinTarget=false` while the editor path remains usable through
provisional results. In those cases, compare startup timings, command
latencies, diagnostics behavior, memory pressure, and readiness components
against the local benchmark artifact.

## Query Facade

Feature endpoints still live in focused modules such as
`Workspace_definition`, `Workspace_references`, and `Workspace_hover`.
External callers should normally enter through `Workspace_query` via
`Workspace.definition_locations_for`, `Workspace.references_locations_for`, and
`Workspace.hover_for`.

`Workspace_query` wraps feature results with:

- readiness
- authority
- reasons such as `workspaceIndexWarming`, `semanticStoreMiss`,
  `softBudgetExceeded`, `cancelled`, or `parseSkippedLargeFile`
- perf counters such as `query.definition`, `query.references`,
  `query.authority.provisional`, and `query.readiness.skeletonready`

The LSP protocol methods still return normal LSP shapes. The extra metadata is
visible through debug reports, CodeLens data, and
`jovial.explainSymbolResolution`.

`Workspace_navigation` is a compatibility boundary only. New internal feature
logic should call the focused feature module or `Workspace_query`; tests enforce
that new library code does not grow fresh dependencies on the compatibility
wrapper.

## Module Summaries

`Module_summary` is the lightweight public surface of a source file. It includes
the source path/URI, content hash, COMPOOL name, exported symbols and types,
imported COMPOOLs, ICOPY targets, public DEFINE macro signatures, and
`public_signature_hash`.

Summaries support two main jobs:

- Dependency invalidation can prune importer revalidation when the public hash
  is unchanged. Body-only or whitespace-only edits should not invalidate
  importers.
- Warm cross-module lookup can answer from summary-backed data before full
  semantic graphs are ready. These results stay provisional unless enough
  semantic/import data proves the target.

Safety rule: incomplete, broken, skipped, or conservative summaries must
invalidate instead of risking stale answers. The older declaration-signature
logic is still retained as a conservative comparison layer.

Useful counters:

- `summary.public_hash_unchanged`
- `summary.public_hash_changed`
- `dep.invalidate.pruned_by_summary`
- `query.cross_module.summary_hit`

## Persistent Cache Authority

Persistent cache files are lightweight and safe to ignore. They live under the
workspace root, mainly:

- `.jovial_ls/cache/source-index.json`
- `.jovial_ls/cache/skeleton-index.json`
- `.jovial_ls/cache/module-summaries.json`
- `.jovial_ls/index/files.json` and related workspace index snapshots

The cache stores source index metadata, quick-nav skeleton entries, module
summaries, public signature hashes, imported COMPOOL lists, exported
symbol/type lists, and reverse importer data. It does not persist full ASTs or
full semantic graphs.

Cache authority is explicit:

- `provisional`: hydrated from cache but not yet metadata-validated in the
  current session.
- `metadataValidated`: file path, metadata, source extensions, parser/indexer
  version, and cache schema checks have passed.

Debug reports expose cache state in `moduleSummaryCache`:

- `loaded`
- `entryCount`
- `provisionalCount`
- `metadataValidatedCount`
- `reverseImporterCompoolCount`
- `documentAuthority`

If a cache is corrupt, stale, or mismatched, the server should ignore it and
rebuild enough data in the background.

## Fallback Scan Behavior

Normal cross-module lookup order is:

1. local scope/navigation data
2. local semantic graph or semantic store
3. imported COMPOOL semantic data
4. imported/open/hydrated module summaries
5. reverse importer graph, when references need importer-side scope
6. bounded fallback scans

Fallback scans are a safety net for cold, broken, partial, or not-yet-indexed
workspaces. They are budgeted and counted with
`query.cross_module.fallback_scan`. Related counters are:

- `query.cross_module.semantic_hit`
- `query.cross_module.summary_hit`
- `query.cross_module.provisional_result`
- `query.cross_module.authoritative_result`

Large-profile startup is stricter: broad source-window fallback scans are
deferred until startup reaches the interactive navigation boundary. During that
window, user-facing queries prefer fast provisional skeleton/summary/index
answers. Small and medium profiles keep compatibility fallback behavior because
the cost is low and many tests/fixtures depend on immediate complete answers.

When investigating fallback counts, correlate `fallbackScanCount` with
`file_text_load`, `nav.soft_budget_exceeded`, and request latency. A nonzero
counter means a fallback path was attempted; it does not always mean the server
performed a large file scan.

## CodeLens Confidence Labels

CodeLens reference/import counts make confidence visible in the title:

- `12 references`: exact authoritative count.
- `~12 references`: provisional count while indexing, cache validation, or
  semantic graph catch-up is still warming.
- `12+ references`: lower bound because a budget stopped the count.
- `references pending`: no useful count was produced inside the current budget.

The lens `data` payload also includes `confidence`, `authority`, `readiness`,
`reasons`, `provisional`, and `countConfidence`.

## Benchmarks

The benchmark executable is opt-in. It is built by `dune build @all` but not
run by `dune runtest`.

Small local baseline:

```sh
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles small --samples 40
```

Large mixed startup pressure run:

```sh
opam exec -- dune exec --display=quiet --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles mixed-stress --stress-realistic-source --stress-active-source-kb 48 --stress-files 1536 --stress-average-mb 2 --stress-outliers 12 --stress-outlier-mb 50 --stress-noise-files 256 --stress-workspace-gb 20 --stress-startup-file-mb 1.50 --startup-request-load --startup-only --startup-timeout-ms 60000 --workspace-root C:\temp\jovial-lsp-bench --output reports/perf/jovial-lsp-mixed-realistic-20g-3gsource-startup.json
```

Important options:

- `--startup-request-load`: rotate diagnostics, hover, semantic-token range,
  definition, and references while startup is warming.
- `--startup-only`: measure cold/warm startup and startup-load probes without
  the post-startup latency sweep.
- `--stress-realistic-source`: generate JOVIAL-like active source plus comment
  ballast instead of whitespace-only padding.
- `--stress-active-source-kb`: cap active non-comment source in each realistic
  stress file.
- `--workspace-root`: reuse generated fixtures across runs.
- `--output`: choose the JSON artifact path; a matching Markdown summary is
  written beside it.

The JSON report is the source of truth. The Markdown file is only a compact
comparison table. Compare runs by profile, source/workspace bytes, startup
stage timings, request percentiles, memory counters, readiness components, and
perf counters.

## Reading Debug Reports

The server command `jovial.debugReport` accepts `[uri, maxTokens]`. CodeLens
actions call it indirectly, and it can also be sent via LSP
`workspace/executeCommand`.

Start with:

- `documentReadiness`, `authority`, and `reasons`: tells you whether this
  document's answer should be trusted as complete.
- `navigationStartupReady`: tells you whether large-profile startup has crossed
  the interactive navigation boundary.
- `moduleSummaryCache`: tells you whether summary cache data is loaded,
  provisional, metadata-validated, and available for reverse-import queries.
- `crossModule`: shows semantic hits, summary hits, fallback attempts,
  provisional results, and authoritative results.
- `counters`: raw perf counters and timing samples for parse, quick-nav,
  cache, query, and budget behavior.

Use `Jovial: Explain Symbol Resolution` for one cursor position. It calls
`jovial.explainSymbolResolution` and returns symbol name/key, position,
readiness, authority, resolution path steps, fallback usage, cache source, and
target definition if one is known.

Common interpretation:

- `readiness=skeletonReady` and `authority=provisional`: early answer from
  skeleton/index/cache; useful but not complete.
- `workspaceIndexWarming` reason: background index or quick-nav catch-up is
  still in progress.
- `summaryHitCount` increasing without fallback scans: summary path is doing
  useful work.
- `fallbackScanCount` increasing with file-load timings: a lookup still needs a
  warmer semantic/summary path.
- `metadataValidatedCount < entryCount`: cache hydration happened, but some
  summaries have not yet been validated for the current session.
