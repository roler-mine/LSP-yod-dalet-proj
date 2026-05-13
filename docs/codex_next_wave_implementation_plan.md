# Next-Wave JOVIAL LSP Implementation Plan

This file contains the phase plan only. Use the separate message-list file for copy-paste Codex prompts.

This guide is the next implementation wave after the performance/readiness/spec-coverage phases are complete.

It is based on the current validation state from:

- `perf-triage-report.md`
- `jovial-spec-coverage.md`

The current repo state appears to have:

- passing OCaml build/tests;
- passing VS Code extension check/unit tests;
- VS Code integration tests blocked locally by the VS Code updater rather than compile failure;
- staged readiness, persistent skeleton cache, query facade, feature split, formatting, CodeLens, inlay hints, debug reports, and spec coverage tracker implemented;
- remaining gaps around repeatable benchmarking, cross-module fallback scans, public semantic summaries, full semantic graph persistence, and deeper JOVIAL language correctness.

## Global rules for Codex

Apply these rules to every phase.

1. **Keep public LSP JSON-RPC standard.**
   - Do not replace public LSP transport with a binary protocol.
   - Do not change existing request/response shapes unless the phase explicitly asks for a capability update.

2. **Prefer additive changes.**
   - Avoid removing compatibility wrappers until a phase explicitly asks for cleanup.
   - Preserve current tests unless they are clearly obsolete and replaced with stronger tests.

3. **Use staged authority.**
   - If a result is incomplete, mark it provisional.
   - Do not present fallback scan results as authoritative.
   - Keep skeleton results fast and semantic results truthful.

4. **Respect large-workspace behavior.**
   - Avoid unbounded scans in user-triggered requests.
   - Use budgets and cancellation checks.
   - Prefer cached summaries and semantic graph paths over full rescans.

5. **Update the spec tracker.**
   - Any phase that improves JOVIAL grammar/semantics must update `docs/jovial-spec-coverage.md` or the repo’s current equivalent tracker.

6. **Run tests before finishing.**
   - Minimum:
     - `opam exec -- dune build @all --root .`
     - `opam exec -- dune runtest --root .`
   - If VS Code extension files changed:
     - `npm run check`
     - `npm run test:unit`
   - If integration test runner is fixed:
     - `npm run test:integration`

7. **Record new perf counters.**
   - Any optimization phase should add or update debug/perf counters so behavior can be verified later.

---

# Recommended execution order

| Phase | Name | Main output |
|---:|---|---|
| 1 | Benchmark harness and baseline artifacts | repeatable perf reports |
| 2 | VS Code integration test reliability | pinned/runnable integration tests |
| 3 | Compatibility-wrapper cleanup and architecture lint | safer module boundaries |
| 4 | Module summaries and public signature hash | early cutoff for body-only edits |
| 5 | Cross-module semantic graph adoption | fewer fallback scans |
| 6 | Persistent module-summary cache | faster warm startup and import graph |
| 7 | Compile-time formula evaluator V1 | foundation for table/type diagnostics |
| 8 | JOVIAL type/formula checking V1 | richer semantic correctness |
| 9 | STATUS enum semantics | status value membership/navigation |
| 10 | CONSTANT TABLE support | parser + readonly semantics |
| 11 | Table/block field ownership and presets | safer navigation/rename/diagnostics |
| 12 | ICOPY include source model | dependency/source maps |
| 13 | Specified tables V1 | parser + hover + diagnostics |
| 14 | OVERLAY declarations V1 | parser + AST + shallow diagnostics |
| 15 | Table packing/layout V1 | layout model foundation |
| 16 | INLINE and READONLY declarations | parser + semantics + diagnostics |
| 17 | Implementation parameters and machine-specific subroutines | target/dialect configuration |
| 18 | CodeLens confidence and Explain Resolution | better user/debug feedback |
| 19 | Macro and field rename safety | safer refactors |
| 20 | Final validation report and tracker refresh | evidence and documentation |

---

---

# Phase plan

## Phase 1 — Benchmark harness and baseline artifacts

### Goal

Create a repeatable benchmark suite that produces real performance artifacts. The current report is regression-oriented and explicitly lacks a pre-series baseline and dedicated cold-start benchmark. This phase turns performance into something measurable.

### Primary files / areas to inspect

- apps/lsp-server/lib/workspace/
- apps/lsp-server/lib/lsp/
- apps/lsp-server/test/workspace_core_suite_test.ml
- existing dune files
- existing debug/perf report commands
- docs/perf-triage-report.md if present

### Implementation checklist

1. Add a benchmark directory, preferably:
   - apps/lsp-server/bench/
2. Add a benchmark executable that can create or load synthetic workspaces:
   - small
   - medium
   - large
   - huge
3. Measure at least:
   - cold startup to skeleton-ready
   - cold startup to local-AST-ready
   - cold startup to full-nav-ready if available
   - warm startup with persistent cache
   - hover latency p50/p95/p99
   - definition latency p50/p95/p99
   - references latency p50/p95/p99
   - semantic-token range latency
   - open-file diagnostics latency
   - memory/retained-AST/cache counts if exposed
4. Output machine-readable JSON reports under:
   - reports/perf/
5. Add a simple markdown summary generator or a JSON summary with enough data for manual comparison.
6. Ensure benchmark code does not run during normal `dune runtest` unless explicitly requested.
7. Add docs explaining how to run the benchmark locally.

### Constraints

- Do not make benchmark fixtures huge in the repo. Generate large fixtures programmatically when possible.
- Do not require external customer code.
- Keep normal unit tests fast.
- Do not change LSP behavior.

### Acceptance criteria

- `opam exec -- dune build @all --root .` passes.
- Existing tests pass.
- A benchmark command can be run manually and writes a JSON report.
- The report includes enough fields to compare future runs.

---

## Phase 2 — VS Code integration test reliability

### Goal

The current report says VS Code integration tests were blocked by the local VS Code updater after TypeScript compilation succeeded. Make integration tests runnable and pinned so future client-facing features can be validated.

### Primary files / areas to inspect

- apps/vscode-extension/package.json
- apps/vscode-extension/test/
- apps/vscode-extension/test/fixtures/fake_lsp_server.js
- apps/vscode-extension/src/
- any existing VS Code test runner scripts

### Implementation checklist

1. Pin or configure the VS Code test version used by integration tests.
2. Add a script such as one of:
   - `npm run test:integration:ci`
   - `npm run test:integration:pinned`
3. Avoid using a locally updating VS Code installation when a downloaded stable test version is available.
4. Make integration tests fail with a clear actionable message if VS Code cannot launch.
5. Update package scripts and docs.
6. Add/adjust tests for feature capability alignment if needed:
   - formatting
   - CodeLens
   - inlay hints
   - semantic tokens
   - code actions
   - navigation
7. Ensure fake LSP server and integration fixtures reflect current server capabilities.

### Constraints

- Do not break `npm run test:unit`.
- Do not require developers to manually close VS Code updater processes.
- Keep CI/local behavior deterministic.

### Acceptance criteria

- `npm run check` passes.
- `npm run test:unit` passes.
- A pinned integration-test command exists and is documented.
- If the environment still cannot run VS Code, the failure message is clear and specific.

---

## Phase 3 — Compatibility-wrapper cleanup and architecture lint

### Goal

The report says `workspace_navigation.ml` is now a thin compatibility layer. Keep it only as needed, migrate tests/callers to focused feature modules and `Workspace_query`, then add a guard against future architecture drift.

### Primary files / areas to inspect

- apps/lsp-server/lib/workspace/features/workspace_navigation.ml
- apps/lsp-server/lib/workspace/features/
- apps/lsp-server/lib/workspace/core/workspace_query.ml
- apps/lsp-server/lib/lsp/lsp_server.ml
- apps/lsp-server/test/

### Implementation checklist

1. Find internal callers of compatibility wrappers in `workspace_navigation.ml`.
2. Migrate callers to:
   - focused feature modules, or
   - `Workspace_query`, where appropriate.
3. Keep compatibility wrappers only if external tests or LSP routing still require them.
4. Move tests away from wrappers where possible.
5. Add a lightweight architecture check, test, or documented rule:
   - New internal modules should not depend on `Workspace_navigation` unless explicitly intended.
6. Update docs/comments explaining the compatibility boundary.

### Constraints

- Do not break public LSP behavior.
- Do not delete wrappers if existing tests still need them, unless you update those tests safely.
- Keep changes incremental.

### Acceptance criteria

- Existing tests pass.
- Most internal use routes through focused modules or `Workspace_query`.
- A guardrail exists to prevent new feature logic from accumulating in `workspace_navigation.ml`.

---

## Phase 4 — Module summaries and public signature hash

### Goal

Implement separate-compilation-style summaries and early cutoff. If an edit only changes a procedure body and exported/imported signatures do not change, importers should not be invalidated.

### Primary files / areas to inspect

- apps/lsp-server/lib/workspace/core/workspace_background.ml
- apps/lsp-server/lib/workspace/core/workspace_state.ml
- apps/lsp-server/lib/workspace/index/workspace_index.ml
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/lib/workspace/core/workspace_imports.ml
- apps/lsp-server/lib/workspace/core/workspace_query.ml
- persistent cache modules/files

### Implementation checklist

1. Add a `Module_summary` module.
2. Summary should include at least:
   - source file path/URI
   - file digest or content hash
   - compool name if any
   - exported symbols
   - exported types
   - imported compools
   - ICOPY targets
   - DEFINE public macro signatures
   - public_signature_hash
3. Compute summaries from skeleton/AST/semantic exports where available.
4. Modify document diff/invalidation logic:
   - if public_signature_hash is unchanged, do not invalidate importers
   - if imports/exports/defines/icopy changed, invalidate relevant dependents
5. Add perf counters:
   - `summary.public_hash_unchanged`
   - `summary.public_hash_changed`
   - `dep.invalidate.pruned_by_summary`
6. Add tests:
   - changing only whitespace does not invalidate dependents
   - changing a procedure body does not invalidate importers
   - changing DEF/REF/type/table public declaration invalidates importers
   - changing COMPOOL imports invalidates correctly

### Constraints

- Do not remove existing declaration_signature logic until summary behavior is verified.
- If summary computation is incomplete, mark it conservative and invalidate rather than risk stale results.
- Keep behavior safe under broken/partial parse by using skeleton fallback.

### Acceptance criteria

- Build/tests pass.
- Importer invalidation is pruned for body-only edits.
- Public API changes still invalidate dependents.
- Debug/perf counters expose pruning behavior.

---

## Phase 5 — Cross-module semantic graph adoption and fallback reduction

### Goal

Reduce bounded fallback scans for cross-module lookup by routing through summaries, semantic graph, and reverse import graph.

### Primary files / areas to inspect

- apps/lsp-server/lib/workspace/core/workspace_query.ml
- apps/lsp-server/lib/workspace/core/workspace_semantic_graph.ml if present
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/lib/workspace/core/workspace_imports.ml
- apps/lsp-server/lib/workspace/core/module_summary.ml if Phase 4 created it
- apps/lsp-server/lib/workspace/features/workspace_definition*.ml
- apps/lsp-server/lib/workspace/features/workspace_references*.ml
- apps/lsp-server/test/

### Implementation checklist

1. Add counters:
   - `query.cross_module.semantic_hit`
   - `query.cross_module.summary_hit`
   - `query.cross_module.fallback_scan`
   - `query.cross_module.provisional_result`
   - `query.cross_module.authoritative_result`
2. Update cross-module definition/reference/type lookup:
   - try local scope/semantic graph
   - try imported COMPOOL summaries
   - try reverse importer graph
   - only then use bounded fallback scan
3. Improve readiness/authority tagging:
   - summary-only result => provisional unless enough data proves target
   - semantic graph result => authoritative when scope/import graph is complete
4. Add debug report fields showing fallback-scan counts.
5. Add tests:
   - imported COMPOOL symbol resolves through summary/semantic path
   - fallback scan counter does not increment after warm summary/semantic data is available
   - broken file still returns provisional skeleton result safely
   - cancellation/budget still stops long reference queries

### Constraints

- Keep fallback scan behavior as a safety net.
- Never return an authoritative cross-module result unless the import graph is actually ready.
- Avoid unbounded workspace scans in user-triggered requests.

### Acceptance criteria

- Existing tests pass.
- New tests prove semantic/summary paths are preferred.
- Debug/perf counters expose fallback usage.

---

## Phase 6 — Persistent module-summary cache

### Goal

Persist public module summaries and reverse import data so warm startup avoids rebuilding everything.

### Primary files / areas to inspect

- persistent cache modules/files
- apps/lsp-server/lib/workspace/core/module_summary.ml
- apps/lsp-server/lib/workspace/core/workspace_state.ml
- apps/lsp-server/lib/workspace/index/workspace_index.ml
- apps/lsp-server/test/workspace_core_suite_test.ml

### Implementation checklist

1. Extend persistent cache format to store:
   - module summaries
   - public_signature_hash
   - imported compool list
   - exported symbol/type summary lists
   - reverse importer graph if safe
2. Add versioning to the cache format.
3. Invalidate cache entries when:
   - file path/mtime/size/content hash changes
   - parser/cache version changes
   - source extension config changes
   - implementation/dialect config changes if present
4. Hydrate summaries at startup before full background validation.
5. Mark hydrated summaries as provisional until file metadata validates.
6. Add tests:
   - cache roundtrip
   - corrupt cache ignored
   - changed public signature invalidates entry
   - unchanged body edit preserves public summary
   - warm startup can answer summary-backed definition/import query provisionally

### Constraints

- Do not persist full AST/semantic graph yet.
- Cache must be safe to ignore.
- Corrupt cache must never crash the server.

### Acceptance criteria

- Build/tests pass.
- Warm startup can hydrate summaries.
- Debug report exposes cache counts and cache authority/provisional state.

---

## Phase 7 — Compile-time formula evaluator V1

### Goal

Implement a conservative compile-time formula evaluator. This is foundational for table bounds, type sizes, presets, specified tables, overlays, and implementation parameters.

### Primary files / areas to inspect

- apps/lsp-server/lib/syntax/ast.ml
- apps/lsp-server/lib/syntax/parser.mly
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/lib/workspace/core/jovial_type*.ml if present
- apps/lsp-server/test/

### Implementation checklist

1. Add a module, preferably:
   - apps/lsp-server/lib/workspace/core/jovial_compile_time.ml
2. Define:
   - ctf_value
   - ctf_error
   - ctf_result
   - eval_expr
3. Support V1:
   - integer literals
   - float literals as values but avoid unsafe exact comparisons
   - string/character literals
   - boolean literals
   - simple unary operators
   - simple binary arithmetic for integers
   - relational operators where safe
   - constant item references when available
   - implementation parameters from config if available, otherwise Unknown
4. Recognize but conservatively return Unknown for:
   - LOC
   - NEXT
   - BIT/BYTE
   - LBOUND/UBOUND
   - NWDSEN
   - WORDSIZE/BYTESIZE/BITSIZE
   until their rules are implemented.
5. Add diagnostics helper:
   - if a compile-time expression is required and eval is Unknown, emit a clear diagnostic only when the context requires it.
6. Add tests:
   - table dimension integer expression
   - bit/char size expression
   - bad non-constant expression in constant-required context
   - Unknown does not crash or overdiagnose

### Constraints

- Be conservative.
- Do not implement full numeric target semantics yet.
- Do not make existing valid fixtures fail because of Unknown results.

### Acceptance criteria

- Build/tests pass.
- New evaluator works for simple constant expressions.
- Existing partial semantics remain safe.

---

## Phase 8 — JOVIAL type/formula checking V1

### Goal

Use the compile-time evaluator and richer JOVIAL type model to improve conversion, integer/float/fixed/bit/char/status/table formula diagnostics.

### Primary files / areas to inspect

- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/lib/workspace/core/jovial_type*.ml
- apps/lsp-server/lib/workspace/core/jovial_compile_time.ml
- apps/lsp-server/lib/workspace/core/workspace_symbol_metadata.ml
- apps/lsp-server/test/

### Implementation checklist

1. Add or extend a dedicated typecheck module:
   - jovial_typecheck.ml
   - jovial_conversion.ml
   - jovial_formula_rules.ml
2. Improve type representation for:
   - signed/unsigned integer with optional width
   - float with optional precision
   - fixed with optional scale/fraction
   - bit string with optional width
   - character string with optional length
   - pointer with typed/untyped target
   - table and block entries
3. Implement conservative rules for:
   - arithmetic operators
   - bit operators
   - relational operators
   - assignment compatibility
   - explicit conversion operator compatibility
   - pointer dereference compatibility
4. Add diagnostics with clear messages and hints:
   - explicit conversion required
   - incompatible assignment
   - invalid bit operator operand
   - invalid fixed/integer mixing where known
   - invalid pointer dereference where known
5. Add tests for:
   - integer/float mismatch
   - explicit conversion suppresses mismatch
   - bit length propagation where simple
   - fixed type display and shallow compatibility
   - pointer typed dereference

### Constraints

- Unknown type should suppress hard errors.
- Prefer Warning/Info or no diagnostic where target-specific implementation rules are missing.
- Keep current test fixtures passing.

### Acceptance criteria

- Build/tests pass.
- New diagnostics trigger only in clear cases.
- Hover/inlay/type display remain consistent with richer type model.

---

## Phase 9 — STATUS enum semantics

### Goal

Model `STATUS` value lists and `V(...)` status constants more accurately.

### Primary files / areas to inspect

- apps/lsp-server/lib/syntax/parser.mly
- apps/lsp-server/lib/syntax/ast.ml
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/lib/workspace/core/workspace_symbol_metadata.ml
- apps/lsp-server/lib/workspace/features/
- apps/lsp-server/test/

### Implementation checklist

1. Ensure parser/AST can represent STATUS lists well enough:
   - STATUS (V(A), V(B), ...)
   - status constants as named symbols if possible
2. Add a status model:
   - status type owner
   - status value list
   - value ordinal/order
   - optional representation for future work
3. Add symbol metadata for status constants:
   - `JovialStatusConstant`
   - owning type/item/table if known
4. Implement navigation:
   - hover on V(NAME)
   - goto definition for V(NAME) when known
   - references to V(NAME) within known status type
5. Implement diagnostics:
   - duplicate status value in one list
   - V(NAME) not a member of expected status type
   - ambiguous status constant when multiple status lists contain same value and context cannot disambiguate
6. Add tests:
   - simple status declaration
   - assignment of valid/invalid status value
   - hover/goto on status value
   - duplicate status value diagnostic

### Constraints

- If status context is unknown, avoid hard errors.
- Do not break existing V(...) parsing as builtin constructor.
- Keep representation-specific rules conservative.

### Acceptance criteria

- Build/tests pass.
- STATUS coverage improves in the tracker.
- Status values participate in hover/navigation/diagnostics where clear.

---

## Phase 10 — CONSTANT TABLE support

### Goal

Implement parser, metadata, semantic readonly behavior, and tests for `CONSTANT TABLE`.

### Primary files / areas to inspect

- apps/lsp-server/lib/syntax/parser.mly
- apps/lsp-server/lib/syntax/ast.ml
- apps/lsp-server/lib/workspace/core/workspace_nav_model.ml
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/lib/workspace/core/workspace_symbol_metadata.ml
- apps/lsp-server/lib/workspace/features/
- apps/lsp-server/test/

### Implementation checklist

1. Add parser support for common `CONSTANT TABLE` forms:
   - CONSTANT TABLE NAME(...);
   - CONSTANT TABLE NAME(...) TYPE;
   - CONSTANT TABLE NAME(...) BEGIN ... END
   - table presets where existing parser support allows
2. Represent it in AST as constant data with `DataTable`, or add a more precise AST form if safer.
3. Ensure metadata uses `JovialConstantTable`.
4. Add semantic readonly behavior:
   - assigning to constant table or its elements should be an error when detectable
5. Add hover/inlay display:
   - classification: constant table
   - readonly: yes
   - dimensions/entry type where known
6. Add completion/symbol behavior if needed.
7. Add tests:
   - parsing constant table
   - hover classification
   - readonly assignment diagnostic
   - document/workspace symbol kind
8. Update spec coverage.

### Constraints

- Support common forms first.
- If full presets are not ready, parse conservatively and avoid crashing.
- Do not regress normal TABLE behavior.

### Acceptance criteria

- Build/tests pass.
- CONSTANT TABLE moves from not-started parser/semantics to at least partial/mostly complete where appropriate.

---

## Phase 11 — Table/block field ownership, dimensions, and presets

### Goal

Make TABLE/BLOCK fields first-class owned symbols and improve dimensions/preset diagnostics.

### Primary files / areas to inspect

- apps/lsp-server/lib/syntax/ast.ml
- apps/lsp-server/lib/syntax/parser.mly
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/lib/workspace/core/workspace_query.ml
- apps/lsp-server/lib/workspace/core/workspace_symbol_metadata.ml
- apps/lsp-server/lib/workspace/features/workspace_references*.ml
- apps/lsp-server/test/

### Implementation checklist

1. Introduce field ownership:
   - field symbol belongs to table/block/type owner
   - same field name in different owners must not collide
2. Improve hover:
   - field owner
   - field type
   - table/block context
3. Improve goto definition/references:
   - field references resolve through owner where possible
   - unrelated same-name fields are not merged
4. Improve dimensions:
   - evaluate simple compile-time bounds
   - emit clear diagnostics for invalid simple bounds
   - preserve Unknown for complex target-specific cases
5. Improve presets:
   - validate simple preset count vs table size where possible
   - validate type compatibility of simple preset values
   - support omitted values without false errors
6. Add tests:
   - two tables with same field name do not collide
   - field hover shows owner
   - simple invalid dimension diagnostic
   - simple preset mismatch diagnostic
   - valid omitted preset no error
7. Update spec coverage.

### Constraints

- Avoid overdiagnosing complex table forms.
- Keep existing fallback navigation safe for incomplete owner info.
- Do not attempt specified tables or packing yet.

### Acceptance criteria

- Build/tests pass.
- Field ownership improves rename/reference safety foundations.
- TABLE/BLOCK coverage tracker updated.

---

## Phase 12 — ICOPY include source model

### Goal

Move ICOPY from dependency hinting toward a source-mapped include model.

### Primary files / areas to inspect

- apps/lsp-server/lib/syntax/preprocess.ml
- apps/lsp-server/lib/workspace/core/workspace_state.ml
- apps/lsp-server/lib/workspace/core/workspace_background.ml
- apps/lsp-server/lib/workspace/index/workspace_index.ml
- apps/lsp-server/lib/workspace/core/workspace_imports.ml
- apps/lsp-server/test/

### Implementation checklist

1. Add an include model:
   - include target name/path
   - directive location
   - resolved file path if known
   - source map record type
2. Extend workspace dependency graph:
   - source file -> ICOPY target
   - reverse include users
3. Add source-map support for future expansion:
   - expanded range
   - original source file/range
   - do not fully expand if not safe yet
4. Improve diagnostics:
   - unresolved ICOPY target
   - cyclic include if detectable
5. Improve debug report:
   - include targets
   - reverse include users
6. Add tests:
   - ICOPY dependency indexed
   - changed include invalidates importer
   - unresolved include diagnostic
   - source map record can be created/serialized if cache uses it
7. Update spec coverage.

### Constraints

- Do not fully inline included content into user buffers unless source maps are correct.
- Avoid breaking current ICOPY dependency tracking.
- Keep include expansion optional/provisional.

### Acceptance criteria

- Build/tests pass.
- ICOPY dependency handling becomes more explicit.
- Diagnostics and debug report expose include resolution.

---

## Phase 13 — Specified tables V1

### Goal

Add parser/AST/hover/shallow diagnostics for specified tables, without full layout modeling yet.

### Primary files / areas to inspect

- apps/lsp-server/lib/syntax/parser.mly
- apps/lsp-server/lib/syntax/ast.ml
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/lib/workspace/core/jovial_compile_time.ml
- apps/lsp-server/lib/workspace/features/workspace_hover*.ml
- apps/lsp-server/test/

### Implementation checklist

1. Add AST representation for specified table attributes:
   - table kind W with entry-size
   - table kind V if applicable
   - item POS(startbit,startword) clauses
2. Extend parser for common specified table forms.
3. Add hover display:
   - specified table
   - entry size
   - field positions where known
4. Add shallow diagnostics:
   - invalid POS expression when simple compile-time eval fails
   - missing POS in contexts where clearly required
   - duplicate field POS when clearly duplicated
   - item position outside entry size when simple values are known
5. Add tests:
   - parse specified table W
   - parse item POS
   - hover shows specified table
   - invalid POS diagnostic
6. Update spec coverage.

### Constraints

- Do not implement full packing/layout yet.
- Unknown compile-time values should not produce hard false positives.
- Keep normal TABLE parsing unaffected.

### Acceptance criteria

- Build/tests pass.
- Specified tables move from not-started to partial with tests.

---

## Phase 14 — OVERLAY declarations V1

### Goal

Add parser/AST/hover/shallow diagnostics for OVERLAY declarations.

### Primary files / areas to inspect

- apps/lsp-server/lib/syntax/parser.mly
- apps/lsp-server/lib/syntax/ast.ml
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/lib/workspace/core/workspace_symbol_metadata.ml
- apps/lsp-server/lib/workspace/features/
- apps/lsp-server/test/

### Implementation checklist

1. Add lexer/parser support if token is missing.
2. Add AST representation for:
   - overlay name/targets
   - spacers if common syntax is supported
   - POS(address) absolute overlays if simple
   - nested overlay shape if easily parsed
3. Add document symbols/hover:
   - classification: overlay declaration
   - participating data names
4. Add shallow diagnostics:
   - unknown overlay target
   - duplicate overlay target
   - invalid spacer expression when simple
   - invalid POS expression when simple
5. Add tests:
   - parse simple OVERLAY
   - hover/document symbol
   - unknown target diagnostic
6. Update spec coverage.

### Constraints

- Do not attempt full storage layout sharing yet.
- Keep shallow diagnostics conservative.
- Do not break normal declarations.

### Acceptance criteria

- Build/tests pass.
- OVERLAY moves from not-started to partial.

---

## Phase 15 — Table packing/layout V1

### Goal

Add a layout model foundation for table packing and storage layout diagnostics.

### Primary files / areas to inspect

- apps/lsp-server/lib/workspace/core/jovial_compile_time.ml
- apps/lsp-server/lib/workspace/core/jovial_type*.ml
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- specified table code from Phase 13
- overlay code from Phase 14
- implementation parameter/config modules
- tests

### Implementation checklist

1. Add a layout module:
   - jovial_layout.ml
2. Define:
   - word size / byte size inputs from implementation config
   - item logical size
   - table entry logical size
   - simple field layout entries
3. Support V1:
   - normal table logical sizes where simple
   - specified table POS field records
   - readonly display of layout facts
4. Add hover/debug output:
   - entry size
   - field offset if known
   - unknown when target-specific or too complex
5. Add shallow diagnostics:
   - field overlap when exact positions known
   - field exceeds entry size when exact values known
6. Add tests:
   - simple non-overlap specified table
   - exact overlap diagnostic
   - unknown values do not emit false errors
7. Update spec coverage.

### Constraints

- Do not claim target-accurate code generation layout.
- Keep target-dependent implementation parameters configurable/unknown.
- Avoid hard errors when values are unknown.

### Acceptance criteria

- Build/tests pass.
- Layout model exists and supports future overlay/packing expansion.

---

## Phase 16 — INLINE and READONLY declarations

### Goal

Support INLINE and READONLY declarations in parser, metadata, hover, diagnostics.

### Primary files / areas to inspect

- apps/lsp-server/lib/syntax/lexer.mll
- apps/lsp-server/lib/syntax/parser.mly
- apps/lsp-server/lib/syntax/ast.ml
- apps/lsp-server/lib/workspace/core/workspace_symbol_metadata.ml
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/lib/workspace/features/workspace_hover*.ml
- apps/lsp-server/test/

### Implementation checklist

1. Add explicit AST representation for INLINE declarations or procedure attribute.
2. Add explicit AST representation for READONLY declarations or data attribute.
3. Add metadata flags:
   - is_inline
   - is_readonly
   or equivalent.
4. Hover:
   - procedure is inline
   - data is readonly
5. Diagnostics:
   - assignment to readonly data is an error where detectable
   - invalid INLINE target if obvious
6. Semantic tokens:
   - readonly modifier if current legend supports it
7. Tests:
   - parse INLINE
   - parse READONLY
   - hover shows flags
   - assignment to readonly diagnostic
8. Update spec coverage.

### Constraints

- Do not overinterpret generic attributes.
- Maintain compatibility with existing parsed procedures.
- If syntax variants are uncertain, support common forms first and preserve unknown attributes.

### Acceptance criteria

- Build/tests pass.
- INLINE/READONLY coverage improves.

---

## Phase 17 — Implementation parameters and machine-specific subroutines

### Goal

Add configuration/modeling for implementation parameters and machine-specific subroutine declarations.

### Primary files / areas to inspect

- apps/lsp-server/lib/config/
- apps/vscode-extension/package.json
- apps/vscode-extension/src/jovial_config.ts
- apps/lsp-server/lib/workspace/core/jovial_compile_time.ml
- apps/lsp-server/lib/workspace/core/jovial_layout.ml if present
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- tests

### Implementation checklist

1. Add implementation config fields:
   - bitsInWord
   - bytesInWord if needed
   - floatPrecision default
   - fixedPrecision default
   - maxIntSize/maxBits/maxBytes where useful
   - dialect/profile name
2. Wire config from:
   - environment/settings
   - VS Code settings
3. Use config in:
   - compile-time formula evaluator
   - type display
   - layout model
4. Add machine-specific subroutine model:
   - known builtin/system proc names from config/list
   - mark as `ExternalSystem` metadata
   - suppress unresolved diagnostics for configured system routines
5. Add tests:
   - configured BITSINWORD appears in evaluator
   - builtin/system subroutine suppresses unresolved diagnostic
   - hover shows system/built-in routine
6. Update spec coverage.

### Constraints

- Do not hardcode one target as universal.
- Defaults should preserve current behavior.
- Unknown implementation parameters should yield Unknown, not bad diagnostics.

### Acceptance criteria

- Build/tests pass.
- Settings and server config remain aligned.
- Implementation parameter support improves type/layout/eval behavior.

---

## Phase 18 — CodeLens confidence and Explain Resolution

### Goal

Make provisional counts/results visible and add a debug command to explain symbol resolution.

### Primary files / areas to inspect

- apps/lsp-server/lib/workspace/features/workspace_codelens*.ml
- apps/lsp-server/lib/workspace/core/workspace_query.ml
- apps/lsp-server/lib/workspace/core/workspace_readiness.ml
- apps/lsp-server/lib/lsp/lsp_server.ml
- apps/vscode-extension/src/commands.ts
- apps/vscode-extension/package.json
- tests

### Implementation checklist

1. Update CodeLens title format:
   - exact authoritative: `12 references`
   - provisional: `~12 references`
   - lower-bound/budgeted: `12+ references`
   - unknown/timeout: `references pending`
2. Ensure CodeLens result carries readiness/authority metadata internally.
3. Add `jovial.explainSymbolResolution` server command.
4. Explain output should include:
   - symbol name/key
   - position
   - readiness
   - authority
   - resolution path steps
   - whether fallback scan was used
   - cache source
   - target definition if any
5. Add VS Code command wrapper if appropriate.
6. Add tests:
   - provisional CodeLens title
   - authoritative title
   - explain command JSON shape
   - fallback scan path visible in explanation if forced
7. Update docs.

### Constraints

- Do not slow CodeLens by forcing full reference scans.
- Do not expose huge raw internal graphs; summarize.
- Keep command read-only.

### Acceptance criteria

- Build/tests pass.
- Users can distinguish provisional from exact counts.
- Explain command helps debug lookup behavior.

---

## Phase 19 — Macro and field rename safety

### Goal

Make rename safer for DEFINE macros and table/block fields.

### Primary files / areas to inspect

- apps/lsp-server/lib/workspace/features/workspace_rename*.ml
- apps/lsp-server/lib/workspace/core/workspace_query.ml
- apps/lsp-server/lib/workspace/core/macro_graph*.ml if present
- apps/lsp-server/lib/workspace/core/workspace_semantics.ml
- apps/lsp-server/test/

### Implementation checklist

1. Macro rename:
   - rename DEFINE declaration
   - rename macro call sites
   - do not rename inside strings/comments
   - do not rename generated names unless explicitly safe
   - preserve source maps
2. Field rename:
   - rename only fields owned by the matching table/block/type owner
   - do not rename unrelated fields with same name
3. Add prepare-rename checks:
   - disallow unsafe generated-name rename by default
   - return placeholder/range for safe symbols
4. Add diagnostics or command warnings if rename is partial/provisional.
5. Add tests:
   - macro rename ignores comments/strings
   - macro rename updates DEFINE and uses
   - field rename in one table does not rename same field in another table
   - unsafe generated-name rename is rejected or clearly marked
6. Update spec coverage if rename coverage is tracked.

### Constraints

- Prefer rejecting unsafe rename to doing a wrong rename.
- Do not do text-wide string replacement.
- Use symbol IDs/field ownership/source maps where available.

### Acceptance criteria

- Build/tests pass.
- Rename behavior is conservative and safer for macros/fields.

---

## Phase 20 — Final validation report and tracker refresh

### Goal

Generate a new evidence-based validation report and update spec coverage after the new wave.

### Primary files / areas to inspect

- reports/perf/
- docs/perf-triage-report.md
- docs/jovial-spec-coverage.md
- benchmark harness from Phase 1
- test outputs

### Implementation checklist

1. Run validation commands:
   - `opam exec -- dune build @all --root .`
   - `opam exec -- dune runtest --root .`
   - workspace core suite if separate
   - `npm run check`
   - `npm run test:unit`
   - pinned integration test command if available
2. Run benchmark harness:
   - cold startup
   - warm startup
   - hover
   - definition
   - references
   - semantic tokens
   - diagnostics
   - memory
3. Create or update:
   - reports/perf-triage-report-next.md
   - reports/perf/*.json
4. Update spec coverage:
   - mark changed features accurately
   - keep conservative status values
   - add notes for partial behavior
5. Add a short “known gaps” section:
   - unsupported target-specific layout
   - incomplete machine-specific semantics
   - any remaining fallback scans
   - any integration test blockers
6. Add a “do not claim” section if measurements are still synthetic only.

### Constraints

- Be honest.
- Do not claim production large-workspace latency without representative benchmark data.
- Keep report grounded in actual commands/results.

### Acceptance criteria

- Validation report exists.
- Spec tracker reflects new state.
- Performance JSON artifacts exist.
- Known gaps are documented clearly.

---

# Optional parallel work items

## A. Documentation cleanup

Use this as optional supporting work between major phases. See the message-list file for the exact Codex prompt.

### Scope

You are working in the JOVIAL LSP repo. Add/update documentation for the current staged architecture.

Include:
- readiness tiers
- query facade
- module summaries
- persistent cache authority
- fallback scan behavior
- CodeLens confidence labels
- how to run benchmarks
- how to read debug reports

Do not change implementation behavior.

---

## B. More real-world fixture corpus

Use this as optional supporting work between major phases. See the message-list file for the exact Codex prompt.

### Scope

You are working in the JOVIAL LSP repo. Add a small anonymized/constructed JOVIAL fixture corpus that covers advanced syntax without including proprietary code.

Include:
- COMPOOL imports
- DEF/REF groups
- DEFINE macros with parameters
- TABLE/BLOCK fields
- STATUS values
- conversion operators
- pointer-qualified references
- ICOPY directives
- broken syntax cases

Keep fixtures small. Add tests that use them for parser/nav/diagnostics.

---

## C. Static architecture check

Use this as optional supporting work between major phases. See the message-list file for the exact Codex prompt.

### Scope

You are working in the JOVIAL LSP repo. Add a static architecture check script.

It should detect:
- feature modules importing compatibility wrapper modules when they should use Workspace_query
- new unbounded workspace scans in user-triggered request code
- missing budget/cancellation checks in reference/semantic-token/CodeLens paths
- public LSP handlers bypassing feature flags

Keep this as a warning-style test initially if strict enforcement would be too disruptive.

---

# Kickoff guidance

Use the one-shot kickoff prompt from the message-list file when you want Codex to read the plan but implement only Phase 1.
