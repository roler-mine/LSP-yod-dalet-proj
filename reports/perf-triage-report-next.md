# JOVIAL LSP Phase 20 Validation Report

Generated: 2026-05-14

This report validates the Phase 1-19 work in this checkout using local build,
test, integration, and synthetic benchmark commands. It is intentionally
conservative: the benchmark data below comes from generated synthetic workspaces,
not representative customer code.

## Inputs

- Benchmark harness docs: `docs/performance/benchmark-harness.md`
- Spec coverage tracker: `docs/jovial-spec-coverage.md`
- Benchmark JSON: `reports/perf/jovial-lsp-phase20-synthetic.json`
- Benchmark Markdown summary: `reports/perf/jovial-lsp-phase20-synthetic.md`
- Huge smoke benchmark JSON: `reports/perf/jovial-lsp-huge-startup-fix.json`
- Huge smoke benchmark Markdown summary: `reports/perf/jovial-lsp-huge-startup-fix.md`
- Target-huge smoke benchmark JSON: `reports/perf/jovial-lsp-target-huge-smoke.json`
- Target-huge smoke benchmark Markdown summary:
  `reports/perf/jovial-lsp-target-huge-smoke.md`
- Target-huge full-scale benchmark JSON: `reports/perf/jovial-lsp-target-huge.json`
- Target-huge full-scale benchmark Markdown summary:
  `reports/perf/jovial-lsp-target-huge.md`
- Mixed-stress smoke benchmark JSON:
  `reports/perf/jovial-lsp-mixed-stress-smoke.json`
- Mixed-stress smoke benchmark Markdown summary:
  `reports/perf/jovial-lsp-mixed-stress-smoke.md`
- Mixed-stress full-scale benchmark JSON:
  `reports/perf/jovial-lsp-mixed-stress.json`
- Mixed-stress full-scale benchmark Markdown summary:
  `reports/perf/jovial-lsp-mixed-stress.md`
- Mixed-stress 20 GiB / 3 GiB-source benchmark JSON:
  `reports/perf/jovial-lsp-mixed-stress-20g-3gsource-startup12.json`
- Mixed-stress 20 GiB / 3 GiB-source benchmark Markdown summary:
  `reports/perf/jovial-lsp-mixed-stress-20g-3gsource-startup12.md`
- Earlier `docs/perf-triage-report.md` and `docs/performance/perf-triage-report.md`
  were not present in this checkout.

## Validation Commands

| Command | Result | Notes |
| --- | --- | --- |
| `opam exec -- dune build @all --root .` | passed | Menhir still reports existing parser conflicts: 83 shift/reduce states, 6 reduce/reduce states, 453 shift/reduce conflicts, and 66 reduce/reduce conflicts. |
| `opam exec -- dune runtest --root .` | passed | Workspace core suite and LSP-related tests completed successfully. |
| `opam exec -- dune exec --root . apps/lsp-server/test/workspace_core_suite_test.exe --` | passed | Explicit workspace core suite run passed, including Phase 19 macro and field rename safety tests. |
| `npm run check` from `apps/vscode-extension` | passed | TypeScript production and test configs compiled with `--noEmit`. |
| `npm run test:unit` from `apps/vscode-extension` | passed | Unit suites passed: `jovial_config`, `provider_race`, `watched_file_queue`, `workspace_paths`. |
| `npm run test:integration:pinned` from `apps/vscode-extension` | passed | Used pinned VS Code `1.85.2`; updates were disabled by the test environment. VS Code emitted non-fatal extension API proposal noise. |

## Benchmark Command

```powershell
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles small,medium,large --samples 30 --output reports/perf/jovial-lsp-phase20-synthetic.json
```

The main harness run generated synthetic small, medium, and large workspaces and
measured cold startup, warm startup, hover, definition, references,
semantic-token range, open-file diagnostics, memory retention, readiness, and
perf counters. A separate huge smoke run was added after the startup fix.

## Benchmark Summary

| Profile | Files | MB | Cold skeleton ms | Cold local AST ms | Cold full nav ms | Warm full nav ms | Hover p95 ms | Def p95 ms | Refs p95 ms | SemTok p95 ms | Diag p95 ms | Live MB | ASTs retained | Fallback scans | Authority |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| small | 20 | 0.02 | 49.08 | 15.01 | 49.08 | 33.58 | 1.00 | 2.00 | 1.01 | 1.50 | 10.52 | 23 | 20 | 0 | 165 auth / 0 prov |
| medium | 108 | 0.20 | 331.49 | 51.52 | 331.49 | 316.26 | 2.50 | 5.04 | 2.00 | 1.09 | 54.54 | 164 | 108 | 0 | 165 auth / 0 prov |
| large | 352 | 0.98 | 627.18 | 157.22 | 1187.78 | 1091.80 | 3.00 | 20.73 | 2.50 | 1.50 | 19.78 | 596 | 255 | 33 | 165 auth / 0 prov |

The large-profile cold skeleton and cold full-navigation phases no longer time
out in this synthetic run. The fix changes startup readiness for large-profile
workspaces to mean "navigable with enough quick-nav data" rather than "all
background queues and every quick-nav item have drained." For the large cold run,
`quickNavIndexReady=true`, `quickNavIndexComplete=false`, `quickNavIndexed=153`,
`quickNavTotal=352`, and `backgroundDrainRequired=false`; background indexing
continues after the workspace is usable.

The benchmark also now batches persistent skeleton-cache writes during quick-nav
indexing. The large run spent about 44 ms in buffered skeleton-cache writes
instead of repeatedly reloading and rewriting the cache per file.

## Huge Smoke Benchmark

After fixing the large-profile timeout path, the harness `huge` profile was run
separately:

```powershell
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles huge --samples 10 --output reports/perf/jovial-lsp-huge-startup-fix.json
```

| Profile | Files | MB | Cold skeleton ms | Cold local AST ms | Cold full nav ms | Warm full nav ms | Hover p95 ms | Def p95 ms | Refs p95 ms | SemTok p95 ms | Diag p95 ms | Cold timeout | Warm timeout |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- |
| huge | 1064 | 4.85 | 2266.33 | 1142.18 | 2986.90 | 1873.17 | 5.51 | 27.52 | 5.08 | 2.01 | 20.02 | no | no |

For this run, cold readiness reached `quickNavIndexReady=true` after indexing
138 of 1064 files, while `quickNavIndexComplete=false` and
`backgroundDrainRequired=false`. This validates the file-count scaling path, but
the generated files are still far smaller than the target production shape.

## Target-Huge Benchmark Profile

The benchmark harness now has an explicit `target-huge` profile for the intended
production shape. By default it generates about 520 source files with an average
file size of 2 MiB and 5 outlier files at 50 MiB, for roughly 1 GiB of generated
temporary source. It is not included in `all` or `everything`; it must be
requested directly.

Full-scale command:

```powershell
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles target-huge --samples 20 --startup-timeout-ms 120000 --workspace-root C:\temp\jovial-lsp-bench --output reports/perf/jovial-lsp-target-huge.json
```

The full-scale run completed on this machine:

| Profile | Files | MB | Cold skeleton ms | Cold local AST ms | Cold full nav ms | Warm full nav ms | Hover p95 ms | Def p95 ms | Refs p95 ms | SemTok p95 ms | Diag p95 ms | Cold timeout | Warm timeout |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- |
| target-huge | 520 | 1040.01 | 10462.17 | 1524.24 | 35834.99 | 935.63 | 20.00 | 303.18 | 166.60 | 3.00 | 158.60 | no | no |

The generated workspace includes five 50 MiB outlier files:
`MAIN451.j73` through `MAIN455.j73`. At readiness, the run was still in critical
memory pressure and background work was still catching up, which is expected for
this synthetic stress shape. Cold readiness had `quickNavIndexed=277` of 520;
warm readiness had `quickNavIndexed=261` of 520. Both reported
`backgroundDrainRequired=false`, `fullyNavigable=true`, and no startup timeout.

A scaled smoke run was also executed to validate the generator and report shape
without creating the full temporary workspace:

```powershell
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles target-huge --target-files 24 --target-average-mb 0.05 --target-outliers 2 --target-outlier-mb 0.20 --samples 5 --startup-timeout-ms 30000 --output reports/perf/jovial-lsp-target-huge-smoke.json
```

| Profile | Files | MB | Cold skeleton ms | Cold local AST ms | Cold full nav ms | Warm full nav ms | Hover p95 ms | Def p95 ms | Refs p95 ms | SemTok p95 ms | Diag p95 ms | Timeout |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| target-huge smoke | 24 | 1.20 | 430.27 | 59.51 | 430.27 | 212.79 | 5.51 | 7.00 | 5.01 | 3.00 | 41.09 | no |

This smoke result is deliberately not used as a performance claim. It exists to
prove the byte-heavy generator, JSON output, markdown output, and configurable
shape work.

## Mixed-Stress Benchmark Profile

The benchmark harness now also has an explicit `mixed-stress` profile. It is
larger than `target-huge` and combines source-byte pressure with more workspace
shape variation: generated COMPOOLs, generated ICOPY include source files,
normal main files, periodic unresolved ICOPY directives outside `MAIN000`,
64 MiB source outliers, and non-source ballast files under `mixed-noise/`.

Full-scale command:

```powershell
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles mixed-stress --samples 10 --startup-timeout-ms 180000 --workspace-root C:\temp\jovial-lsp-bench --output reports/perf/jovial-lsp-mixed-stress.json
```

The full-scale run wrote a JSON report and Markdown summary:

| Profile | Files | Source MB | Workspace MB | Cold skeleton ms | Cold local AST ms | Cold full nav ms | Warm full nav ms | Hover p95 ms | Def p95 ms | Refs p95 ms | SemTok p95 ms | Diag p95 ms | Cold timeout | Warm timeout |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- |
| mixed-stress | 640 | 1600.04 | 1760.04 | 10132.37 | 1896.78 | 42426.41 | 2018.40 | 38.52 | 296.34 | 167.97 | 3.00 | 168.84 | no | no |

This run was intentionally harsher than the `target-huge` profile. It generated
eight 64 MiB outlier source files: `MAIN381.j73` through `MAIN388.j73`.
Startup did not time out under the 180 s timeout, but cold full-navigation
readiness still missed the 30 s readiness target embedded in the server
readiness report: `readyWithinTarget=false`, `quickNavIndexed=351` of 640,
`quickNavIndexComplete=false`, and `fullyNavigable=true` at readiness.

The request loop exposed the same authority gap more sharply than
`target-huge`: after requests the run recorded 52
`query.cross_module.fallback_scan` samples and 65 provisional authority samples,
with no authoritative cross-module samples in this query mix. Memory pressure
was critical throughout; after requests the debug report showed about 3772 MiB
live, 4 retained ASTs, and 13 shed ASTs.

A scaled smoke run was also executed:

```powershell
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles mixed-stress --stress-files 40 --stress-average-mb 0.08 --stress-outliers 2 --stress-outlier-mb 0.25 --stress-noise-files 8 --stress-noise-mb 0.05 --samples 5 --startup-timeout-ms 30000 --workspace-root C:\temp\jovial-lsp-bench --output reports/perf/jovial-lsp-mixed-stress-smoke.json
```

| Profile | Files | Source MB | Workspace MB | Cold skeleton ms | Cold local AST ms | Cold full nav ms | Warm full nav ms | Hover p95 ms | Def p95 ms | Refs p95 ms | SemTok p95 ms | Diag p95 ms | Timeout |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| mixed-stress smoke | 40 | 3.20 | 3.60 | 1015.49 | 160.29 | 1015.49 | 512.95 | 12.00 | 13.00 | 11.00 | 2.00 | 155.24 | no |

## 20 GiB Mixed Workspace Startup Target

After the 1.6 GiB mixed run, the target workspace shape was adjusted to match
the intended deployment pressure more closely: about 20 GiB logical workspace
size with about 3 GiB of `.j73` source. Non-source ballast is generated as
sparse marker files; the JOVIAL source files are real generated files.

The first 20 GiB / 3 GiB-source run reproduced the remaining startup problem:
cold skeleton was ready in about 6.5 s, but cold full-navigation readiness was
still 67.9 s because startup readiness waited for background/root/import parse
lanes to quiet.

The startup readiness gate was then changed for large workspaces. Large
workspaces now become startup-ready once the source index exists, graph seed is
fresh, the opened document is authoritative, and the quick-nav seed threshold is
met. Root closure, high import promotion from background files, normal parsing,
and full quick-nav indexing continue after readiness instead of blocking it.

Validation command:

```powershell
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles mixed-stress --samples 5 --startup-timeout-ms 180000 --workspace-root C:\temp\jovial-lsp-bench --output reports/perf/jovial-lsp-mixed-stress-20g-3gsource-startup12.json
```

Result:

| Profile | Files | Source MB | Workspace MB | Cold skeleton ms | Cold local AST ms | Cold full nav ms | Warm full nav ms | Hover p95 ms | Def p95 ms | Refs p95 ms | SemTok p95 ms | Diag p95 ms | Cold timeout | Warm timeout |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- |
| mixed-stress 20 GiB | 640 | 3072.01 | 20484.47 | 7018.28 | 2143.22 | 7018.28 | 6358.85 | 99.54 | 379.63 | 1006.42 | 6.00 | 319.49 | no | no |

The 12 s cold full-navigation startup target was met on this synthetic 20 GiB
workspace shape. At readiness, `quickNavIndexed=24` of 640 and background queues
were still active (`highQueuesEmpty=false`, `interactiveQueuesEmpty=false`,
`queuesEmpty=false`). This is intentional for the new readiness contract, but it
means immediately-after-startup references remain expensive: p95 was about
1006 ms in this run.

## What Improved

- A repeatable benchmark harness now writes machine-readable JSON and Markdown
  artifacts under `reports/perf/`.
- VS Code integration testing is pinned and deterministic enough for this local
  run; it did not use a locally updating VS Code installation.
- Public module summaries, public signature hashes, cache hydration, and summary
  invalidation counters are visible in debug/perf output.
- Cross-module query paths expose authority and fallback counters, making
  provisional results observable instead of implicit.
- Spec-aware coverage expanded across compile-time formulas, type checking,
  STATUS values, constant tables, field ownership, ICOPY source models,
  specified tables, overlays, layout facts, INLINE/READONLY metadata,
  implementation parameters, CodeLens confidence, explain-resolution, and
  conservative macro/field rename safety.

## Known Gaps

- Target-specific layout remains unsupported. `Jovial_layout` reports logical
  V1 facts only and does not claim compiler-accurate packing or code generation
  layout.
- Machine-specific semantics remain incomplete. Configured implementation
  parameters and system routines are modeled, but target calling conventions,
  representation legality, and full numeric bounds are still future work.
- Fallback scans remain as a safety net. They did not appear in the small and
  medium measured request loops. The large measured request loop still recorded
  33 fallback scans, so fallback usage remains something to track on real large
  and huge workspaces.
- The old synthetic large fixture is only about 1 MB total. It proves the
  previous cold-start timeout path is fixed for the benchmark harness, but it is
  not representative of the target 500+ file workspace with average 2 MB files
  and occasional 50 MB files.
- The new `target-huge` profile covers that byte-heavy shape and completed
  without startup timeouts, but it still uses generated whitespace-padded source
  rather than customer code.
- Files over the huge-file threshold are expected to stay on guarded
  parse/skeleton paths unless full huge-file parsing is explicitly enabled.
  That protects startup responsiveness, but it also means huge-file semantic
  completeness still needs targeted validation.
- Parser conflict warnings remain in generated parser output and should be
  tracked separately from performance work.
- The huge smoke profile was run for file-count validation, but it is still only
  about 4.85 MB total.
- Target-huge request results remain provisional/fallback-backed in this run:
  92 fallback scans and 115 provisional authority samples were recorded after
  requests. That is now observable, but it is still a performance/authority gap.
- Mixed-stress is larger and more mixed than target-huge, and it completed
  without startup timeout, but cold full navigation was about 42.4 s and missed
  the current 30 s readiness target. It also still relied on fallback scans and
  provisional query authority under pressure.
- The 20 GiB mixed profile now reaches cold full-navigation readiness in about
  7.0 s, but that readiness is an interactive/provisional readiness boundary,
  not proof that all background queues, root closure, or full quick-nav indexing
  have completed.

## Do Not Claim

- Do not claim customer-code large-workspace latency from this report. The
  1.04 GiB target-huge run is full-scale synthetic data, not production source.
- Do not claim every file is fully parsed or every background index is complete
  when large-profile startup readiness is reached. The intended guarantee is
  interactive navigation readiness with background catch-up still in progress.
- Do not claim target-accurate table packing, overlay storage sharing, or
  machine-specific formula semantics.
- Do not claim fallback scans have been eliminated. They are now observable and
  reduced from the previous timing-out large run, but still present.
- Do not claim rename is universally safe for generated macro expansion names.
  Unsafe generated-name rename is rejected by default.
