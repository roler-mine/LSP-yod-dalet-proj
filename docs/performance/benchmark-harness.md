# JOVIAL LSP Benchmark Harness

The workspace benchmark is an explicit opt-in executable. It is built by
`dune build @all`, but it is not attached to `runtest`, so normal unit tests stay
fast.

The harness calls the OCaml workspace engine directly. It does not launch VS
Code, the TypeScript extension, the LSP stdio transport, or JSON message
framing; use it for server indexing/query regressions, not client/server wire
throughput.

For end-to-end editor UX timing, use the VS Code benchmark instead. It launches
VS Code's extension test host, activates the TypeScript extension, starts the
real OCaml server over stdio through `vscode-languageclient`, runs provider
commands through VS Code, and measures unsaved live-edit diagnostics.

```sh
npm --prefix apps/vscode-extension run test:e2e-perf
```

That command rebuilds the host server into the dune target build directory,
runs that fresh executable directly, and writes a report to
`reports/perf/jovial-vscode-e2e-<timestamp>.json`. Set
`JOVIAL_E2E_SERVER_PATH=<path-to-jovial-lsp>` to benchmark a specific server
build, or `JOVIAL_E2E_PERF_REPORT=<path>` to choose the output file. When
invoking through `npm --prefix`, use an absolute `JOVIAL_E2E_PERF_REPORT` if
you override the destination; relative paths are resolved from the extension
package directory.

For a huge-file UX smoke run, use:

```sh
npm --prefix apps/vscode-extension run test:e2e-perf:huge
```

The huge profile writes generated JOVIAL procedure bodies into `MAIN.j73`
before `TERM`, opens it through VS Code, enables huge-file full parsing, and
uses a viewport-sized provider range so hover, go-to, completion, inlay hints,
semantic tokens, and edit diagnostics are timed like visible editor
interactions. The default real-code file is 16 MiB and is intentionally heavy;
use `JOVIAL_E2E_HUGE_MAIN_MB=4` for a shorter stepped run. Override the file
size with `JOVIAL_E2E_HUGE_MAIN_MB=<MiB>` or pass `--huge-main-mb <MiB>` to
`run_e2e_perf.js`.

## Run A Local Baseline

From the repository root:

```sh
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles small --samples 40
```

The command writes:

- `reports/perf/jovial-lsp-benchmark-<timestamp>.json`
- `reports/perf/jovial-lsp-benchmark-<timestamp>.md`

The JSON report is the source of truth. The Markdown file is a compact manual
comparison table.

For the architecture terms used in reports, see
`docs/architecture/staged-workspace-architecture.md`.

## Profiles

Synthetic workspaces are generated programmatically; no large fixtures are
checked into the repo.

- `small`
- `medium`
- `large`
- `huge`
- `target-huge`
- `mixed-stress`

`all` runs small, medium, and large. `everything` includes huge.
`target-huge` is intentionally explicit only because its default shape writes
about 1 GiB of temporary source files. `mixed-stress` is also explicit only; it
writes a larger mixed workspace with about 3 GiB of JOVIAL source, ICOPY include
files, COMPOOL imports, unresolved include pressure in some files, 64 MiB source
outliers, and sparse non-source ballast so the workspace reports about 20 GiB
logical size without physically writing all ballast bytes.

Example:

```sh
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles small,medium --samples 60
```

`target-huge` is the representative byte-heavy profile for workspaces around
500+ source files, roughly 2 MiB average source files, and a few large outliers:
the generator writes normal declarations at the top of each file and pads the
rest with whitespace so prefix scans remain syntactically safe.

```sh
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles target-huge --samples 20 --startup-timeout-ms 120000 --workspace-root C:\temp\jovial-lsp-bench --output reports/perf/jovial-lsp-target-huge.json
```

The default target shape is configurable:

```sh
--target-files 520
--target-average-mb 2.0
--target-outliers 5
--target-outlier-mb 50.0
```

For a larger mixed-environment pressure run:

```sh
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles mixed-stress --samples 10 --startup-timeout-ms 180000 --workspace-root C:\temp\jovial-lsp-bench --output reports/perf/jovial-lsp-mixed-stress.json
```

The default mixed shape is configurable:

```sh
--stress-files 640
--stress-average-mb 4.8
--stress-outliers 8
--stress-outlier-mb 64.0
--stress-noise-files 80
--stress-noise-mb 2.0
--stress-workspace-gb 20.0
```

For a realistic 20 GiB mixed-workspace startup pressure run with continuous
requests during startup:

```sh
opam exec -- dune exec --display=quiet --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles mixed-stress --stress-realistic-source --stress-active-source-kb 48 --stress-files 1536 --stress-average-mb 2 --stress-outliers 12 --stress-outlier-mb 50 --stress-noise-files 256 --stress-workspace-gb 20 --stress-startup-file-mb 1.50 --startup-request-load --startup-only --startup-timeout-ms 60000 --workspace-root C:\temp\jovial-lsp-bench --output reports/perf/jovial-lsp-mixed-realistic-20g-3gsource-startup.json
```

When `--stress-workspace-gb` is set, non-source ballast files are sparse marker
files. `sourceBytes` reports actual `.j73` source size; `workspaceBytes` reports
the logical workspace size on disk.

To exercise the same generated workspace through the VS Code extension path
with complex seeded diagnostics, run the e2e mixed profile against the generated
workspace:

```powershell
$env:JOVIAL_E2E_WAIT_TIMEOUT_MS = "60000"
$env:JOVIAL_E2E_PERF_REPORT = "$(Resolve-Path reports/perf)\jovial-vscode-e2e-mixed-local.json"
npm --prefix apps/vscode-extension run test:e2e-perf -- --profile mixed --workspace-root C:\temp\jovial-lsp-bench\mixed-realistic-stress
```

The mixed e2e profile injects a small diagnostic fixture into the large
workspace, opens the 50 MiB stress sample, then measures document symbols,
hover, definition, references, inlay hints, semantic-token range, completion,
visible seeded diagnostics, and unsaved edit-to-diagnostics restore behavior.
The seeded fixture covers imported types/procs, assignment mismatch,
procedure-argument type and count mismatch, unresolved procedure/import,
unknown field access, and invalid pointer dereference.

Lower-spec environments should use these full mixed runs as machine-local
baselines. On an i5-8250U laptop with about 8 GiB RAM, the 1536-file, 3 GiB
source / 20 GiB logical workspace reached cold interactive readiness in about
20.1 s and warm readiness in about 16.0 s, missing the default 1500 ms
readiness target. The matching VS Code e2e run still had no provider command
errors, visible seeded diagnostics in about 544 ms, a full
`jovial.pullDiagnostics` result in about 134 ms, live edit diagnostics in about
555 ms, hover/definition/references/inlay/semantic range p95s under 11 ms,
document symbols p95 around 489 ms, and completion p95 around 378 ms. In that
environment, treat `readyWithinTarget=false` as a capacity signal and compare
against the local artifact rather than treating it as a correctness failure.

For a quick smoke test that exercises the same generator without creating a
large workspace:

```sh
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles target-huge --target-files 24 --target-average-mb 0.05 --target-outliers 2 --target-outlier-mb 0.20 --samples 5 --output reports/perf/jovial-lsp-target-huge-smoke.json
```

For a mixed-stress smoke run:

```sh
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles mixed-stress --stress-files 40 --stress-average-mb 0.08 --stress-outliers 2 --stress-outlier-mb 0.25 --stress-noise-files 8 --stress-workspace-gb 0.02 --samples 5 --output reports/perf/jovial-lsp-mixed-stress-smoke.json
```

To reuse generated fixtures across runs:

```sh
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --workspace-root C:\temp\jovial-lsp-bench --profiles small,medium
```

## Measured Fields

Each profile includes:

- cold startup to skeleton-ready
- cold startup to local-AST-ready
- cold startup to full-nav-ready
- warm startup with persistent cache
- hover latency p50/p95/p99
- definition latency p50/p95/p99
- references latency p50/p95/p99
- semantic-token range latency p50/p95/p99
- open-file diagnostics latency p50/p95/p99
- debug memory counters, scheduler counters, and workspace perf counters

`skeleton-ready` maps to `startup.readiness.components.quickNavIndexReady`.
`local-AST-ready` maps to the opened benchmark document converging.
`full-nav-ready` maps to `Workspace.startup_is_ready_now`.

On large profiles, `full-nav-ready` is the interactive navigation readiness
boundary. It can be true while normal background parse queues, full quick-nav
catch-up, or cache validation continue. Use
`startup.readiness.components.backgroundDrainRequired`,
`quickNavIndexComplete`, `quickNavIndexed`, `quickNavTotal`, and `queuesEmpty`
to distinguish interactive readiness from complete workspace drain.

## Useful Options

```sh
--profiles small,medium,large
--samples 40
--startup-timeout-ms 30000
--tick-budget-ms 50
--startup-request-load
--startup-only
--target-files 520
--target-average-mb 2.0
--target-outliers 5
--target-outlier-mb 50.0
--stress-files 640
--stress-average-mb 4.8
--stress-outliers 8
--stress-outlier-mb 64.0
--stress-noise-files 80
--stress-noise-mb 2.0
--stress-workspace-gb 20.0
--stress-startup-file-mb 1.50
--stress-realistic-source
--stress-active-source-kb 48
--workspace-root <path>
--out-dir reports/perf
--output reports/perf/my-baseline.json
```

Use the same profile list, sample count, and machine state when comparing future
optimization work.

## Reading Benchmark JSON

Key paths:

- `runs[].startup.cold` and `runs[].startup.warm`: staged startup timing,
  readiness components, scheduler state, memory state, and perf counters.
- `runs[].latencies`: post-startup p50/p95/p99 request latencies unless
  `--startup-only` was used.
- `startupLoad`: request latencies captured while startup was still warming
  when `--startup-request-load` was enabled.
- `perfStats.metrics`: counters and timings such as
  `quick_nav.index_file`, `parse.open_doc_forced`,
  `index.startup.load_or_build_source`, `query.cross_module.fallback_scan`,
  `query.cross_module.summary_hit`, and `nav.soft_budget_exceeded`.

The Markdown summary is intentionally small. Use the JSON to compare authority,
fallback, memory, and readiness details.
