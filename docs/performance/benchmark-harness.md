# JOVIAL LSP Benchmark Harness

The workspace benchmark is an explicit opt-in executable. It is built by
`dune build @all`, but it is not attached to `runtest`, so normal unit tests stay
fast.

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

## Profiles

Synthetic workspaces are generated programmatically; no large fixtures are
checked into the repo.

- `small`
- `medium`
- `large`
- `huge`

`all` runs small, medium, and large. `everything` includes huge.

Example:

```sh
opam exec -- dune exec --root . apps/lsp-server/bench/workspace_bench.exe -- --profiles small,medium --samples 60
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

## Useful Options

```sh
--profiles small,medium,large
--samples 40
--startup-timeout-ms 30000
--tick-budget-ms 50
--workspace-root <path>
--out-dir reports/perf
--output reports/perf/my-baseline.json
```

Use the same profile list, sample count, and machine state when comparing future
optimization work.
