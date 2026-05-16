# Performance Reports

`workspace_bench.exe` writes timestamped JSON reports and matching Markdown
summaries here by default.

Generated benchmark reports are machine-local artifacts: compare them by schema
fields, profile, source size, startup stages, request latency percentiles, and
memory counters rather than by absolute file name.

Architecture terms used in these reports are documented in
`docs/architecture/staged-workspace-architecture.md`; benchmark command usage is
documented in `docs/performance/benchmark-harness.md`.
