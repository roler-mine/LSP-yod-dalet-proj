import assert from "node:assert/strict";

import {
  buildInitializationOptions,
  readJovialConfig,
  sanitizePositiveInt,
  sanitizeStringArray,
  sanitizeWorkspaceDiagnosticsMode,
} from "../src/jovial_config";

export function run(): void {
  const values: Record<string, unknown> = {
    "server.path": "server.exe",
    "server.preferBundled": false,
    "server.args": ["--trace", " ", 17],
    autostart: false,
    trace: "verbose",
    "lsif.fastPath": true,
    "workspaceDiagnostics.mode": "broken",
    "workspace.profileMode": "medium",
    "workspace.rootModel": "manual",
    "workspace.manualRootFiles": ["main.j73", " ", 7, "ops.j73"],
    "background.indexBudgetMs": 0.8,
    "background.diagBatchSize": -4,
    "server.parseMaxFileBytes": 4096.3,
    "server.pressureSoftMb": 640,
    "server.pressureCriticalMb": 896,
  };

  const cfg = readJovialConfig({
    get(section, defaultValue) {
      return (values[section] as typeof defaultValue) ?? defaultValue;
    },
  });

  assert.deepEqual(cfg, {
    serverPath: "server.exe",
    preferBundled: false,
    serverArgs: ["--trace"],
    autostart: false,
    trace: "verbose",
    lsifFastPath: true,
    workspaceDiagnosticsMode: "errors",
    workspaceProfileMode: "medium",
    rootModel: "manual",
    manualRootFiles: ["main.j73", "ops.j73"],
    backgroundIndexBudgetMs: 1,
    backgroundDiagBatchSize: 1,
    parseMaxFileBytes: 4096,
    pressureSoftMb: 640,
    pressureCriticalMb: 896,
  });

  const init = buildInitializationOptions({
    serverPath: "",
    preferBundled: true,
    serverArgs: [],
    autostart: true,
    trace: "off",
    lsifFastPath: false,
    workspaceDiagnosticsMode: "all",
    workspaceProfileMode: "large",
    rootModel: "heuristic",
    manualRootFiles: ["main.j73"],
    backgroundIndexBudgetMs: 11,
    backgroundDiagBatchSize: 23,
    parseMaxFileBytes: 8192,
    pressureSoftMb: 700,
    pressureCriticalMb: 900,
  });

  assert.deepEqual(init, {
    jovial: {
      workspace: {
        diagnosticsMode: "all",
        profileMode: "large",
        rootModel: "heuristic",
        manualRootFiles: ["main.j73"],
      },
      background: { indexBudgetMs: 11, diagBatchSize: 23 },
      server: {
        parseMaxFileBytes: 8192,
        pressureSoftMb: 700,
        pressureCriticalMb: 900,
      },
    },
  });

  assert.equal(sanitizeWorkspaceDiagnosticsMode("off"), "off");
  assert.equal(sanitizeWorkspaceDiagnosticsMode("oops"), "errors");
  assert.equal(sanitizePositiveInt(7.9, 3), 7);
  assert.equal(sanitizePositiveInt(0, 3), 1);
  assert.deepEqual(sanitizeStringArray(["a", " ", 7, "b"]), ["a", "b"]);
}
