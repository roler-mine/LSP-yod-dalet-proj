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
    "workspace.maxStartupFiles": 2500.8,
    "workspace.extraSourceFileExtensions": [".j", "bad/ext", "J"],
    "background.indexBudgetMs": 0.8,
    "background.diagBatchSize": -4,
    "performance.largeFileThresholdBytes": 131072.9,
    "performance.hugeFileThresholdBytes": 20971520.2,
    "performance.fullSemanticTokensMaxBytes": 1048576.8,
    "performance.fullParseMaxBytes": 5242880.4,
    "performance.enableHugeFileFullParse": true,
    "performance.backgroundParseWorkerCount": 3.9,
    "server.parseMaxFileBytes": 4096.3,
    "server.pressureSoftMb": 640,
    "server.pressureCriticalMb": 896,
    "startup.priorityMode": "infoFirst",
    "features.profile": "responsive",
    "features.hover": false,
    "features.overrides.inlayHints": false,
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
    maxStartupFiles: 2500,
    extraSourceFileExtensions: [".j"],
    sourceExtensions: [".jov", ".j73", ".jvl", ".j"],
    backgroundIndexBudgetMs: 1,
    backgroundDiagBatchSize: 1,
    largeFileThresholdBytes: 131072,
    hugeFileThresholdBytes: 20971520,
    fullSemanticTokensMaxBytes: 1048576,
    fullParseMaxBytes: 5242880,
    enableHugeFileFullParse: true,
    backgroundParseWorkerCount: 3,
    parseMaxFileBytes: 4096,
    pressureSoftMb: 640,
    pressureCriticalMb: 896,
    startupPriorityMode: "infoFirst",
    featureProfile: "responsive",
    customEnabledFeatures: [
      "documentSymbols",
      "workspaceSymbols",
      "hover",
      "signatureHelp",
      "completion",
      "codeActions",
      "inlayHints",
      "semanticTokens",
    ],
    featureOverrides: {
      diagnostics: null,
      documentSymbols: null,
      workspaceSymbols: null,
      hover: false,
      signatureHelp: null,
      completion: null,
      codeActions: null,
      inlayHints: false,
      semanticTokens: null,
    },
    features: {
      diagnostics: true,
      definition: true,
      declaration: true,
      typeDefinition: true,
      implementation: true,
      references: true,
      documentSymbols: true,
      workspaceSymbols: true,
      hover: false,
      signatureHelp: true,
      rename: true,
      completion: true,
      codeActions: false,
      inlayHints: false,
      semanticTokens: false,
    },
  });

  const customCfg = readJovialConfig({
    get(section, defaultValue) {
      const customValues: Record<string, unknown> = {
        "features.profile": "custom",
        "features.custom.enabledFeatures": [
          "diagnostics",
          "hover",
          "completion",
          "inlayHints",
          "hover",
          "notAFeature",
        ],
        "features.diagnostics": true,
        "features.hover": false,
        "features.overrides.completion": false,
      };
      return (customValues[section] as typeof defaultValue) ?? defaultValue;
    },
  });
  assert.equal(customCfg.featureProfile, "custom");
  assert.deepEqual(customCfg.customEnabledFeatures, [
    "hover",
    "completion",
    "inlayHints",
  ]);
  assert.deepEqual(customCfg.features, {
    diagnostics: true,
    definition: true,
    declaration: true,
    typeDefinition: true,
    implementation: true,
    references: true,
    documentSymbols: false,
    workspaceSymbols: false,
    hover: true,
    signatureHelp: false,
    rename: true,
    completion: false,
    codeActions: false,
    inlayHints: true,
    semanticTokens: false,
  });

  const decoupledCfg = readJovialConfig({
    get(section, defaultValue) {
      const decoupledValues: Record<string, unknown> = {
        "features.diagnostics": false,
        "features.overrides.diagnostics": false,
        "features.inlayHints": false,
      };
      return (decoupledValues[section] as typeof defaultValue) ?? defaultValue;
    },
  });
  assert.equal(
    decoupledCfg.features.diagnostics,
    true,
    "Diagnostics stay enabled as core language support",
  );
  assert.equal(
    decoupledCfg.featureOverrides.diagnostics,
    null,
    "Legacy diagnostics overrides are ignored",
  );
  assert.equal(
    decoupledCfg.features.inlayHints,
    false,
    "Inlay hints still obey their own setting",
  );

  const defaultCfg = readJovialConfig({
    get(_section, defaultValue) {
      return defaultValue;
    },
  });
  assert.equal(defaultCfg.maxStartupFiles, 1000);

  const init = buildInitializationOptions(
    {
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
      maxStartupFiles: 1234,
      extraSourceFileExtensions: [],
      sourceExtensions: [".jov", ".j73", ".jvl"],
      backgroundIndexBudgetMs: 11,
      backgroundDiagBatchSize: 23,
      largeFileThresholdBytes: 131072,
      hugeFileThresholdBytes: 20971520,
      fullSemanticTokensMaxBytes: 1048576,
      fullParseMaxBytes: 5242880,
      enableHugeFileFullParse: false,
      backgroundParseWorkerCount: 2,
      parseMaxFileBytes: 8192,
      pressureSoftMb: 700,
      pressureCriticalMb: 900,
      startupPriorityMode: "balanced",
      featureProfile: "minimal",
      customEnabledFeatures: [
        "documentSymbols",
        "workspaceSymbols",
        "hover",
        "signatureHelp",
        "completion",
        "codeActions",
        "inlayHints",
        "semanticTokens",
      ],
      featureOverrides: {
        diagnostics: null,
        documentSymbols: null,
        workspaceSymbols: null,
        hover: true,
        signatureHelp: null,
        completion: null,
        codeActions: null,
        inlayHints: true,
        semanticTokens: false,
      },
      features: {
        diagnostics: true,
        definition: true,
        declaration: true,
        typeDefinition: true,
        implementation: true,
        references: true,
        documentSymbols: true,
        workspaceSymbols: true,
        hover: true,
        signatureHelp: true,
        rename: true,
        completion: true,
        codeActions: true,
        inlayHints: true,
        semanticTokens: false,
      },
    },
    [
      {
        rootUri: "file:///repo",
        fileUris: ["file:///repo/main.j73"],
        searchTruncated: false,
      },
    ],
  );

  assert.deepEqual(init, {
    jovial: {
      workspace: {
        diagnosticsMode: "all",
        profileMode: "large",
        rootModel: "heuristic",
        manualRootFiles: ["main.j73"],
        maxStartupFiles: 1234,
        sourceDiscoveryTruncated: false,
        extraSourceFileExtensions: [],
        sourceFileSets: [
          {
            rootUri: "file:///repo",
            fileUris: ["file:///repo/main.j73"],
            searchTruncated: false,
          },
        ],
      },
      background: { indexBudgetMs: 11, diagBatchSize: 23 },
      features: {
        profile: "minimal",
        overrides: {
          diagnostics: null,
          documentSymbols: null,
          workspaceSymbols: null,
          hover: true,
          signatureHelp: null,
          completion: null,
          codeActions: null,
          inlayHints: true,
          semanticTokens: false,
        },
        diagnostics: true,
        definition: true,
        declaration: true,
        typeDefinition: true,
        implementation: true,
        references: true,
        documentSymbols: true,
        workspaceSymbols: true,
        hover: true,
        signatureHelp: true,
        rename: true,
        completion: true,
        codeActions: true,
        inlayHints: true,
        semanticTokens: false,
      },
      server: {
        parseMaxFileBytes: 8192,
        pressureSoftMb: 700,
        pressureCriticalMb: 900,
      },
      startup: {
        priorityMode: "balanced",
      },
      performance: {
        priorityMode: "balanced",
        largeFileThresholdBytes: 131072,
        hugeFileThresholdBytes: 20971520,
        fullSemanticTokensMaxBytes: 1048576,
        fullParseMaxBytes: 5242880,
        enableHugeFileFullParse: false,
        backgroundParseWorkerCount: 2,
      },
    },
  });

  assert.equal(sanitizeWorkspaceDiagnosticsMode("off"), "off");
  assert.equal(sanitizeWorkspaceDiagnosticsMode("oops"), "errors");
  assert.equal(sanitizePositiveInt(7.9, 3), 7);
  assert.equal(sanitizePositiveInt(0, 3), 1);
  assert.deepEqual(sanitizeStringArray(["a", " ", 7, "b"]), ["a", "b"]);
}
