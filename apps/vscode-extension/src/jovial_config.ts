export type WorkspaceDiagnosticsMode = "off" | "errors" | "all";
export type JovialTrace = "off" | "messages" | "verbose";
export type WorkspaceProfileMode = "auto" | "small" | "medium" | "large";
export type RootModel = "auto" | "heuristic" | "manual";

export type JovialConfig = {
  serverPath: string;
  preferBundled: boolean;
  serverArgs: string[];
  autostart: boolean;
  trace: JovialTrace;
  lsifFastPath: boolean;
  workspaceDiagnosticsMode: WorkspaceDiagnosticsMode;
  workspaceProfileMode: WorkspaceProfileMode;
  rootModel: RootModel;
  manualRootFiles: string[];
  backgroundIndexBudgetMs: number;
  backgroundDiagBatchSize: number;
  parseMaxFileBytes: number;
  pressureSoftMb: number;
  pressureCriticalMb: number;
};

export type ConfigSource = {
  get<T>(section: string, defaultValue: T): T;
};

export type JovialInitializationOptions = {
  jovial: {
    workspace: {
      diagnosticsMode: WorkspaceDiagnosticsMode;
      profileMode: WorkspaceProfileMode;
      rootModel: RootModel;
      manualRootFiles: string[];
    };
    background: {
      indexBudgetMs: number;
      diagBatchSize: number;
    };
    server: {
      parseMaxFileBytes: number;
      pressureSoftMb: number;
      pressureCriticalMb: number;
    };
  };
};

export function sanitizeWorkspaceDiagnosticsMode(
  value: unknown,
): WorkspaceDiagnosticsMode {
  return value === "off" || value === "all" || value === "errors"
    ? value
    : "errors";
}

export function sanitizeTrace(value: unknown): JovialTrace {
  return value === "messages" || value === "verbose" || value === "off"
    ? value
    : "off";
}

export function sanitizeWorkspaceProfileMode(
  value: unknown,
): WorkspaceProfileMode {
  return value === "small" ||
    value === "medium" ||
    value === "large" ||
    value === "auto"
    ? value
    : "auto";
}

export function sanitizeRootModel(value: unknown): RootModel {
  return value === "heuristic" || value === "manual" || value === "auto"
    ? value
    : "auto";
}

export function sanitizePositiveInt(value: unknown, fallback: number): number {
  if (typeof value !== "number" || !Number.isFinite(value)) return fallback;
  return Math.max(1, Math.trunc(value));
}

export function sanitizeStringArray(value: unknown): string[] {
  if (!Array.isArray(value)) return [];
  return value
    .filter((item): item is string => typeof item === "string")
    .map((item) => item.trim())
    .filter((item) => item.length > 0);
}

export function readJovialConfig(source: ConfigSource): JovialConfig {
  return {
    serverPath: source.get<string>("server.path", ""),
    preferBundled: source.get<boolean>("server.preferBundled", true),
    serverArgs: sanitizeStringArray(source.get<unknown>("server.args", [])),
    autostart: source.get<boolean>("autostart", true),
    trace: sanitizeTrace(source.get<unknown>("trace", "off")),
    lsifFastPath: source.get<boolean>("lsif.fastPath", false),
    workspaceDiagnosticsMode: sanitizeWorkspaceDiagnosticsMode(
      source.get<unknown>("workspaceDiagnostics.mode", "errors"),
    ),
    workspaceProfileMode: sanitizeWorkspaceProfileMode(
      source.get<unknown>("workspace.profileMode", "auto"),
    ),
    rootModel: sanitizeRootModel(
      source.get<unknown>("workspace.rootModel", "auto"),
    ),
    manualRootFiles: sanitizeStringArray(
      source.get<unknown>("workspace.manualRootFiles", []),
    ),
    backgroundIndexBudgetMs: sanitizePositiveInt(
      source.get<unknown>("background.indexBudgetMs", 8),
      8,
    ),
    backgroundDiagBatchSize: sanitizePositiveInt(
      source.get<unknown>("background.diagBatchSize", 64),
      64,
    ),
    parseMaxFileBytes: sanitizePositiveInt(
      source.get<unknown>("server.parseMaxFileBytes", 16777216),
      16777216,
    ),
    pressureSoftMb: sanitizePositiveInt(
      source.get<unknown>("server.pressureSoftMb", 512),
      512,
    ),
    pressureCriticalMb: sanitizePositiveInt(
      source.get<unknown>("server.pressureCriticalMb", 768),
      768,
    ),
  };
}

export function buildInitializationOptions(
  config: JovialConfig,
): JovialInitializationOptions {
  return {
    jovial: {
      workspace: {
        diagnosticsMode: config.workspaceDiagnosticsMode,
        profileMode: config.workspaceProfileMode,
        rootModel: config.rootModel,
        manualRootFiles: config.manualRootFiles,
      },
      background: {
        indexBudgetMs: config.backgroundIndexBudgetMs,
        diagBatchSize: config.backgroundDiagBatchSize,
      },
      server: {
        parseMaxFileBytes: config.parseMaxFileBytes,
        pressureSoftMb: config.pressureSoftMb,
        pressureCriticalMb: config.pressureCriticalMb,
      },
    },
  };
}
