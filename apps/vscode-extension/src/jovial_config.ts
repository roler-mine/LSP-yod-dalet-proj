import {
  sanitizeSourceExtensions,
  sourceExtensionsWithDefaults,
} from "./source_extensions";

export type WorkspaceDiagnosticsMode = "off" | "errors" | "all";
export type JovialTrace = "off" | "messages" | "verbose";
export type WorkspaceProfileMode = "auto" | "small" | "medium" | "large";
export type RootModel = "auto" | "heuristic" | "manual";
export type StartupPriorityMode = "balanced" | "navigationFirst" | "infoFirst";
export type FeatureProfile = "full" | "responsive" | "minimal" | "custom";

export type JovialFeatureFlags = {
  diagnostics: boolean;
  definition: boolean;
  declaration: boolean;
  typeDefinition: boolean;
  implementation: boolean;
  references: boolean;
  documentSymbols: boolean;
  workspaceSymbols: boolean;
  hover: boolean;
  signatureHelp: boolean;
  rename: boolean;
  completion: boolean;
  codeActions: boolean;
  inlayHints: boolean;
  semanticTokens: boolean;
};

export type JovialPassiveFeatureOverrides = {
  diagnostics: boolean | null;
  documentSymbols: boolean | null;
  workspaceSymbols: boolean | null;
  hover: boolean | null;
  signatureHelp: boolean | null;
  completion: boolean | null;
  codeActions: boolean | null;
  inlayHints: boolean | null;
  semanticTokens: boolean | null;
};

export type JovialPassiveFeatureName = keyof JovialPassiveFeatureOverrides;

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
  maxStartupFiles: number;
  backgroundIndexBudgetMs: number;
  backgroundDiagBatchSize: number;
  largeFileThresholdBytes: number;
  hugeFileThresholdBytes: number;
  fullSemanticTokensMaxBytes: number;
  fullParseMaxBytes: number;
  enableHugeFileFullParse: boolean;
  backgroundParseWorkerCount: number;
  parseMaxFileBytes: number;
  pressureSoftMb: number;
  pressureCriticalMb: number;
  startupPriorityMode: StartupPriorityMode;
  featureProfile: FeatureProfile;
  customEnabledFeatures: JovialPassiveFeatureName[];
  featureOverrides: JovialPassiveFeatureOverrides;
  extraSourceFileExtensions: string[];
  sourceExtensions: string[];
  features: JovialFeatureFlags;
};

export type JovialSourceFileSet = {
  workspaceUri?: string;
  rootUri: string;
  fileUris: string[];
  searchTruncated: boolean;
  inferred?: boolean;
  reason?: "configured" | "minimal-common-container" | "manual" | "rescan";
  flatNamespace?: true;
};

type JovialPassiveFeatureFlags = Record<JovialPassiveFeatureName, boolean>;

const passiveFeatureNames: readonly JovialPassiveFeatureName[] = [
  "diagnostics",
  "documentSymbols",
  "workspaceSymbols",
  "hover",
  "signatureHelp",
  "completion",
  "codeActions",
  "inlayHints",
  "semanticTokens",
];

const configurablePassiveFeatureNames: readonly JovialPassiveFeatureName[] =
  passiveFeatureNames.filter((name) => name !== "diagnostics");

function allPassiveFeatureFlags(enabled: boolean): JovialPassiveFeatureFlags {
  return {
    diagnostics: enabled,
    documentSymbols: enabled,
    workspaceSymbols: enabled,
    hover: enabled,
    signatureHelp: enabled,
    completion: enabled,
    codeActions: enabled,
    inlayHints: enabled,
    semanticTokens: enabled,
  };
}

function passiveFlagsForProfile(
  profile: FeatureProfile,
  customEnabledFeatures: readonly JovialPassiveFeatureName[],
): JovialPassiveFeatureFlags {
  switch (profile) {
    case "responsive":
      return {
        ...allPassiveFeatureFlags(true),
        codeActions: false,
        inlayHints: false,
        semanticTokens: false,
      };
    case "minimal":
      return {
        ...allPassiveFeatureFlags(true),
        documentSymbols: false,
        workspaceSymbols: false,
        signatureHelp: false,
        completion: false,
        codeActions: false,
        inlayHints: false,
        semanticTokens: false,
      };
    case "custom": {
      const selected = new Set<JovialPassiveFeatureName>(customEnabledFeatures);
      return {
        diagnostics: selected.has("diagnostics"),
        documentSymbols: selected.has("documentSymbols"),
        workspaceSymbols: selected.has("workspaceSymbols"),
        hover: selected.has("hover"),
        signatureHelp: selected.has("signatureHelp"),
        completion: selected.has("completion"),
        codeActions: selected.has("codeActions"),
        inlayHints: selected.has("inlayHints"),
        semanticTokens: selected.has("semanticTokens"),
      };
    }
    case "full":
    default:
      return allPassiveFeatureFlags(true);
  }
}

function applyPassiveOverrides(
  base: JovialPassiveFeatureFlags,
  overrides: JovialPassiveFeatureOverrides,
): JovialPassiveFeatureFlags {
  return {
    diagnostics: true,
    documentSymbols: overrides.documentSymbols ?? base.documentSymbols,
    workspaceSymbols: overrides.workspaceSymbols ?? base.workspaceSymbols,
    hover: overrides.hover ?? base.hover,
    signatureHelp: overrides.signatureHelp ?? base.signatureHelp,
    completion: overrides.completion ?? base.completion,
    codeActions: overrides.codeActions ?? base.codeActions,
    inlayHints: overrides.inlayHints ?? base.inlayHints,
    semanticTokens: overrides.semanticTokens ?? base.semanticTokens,
  };
}

export type ConfigSource = {
  get<T>(section: string, defaultValue: T): T;
  inspect?<T>(section: string):
    | {
        globalValue?: T;
        workspaceValue?: T;
        workspaceFolderValue?: T;
        globalLanguageValue?: T;
        workspaceLanguageValue?: T;
        workspaceFolderLanguageValue?: T;
      }
    | undefined;
};

export type JovialInitializationOptions = {
  jovial: {
    workspace: {
      diagnosticsMode: WorkspaceDiagnosticsMode;
      profileMode: WorkspaceProfileMode;
      rootModel: RootModel;
      manualRootFiles: string[];
      maxStartupFiles: number;
      sourceDiscoveryTruncated: boolean;
      extraSourceFileExtensions: string[];
      sourceFileSets: JovialSourceFileSet[];
    };
    background: {
      indexBudgetMs: number;
      diagBatchSize: number;
    };
    features: Partial<JovialFeatureFlags> & {
      profile: FeatureProfile;
      overrides: JovialPassiveFeatureOverrides;
    };
    server: {
      parseMaxFileBytes: number;
      pressureSoftMb: number;
      pressureCriticalMb: number;
    };
    startup: {
      priorityMode: StartupPriorityMode;
    };
    performance: {
      priorityMode: StartupPriorityMode;
      largeFileThresholdBytes: number;
      hugeFileThresholdBytes: number;
      fullSemanticTokensMaxBytes: number;
      fullParseMaxBytes: number;
      enableHugeFileFullParse: boolean;
      backgroundParseWorkerCount: number;
    };
  };
};

function readOptional<T>(source: ConfigSource, section: string): T | undefined {
  return source.get<T | undefined>(section, undefined);
}

function readOptionalBoolean(
  source: ConfigSource,
  section: string,
): boolean | undefined {
  const value = readOptional<unknown>(source, section);
  return typeof value === "boolean" ? value : undefined;
}

function readConfiguredOptionalBoolean(
  source: ConfigSource,
  section: string,
): boolean | undefined {
  const inspect = source.inspect?.<unknown>(section);
  if (inspect) {
    const values = [
      inspect.workspaceFolderLanguageValue,
      inspect.workspaceLanguageValue,
      inspect.globalLanguageValue,
      inspect.workspaceFolderValue,
      inspect.workspaceValue,
      inspect.globalValue,
    ];
    for (const value of values) {
      if (typeof value === "boolean") return value;
    }
    return undefined;
  }
  return readOptionalBoolean(source, section);
}

function readConfiguredOptionalString(
  source: ConfigSource,
  section: string,
): string | undefined {
  const inspect = source.inspect?.<unknown>(section);
  if (inspect) {
    const values = [
      inspect.workspaceFolderLanguageValue,
      inspect.workspaceLanguageValue,
      inspect.globalLanguageValue,
      inspect.workspaceFolderValue,
      inspect.workspaceValue,
      inspect.globalValue,
    ];
    for (const value of values) {
      if (typeof value === "string") return value;
    }
    return undefined;
  }
  const value = readOptional<unknown>(source, section);
  return typeof value === "string" ? value : undefined;
}

function oneOf<T extends string>(
  value: unknown,
  allowed: readonly T[],
  fallback: T,
): T {
  return typeof value === "string" && allowed.includes(value as T)
    ? (value as T)
    : fallback;
}

const workspaceDiagnosticsModes = ["off", "errors", "all"] as const;
const traceModes = ["off", "messages", "verbose"] as const;
const workspaceProfileModes = ["auto", "small", "medium", "large"] as const;
const rootModels = ["auto", "heuristic", "manual"] as const;
const startupPriorityModes = [
  "balanced",
  "navigationFirst",
  "infoFirst",
] as const;
const featureProfiles = ["full", "responsive", "minimal", "custom"] as const;

export function sanitizeWorkspaceDiagnosticsMode(
  value: unknown,
): WorkspaceDiagnosticsMode {
  return oneOf(value, workspaceDiagnosticsModes, "errors");
}

export function sanitizeTrace(value: unknown): JovialTrace {
  return oneOf(value, traceModes, "off");
}

export function sanitizeWorkspaceProfileMode(
  value: unknown,
): WorkspaceProfileMode {
  return oneOf(value, workspaceProfileModes, "auto");
}

export function sanitizeRootModel(value: unknown): RootModel {
  return oneOf(value, rootModels, "auto");
}

export function sanitizeStartupPriorityMode(
  value: unknown,
): StartupPriorityMode {
  return oneOf(value, startupPriorityModes, "navigationFirst");
}

export function sanitizeFeatureProfile(value: unknown): FeatureProfile {
  return oneOf(value, featureProfiles, "full");
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

export function sanitizeCustomEnabledFeatures(
  value: unknown,
): JovialPassiveFeatureName[] {
  if (!Array.isArray(value)) return [...configurablePassiveFeatureNames];
  const allowed = new Set<string>(configurablePassiveFeatureNames);
  const out: JovialPassiveFeatureName[] = [];
  for (const item of value) {
    if (typeof item !== "string" || !allowed.has(item)) continue;
    const feature = item as JovialPassiveFeatureName;
    if (!out.includes(feature)) out.push(feature);
  }
  return out;
}

export function readJovialConfig(source: ConfigSource): JovialConfig {
  const featureProfile = sanitizeFeatureProfile(
    source.get<unknown>("features.profile", "full"),
  );
  const customEnabledFeatures = sanitizeCustomEnabledFeatures(
    source.get<unknown>("features.custom.enabledFeatures", [
      ...configurablePassiveFeatureNames,
    ]),
  );
  const readPassiveOverride = (
    name: keyof JovialPassiveFeatureOverrides,
    legacySection: string,
    legacySections: string[] = [],
  ): boolean | null => {
    const value = readOptionalBoolean(source, `features.overrides.${name}`);
    if (value !== undefined) return value;
    if (featureProfile === "custom") return null;
    for (const candidate of [legacySection, ...legacySections]) {
      const legacy = readConfiguredOptionalBoolean(source, candidate);
      if (legacy !== undefined) return legacy;
    }
    return null;
  };
  const featureOverrides: JovialPassiveFeatureOverrides = {
    diagnostics: null,
    documentSymbols: readPassiveOverride(
      "documentSymbols",
      "features.documentSymbols",
    ),
    workspaceSymbols: readPassiveOverride(
      "workspaceSymbols",
      "features.workspaceSymbols",
    ),
    hover: readPassiveOverride("hover", "features.hover"),
    signatureHelp: readPassiveOverride(
      "signatureHelp",
      "features.signatureHelp",
    ),
    completion: readPassiveOverride("completion", "features.completion"),
    codeActions: readPassiveOverride("codeActions", "features.codeActions"),
    inlayHints: readPassiveOverride("inlayHints", "features.inlayHints", [
      "server.inlayHints",
      "server.inlayhints",
      "server.InlayHint",
    ]),
    semanticTokens: readPassiveOverride(
      "semanticTokens",
      "features.semanticTokens",
    ),
  };
  const extraSourceFileExtensions = sanitizeSourceExtensions(
    source.get<unknown>("workspace.extraSourceFileExtensions", []),
  );
  const sourceExtensions = sourceExtensionsWithDefaults(
    extraSourceFileExtensions,
  );
  const passiveFeatures = applyPassiveOverrides(
    passiveFlagsForProfile(featureProfile, customEnabledFeatures),
    featureOverrides,
  );
  const features: JovialFeatureFlags = {
    diagnostics: true,
    definition: true,
    declaration: true,
    typeDefinition: true,
    implementation: true,
    references: true,
    documentSymbols: passiveFeatures.documentSymbols,
    workspaceSymbols: passiveFeatures.workspaceSymbols,
    hover: passiveFeatures.hover,
    signatureHelp: passiveFeatures.signatureHelp,
    rename: true,
    completion: passiveFeatures.completion,
    codeActions: passiveFeatures.codeActions,
    inlayHints: passiveFeatures.inlayHints,
    semanticTokens: passiveFeatures.semanticTokens,
  };

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
    maxStartupFiles: sanitizePositiveInt(
      source.get<unknown>("workspace.maxStartupFiles", 1000),
      1000,
    ),
    backgroundIndexBudgetMs: sanitizePositiveInt(
      source.get<unknown>("background.indexBudgetMs", 8),
      8,
    ),
    backgroundDiagBatchSize: sanitizePositiveInt(
      source.get<unknown>("background.diagBatchSize", 64),
      64,
    ),
    largeFileThresholdBytes: sanitizePositiveInt(
      source.get<unknown>("performance.largeFileThresholdBytes", 131072),
      131072,
    ),
    hugeFileThresholdBytes: sanitizePositiveInt(
      source.get<unknown>("performance.hugeFileThresholdBytes", 20971520),
      20971520,
    ),
    fullSemanticTokensMaxBytes: sanitizePositiveInt(
      source.get<unknown>("performance.fullSemanticTokensMaxBytes", 1048576),
      1048576,
    ),
    fullParseMaxBytes: sanitizePositiveInt(
      source.get<unknown>("performance.fullParseMaxBytes", 5242880),
      5242880,
    ),
    enableHugeFileFullParse: source.get<boolean>(
      "performance.enableHugeFileFullParse",
      false,
    ),
    backgroundParseWorkerCount: sanitizePositiveInt(
      source.get<unknown>("performance.backgroundParseWorkerCount", 2),
      2,
    ),
    parseMaxFileBytes: sanitizePositiveInt(
      source.get<unknown>("server.parseMaxFileBytes", 5242880),
      5242880,
    ),
    pressureSoftMb: sanitizePositiveInt(
      source.get<unknown>("server.pressureSoftMb", 512),
      512,
    ),
    pressureCriticalMb: sanitizePositiveInt(
      source.get<unknown>("server.pressureCriticalMb", 768),
      768,
    ),
    startupPriorityMode: sanitizeStartupPriorityMode(
      readConfiguredOptionalString(source, "performance.priorityMode") ??
        readConfiguredOptionalString(source, "startup.priorityMode") ??
        "navigationFirst",
    ),
    featureProfile,
    customEnabledFeatures,
    featureOverrides,
    extraSourceFileExtensions,
    sourceExtensions,
    features,
  };
}

export function buildInitializationOptions(
  config: JovialConfig,
  sourceFileSets: JovialSourceFileSet[],
): JovialInitializationOptions {
  return {
    jovial: {
      workspace: {
        diagnosticsMode: config.workspaceDiagnosticsMode,
        profileMode: config.workspaceProfileMode,
        rootModel: config.rootModel,
        manualRootFiles: config.manualRootFiles,
        maxStartupFiles: config.maxStartupFiles,
        sourceDiscoveryTruncated: sourceFileSets.some(
          (set) => set.searchTruncated,
        ),
        extraSourceFileExtensions: config.extraSourceFileExtensions,
        sourceFileSets,
      },
      background: {
        indexBudgetMs: config.backgroundIndexBudgetMs,
        diagBatchSize: config.backgroundDiagBatchSize,
      },
      features: {
        profile: config.featureProfile,
        overrides: config.featureOverrides,
        ...config.features,
      },
      server: {
        parseMaxFileBytes: config.parseMaxFileBytes,
        pressureSoftMb: config.pressureSoftMb,
        pressureCriticalMb: config.pressureCriticalMb,
      },
      startup: {
        priorityMode: config.startupPriorityMode,
      },
      performance: {
        priorityMode: config.startupPriorityMode,
        largeFileThresholdBytes: config.largeFileThresholdBytes,
        hugeFileThresholdBytes: config.hugeFileThresholdBytes,
        fullSemanticTokensMaxBytes: config.fullSemanticTokensMaxBytes,
        fullParseMaxBytes: config.fullParseMaxBytes,
        enableHugeFileFullParse: config.enableHugeFileFullParse,
        backgroundParseWorkerCount: config.backgroundParseWorkerCount,
      },
    },
  };
}
