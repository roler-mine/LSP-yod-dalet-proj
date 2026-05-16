// Module overview: VS Code end-to-end performance-test launcher for startup and large-workspace scenarios.

import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { pathToFileURL } from "url";

import { downloadAndUnzipVSCode, runTests } from "@vscode/test-electron";

const DEFAULT_PINNED_VSCODE_VERSION = "1.85.2";
const MIB = 1024 * 1024;
const DEFAULT_HUGE_MAIN_MIB = 16;

type E2eProfile = "small" | "huge" | "mixed";

type RunnerOptions = {
  profile: E2eProfile;
  hugeMainBytes: number;
  workspaceRoot?: string;
  samplePath?: string;
  maxStartupFiles?: number;
  enableHugeFullParse?: boolean;
  injectDiagnostics: boolean;
};

type ExtensionManifest = {
  config?: {
    vscodeTestVersion?: unknown;
  };
  engines?: {
    vscode?: unknown;
  };
};

type ResolvedVsCode = {
  executablePath: string;
  source: string;
  version: string;
};

type ResolvedServer = {
  executablePath: string;
  source: string;
};

type DiagnosticSeed = {
  samplePath: string;
  files: string[];
  expectedMessages: string[];
};

type LaunchDiagnostics = {
  extensionDevelopmentPath: string;
  extensionTestsPath: string;
  workspacePath: string;
  samplePath: string;
  reportPath: string;
  userDataDir: string;
  extensionsDir: string;
  serverExecutablePath?: string;
  serverSource?: string;
  vscodeExecutablePath?: string;
  vscodeSource?: string;
  vscodeVersion: string;
};

let launchDiagnostics: LaunchDiagnostics | undefined;

function argValue(argv: readonly string[], name: string): string | undefined {
  const prefix = `${name}=`;
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === name) return argv[i + 1];
    if (arg.startsWith(prefix)) return arg.slice(prefix.length);
  }
  return undefined;
}

function parsePositiveNumber(raw: string | undefined): number | undefined {
  if (!raw) return undefined;
  const value = Number(raw);
  return Number.isFinite(value) && value > 0 ? value : undefined;
}

function parseOptionalBoolean(raw: string | undefined): boolean | undefined {
  if (raw === undefined) return undefined;
  const value = raw.trim().toLowerCase();
  if (value === "1" || value === "true" || value === "yes" || value === "on") {
    return true;
  }
  if (
    value === "0" ||
    value === "false" ||
    value === "no" ||
    value === "off"
  ) {
    return false;
  }
  return undefined;
}

function parseProfile(raw: string | undefined): E2eProfile {
  const value = raw?.trim().toLowerCase();
  if (!value || value === "small") return "small";
  if (value === "huge" || value === "large-file" || value === "large_file") {
    return "huge";
  }
  if (
    value === "mixed" ||
    value === "mixed-stress" ||
    value === "mixed_realistic_stress" ||
    value === "mixed-realistic-stress"
  ) {
    return "mixed";
  }
  throw new Error(
    `Unsupported JOVIAL e2e profile '${raw}'. Supported profiles: small, huge, mixed.`,
  );
}

function parseRunnerOptions(argv: readonly string[]): RunnerOptions {
  const profile = parseProfile(
    argValue(argv, "--profile") ?? process.env.JOVIAL_E2E_PROFILE,
  );
  const hugeMainMiB =
    parsePositiveNumber(argValue(argv, "--huge-main-mb")) ??
    parsePositiveNumber(process.env.JOVIAL_E2E_HUGE_MAIN_MB) ??
    DEFAULT_HUGE_MAIN_MIB;
  const workspaceRoot =
    argValue(argv, "--workspace-root") ??
    process.env.JOVIAL_E2E_WORKSPACE_ROOT ??
    undefined;
  const samplePath =
    argValue(argv, "--sample-path") ??
    argValue(argv, "--sample") ??
    process.env.JOVIAL_E2E_SAMPLE_PATH ??
    undefined;
  const maxStartupFilesRaw =
    parsePositiveNumber(argValue(argv, "--max-startup-files")) ??
    parsePositiveNumber(process.env.JOVIAL_E2E_MAX_STARTUP_FILES);
  const enableHugeFullParseRaw =
    argValue(argv, "--enable-huge-full-parse") ??
    process.env.JOVIAL_E2E_ENABLE_HUGE_FULL_PARSE;
  const enableHugeFullParse =
    enableHugeFullParseRaw === undefined
      ? undefined
      : enableHugeFullParseRaw === "1" ||
        enableHugeFullParseRaw.toLowerCase() === "true";
  const injectDiagnostics =
    parseOptionalBoolean(
      argValue(argv, "--inject-diagnostics") ??
        process.env.JOVIAL_E2E_INJECT_DIAGNOSTICS,
    ) ?? profile === "mixed";

  return {
    profile,
    hugeMainBytes: Math.round(hugeMainMiB * MIB),
    workspaceRoot: workspaceRoot ? path.resolve(workspaceRoot) : undefined,
    samplePath: samplePath ? path.resolve(samplePath) : undefined,
    maxStartupFiles: maxStartupFilesRaw
      ? Math.max(1, Math.trunc(maxStartupFilesRaw))
      : undefined,
    enableHugeFullParse,
    injectDiagnostics,
  };
}

function ensureDir(dirPath: string): void {
  fs.mkdirSync(dirPath, { recursive: true });
}

function writeJson(filePath: string, value: unknown): void {
  ensureDir(path.dirname(filePath));
  fs.writeFileSync(filePath, `${JSON.stringify(value, null, 2)}\n`, "utf8");
}

function readManifest(extensionDevelopmentPath: string): ExtensionManifest {
  const manifestPath = path.join(extensionDevelopmentPath, "package.json");
  return JSON.parse(fs.readFileSync(manifestPath, "utf8")) as ExtensionManifest;
}

function findInstalledVsCode(): string | undefined {
  const candidates = [
    path.join(
      process.env.LOCALAPPDATA ?? "",
      "Programs",
      "Microsoft VS Code",
      "Code.exe",
    ),
    "C:\\Program Files\\Microsoft VS Code\\Code.exe",
    "C:\\Program Files (x86)\\Microsoft VS Code\\Code.exe",
  ].filter((candidate): candidate is string => Boolean(candidate));

  return candidates.find((candidate) => fs.existsSync(candidate));
}

function configuredVsCodeVersion(extensionDevelopmentPath: string): string {
  const envVersion = process.env.VSCODE_TEST_VERSION?.trim();
  if (envVersion) return envVersion;

  const manifest = readManifest(extensionDevelopmentPath);
  const configured = manifest.config?.vscodeTestVersion;
  if (typeof configured === "string" && configured.trim()) {
    return configured.trim();
  }

  const engine = manifest.engines?.vscode;
  if (typeof engine === "string") {
    const exact = engine.match(/(\d+\.\d+\.\d+)/);
    if (exact) return exact[1];
  }

  return DEFAULT_PINNED_VSCODE_VERSION;
}

function configuredExecutablePath(): string | undefined {
  return (
    process.env.VSCODE_TEST_EXECUTABLE_PATH?.trim() ||
    process.env.VSCODE_EXECUTABLE_PATH?.trim() ||
    undefined
  );
}

function requireExistingExecutable(filePath: string, source: string): string {
  if (!fs.existsSync(filePath)) {
    throw new Error(`${source} does not exist: ${filePath}`);
  }
  return filePath;
}

async function resolveVsCodeExecutable(
  extensionDevelopmentPath: string,
): Promise<ResolvedVsCode> {
  const version = configuredVsCodeVersion(extensionDevelopmentPath);
  const explicitPath = configuredExecutablePath();
  if (explicitPath) {
    return {
      executablePath: requireExistingExecutable(
        explicitPath,
        "Configured VS Code executable",
      ),
      source: "explicit environment override",
      version,
    };
  }

  if (process.env.JOVIAL_INTEGRATION_USE_INSTALLED_VSCODE === "1") {
    const installed = findInstalledVsCode();
    if (!installed) {
      throw new Error(
        "JOVIAL_INTEGRATION_USE_INSTALLED_VSCODE=1 was set, but no installed VS Code executable was found.",
      );
    }
    return {
      executablePath: installed,
      source: "installed VS Code opt-in",
      version,
    };
  }

  return {
    executablePath: await downloadAndUnzipVSCode({ version }),
    source: "downloaded pinned VS Code test host",
    version,
  };
}

function hostRuntimeTarget(): string | undefined {
  if (
    (process.platform !== "win32" && process.platform !== "linux") ||
    (process.arch !== "x64" && process.arch !== "arm64")
  ) {
    return undefined;
  }
  return `${process.platform}-${process.arch}`;
}

function serverBinaryNames(): string[] {
  return process.platform === "win32"
    ? ["jovial-lsp.exe", "Main.exe", "jovial-lsp", "Main"]
    : ["jovial-lsp", "Main", "jovial-lsp.exe", "Main.exe"];
}

function serverBinaryCandidates(
  repoRoot: string,
  extensionDevelopmentPath: string,
): Array<{ filePath: string; source: string }> {
  const target = hostRuntimeTarget();
  const names = serverBinaryNames();
  const candidates: Array<{ filePath: string; source: string }> = [];

  const explicit = process.env.JOVIAL_E2E_SERVER_PATH?.trim();
  if (explicit) {
    candidates.push({
      filePath: path.resolve(explicit),
      source: "JOVIAL_E2E_SERVER_PATH",
    });
  }

  if (target) {
    const serverProjectDir = path.join(repoRoot, "apps", "lsp-server");
    for (const name of names) {
      candidates.push({
        filePath: path.join(
          serverProjectDir,
          `_build-${target}`,
          "install",
          "default",
          "bin",
          name,
        ),
        source: `apps/lsp-server/_build-${target}/install`,
      });
      candidates.push({
        filePath: path.join(
          serverProjectDir,
          `_build-${target}`,
          "default",
          "bin",
          name,
        ),
        source: `apps/lsp-server/_build-${target}/default/bin`,
      });
    }

    for (const name of names) {
      candidates.push({
        filePath: path.join(
          extensionDevelopmentPath,
          "runtime",
          "server",
          target,
          name,
        ),
        source: `bundled runtime/server/${target}`,
      });
    }
  }

  for (const name of names) {
    candidates.push({
      filePath: path.join(
        repoRoot,
        "_build",
        "default",
        "apps",
        "lsp-server",
        "bin",
        name,
      ),
      source: "repo _build/default/apps/lsp-server/bin",
    });
    candidates.push({
      filePath: path.join(
        repoRoot,
        "_build",
        "install",
        "default",
        "bin",
        name,
      ),
      source: "repo _build/install/default/bin",
    });
  }

  return candidates;
}

function resolveServerExecutable(
  repoRoot: string,
  extensionDevelopmentPath: string,
): ResolvedServer {
  const candidates = serverBinaryCandidates(repoRoot, extensionDevelopmentPath);
  const hit = candidates.find((candidate) => fs.existsSync(candidate.filePath));
  if (hit) {
    return {
      executablePath: hit.filePath,
      source: hit.source,
    };
  }

  throw new Error(
    "No Jovial LSP server executable was found for the end-to-end benchmark.\n" +
      "Run `npm --prefix apps/vscode-extension run build:server:host` first, " +
      "or set JOVIAL_E2E_SERVER_PATH to a built jovial-lsp executable.\n" +
      `Checked:\n- ${candidates.map((candidate) => candidate.filePath).join("\n- ")}`,
  );
}

function writeLines(filePath: string, lines: readonly string[]): void {
  fs.writeFileSync(filePath, `${lines.join("\n")}\n`, "utf8");
}

function stressProcedureText(index: number): string {
  const suffix = index.toString().padStart(6, "0");
  return [
    `DEF PROC STRESS${suffix} RENT;`,
    "BEGIN",
    `  ITEM LOCAL${suffix} U 10;`,
    `  ITEM COUNT${suffix} U 10;`,
    `  ITEM RATE${suffix} F 30;`,
    `  LOCAL${suffix} = LIMIT;`,
    `  COUNT${suffix} = LOCAL${suffix} + 1;`,
    `  LOCAL${suffix} = COUNT${suffix} + LIMIT;`,
    `  RATE${suffix} = FIND(LOCAL${suffix}, PRIVILEGE);`,
    `  RATE${suffix} = RATE${suffix} + 1.;`,
    `  COUNT${suffix} = COUNT${suffix} + LOCAL${suffix};`,
    `  LOCAL${suffix} = COUNT${suffix};`,
    "END",
    "",
  ].join("\n");
}

function appendStressCodeToBytes(filePath: string, targetBytes: number): void {
  let currentBytes = fs.statSync(filePath).size;
  if (currentBytes >= targetBytes) return;

  const fd = fs.openSync(filePath, "a");
  try {
    let index = 0;
    while (currentBytes < targetBytes) {
      const next = stressProcedureText(index);
      fs.writeSync(fd, next, undefined, "utf8");
      currentBytes += Buffer.byteLength(next, "utf8");
      index += 1;
    }
  } finally {
    fs.closeSync(fd);
  }
}

function writeBenchmarkWorkspace(
  workspacePath: string,
  options: RunnerOptions,
): string {
  const samplePath = path.join(workspacePath, "MAIN.j73");
  ensureDir(workspacePath);

  writeLines(path.join(workspacePath, "DATA.j73"), [
    "START COMPOOL DATA;",
    "TYPE COUNTER U 10;",
    "DEF ITEM LIMIT U 10;",
    "DEF TABLE PRIVILEGE(100);",
    "BEGIN",
    "  ITEM CODE U 10;",
    "  ITEM VALUE F 30;",
    "END",
    "REF PROC FIND(CODE,TAB) F;",
    "BEGIN",
    "  ITEM CODE U 10;",
    "  TABLE TAB(*);",
    "  BEGIN",
    "    ITEM TABCODE U 10;",
    "    ITEM TABVALUE F 30;",
    "  END",
    "END",
    "TERM",
  ]);

  writeLines(path.join(workspacePath, "FIND_IMPL.j73"), [
    "START",
    "DEF PROC FIND(CODE,TAB) F;",
    "BEGIN",
    "  ITEM CODE U 10;",
    "  TABLE TAB(*);",
    "  BEGIN",
    "    ITEM TABCODE U 10;",
    "    ITEM TABVALUE F 30;",
    "  END",
    "  FIND = -99999.;",
    "END",
    "TERM",
  ]);

  const mainLines = [
    "START",
    "!COMPOOL ('DATA');",
    "DEF PROC MAIN RENT;",
    "BEGIN",
    "  ITEM CLOCK COUNTER;",
    "  ITEM SPEED F 30;",
    "  CLOCK = LIMIT;",
    "  SPEED = FIND(CLOCK, PRIVILEGE);",
    "END",
  ];

  if (options.profile === "huge") {
    writeLines(samplePath, mainLines);
    appendStressCodeToBytes(samplePath, options.hugeMainBytes);
    fs.appendFileSync(samplePath, "TERM\n", "utf8");
  } else {
    writeLines(samplePath, [...mainLines, "TERM"]);
  }

  return samplePath;
}

function writeDiagnosticSeedWorkspace(workspacePath: string): DiagnosticSeed {
  ensureDir(workspacePath);
  const staleSeedFiles = [
    "ZZ_E2E_DIAG_TYPES.j73",
    "ZZ_E2E_DIAGNOSTICS.j73",
  ];
  for (const name of staleSeedFiles) {
    fs.rmSync(path.join(workspacePath, name), { force: true });
  }

  const compoolPath = path.join(workspacePath, "AA_E2E_DIAG_TYPES.j73");
  const samplePath = path.join(workspacePath, "AA_E2E_DIAGNOSTICS.j73");

  writeLines(compoolPath, [
    "START COMPOOL AA_E2E_DIAG_TYPES;",
    "TYPE E2E_REMOTE_COUNT U 10;",
    "DEF ITEM E2E_REMOTE_LIMIT U 10;",
    "REF PROC E2E_IMPORTED_PROC RENT;",
    "TERM",
  ]);

  writeLines(samplePath, [
    "START",
    "PROC E2E'MIXED(BYVAL'COUNT: BYREF'FLAGS) RENT;",
    "BEGIN",
    "  ITEM BYVAL'COUNT U 10;",
    "  ITEM BYREF'FLAGS B 4;",
    "END",
    "TYPE E2E'RECORD TABLE;",
    "BEGIN",
    "  ITEM GOODFIELD U 10;",
    "END",
    "DEF PROC MAIN RENT;",
    "BEGIN",
    "  ITEM COUNT U 10;",
    "  ITEM FLAGS B 4;",
    "  ITEM TEXT C 1;",
    "  ITEM REC'PTR P E2E'RECORD;",
    "  ITEM NEEDS'IMPORT E2E_REMOTE_COUNT;",
    "  COUNT = TEXT;",
    "  E2E'MIXED(COUNT: FLAGS);",
    "  E2E'MIXED(TEXT: FLAGS);",
    "  E2E'MIXED(COUNT: FLAGS, TEXT);",
    "  E2E'UNREFD'PROC(COUNT);",
    "  E2E_IMPORTED_PROC;",
    "  COUNT = GOODFIELD @ REC'PTR;",
    "  COUNT = MISSINGFIELD @ REC'PTR;",
    "  COUNT = @ COUNT;",
    "END",
    "TERM",
  ]);

  return {
    samplePath,
    files: [compoolPath, samplePath],
    expectedMessages: [
      "E2E_REMOTE_COUNT",
      "Type mismatch in assignment",
      "Argument type mismatch in call to \"E2E'MIXED\"",
      "Argument count mismatch in call to \"E2E'MIXED\"",
      "Undefined procedure \"E2E'UNREFD'PROC\"",
      "E2E_IMPORTED_PROC",
      "Unknown field \"MISSINGFIELD\" for @ access",
      "Invalid pointer dereference",
    ],
  };
}

function isJovialSourceFile(name: string): boolean {
  return /\.(jov|j73|jvl)$/i.test(name);
}

function sourceFilesInWorkspace(workspacePath: string): string[] {
  return fs
    .readdirSync(workspacePath, { withFileTypes: true })
    .filter((entry) => entry.isFile() && isJovialSourceFile(entry.name))
    .map((entry) => path.join(workspacePath, entry.name))
    .sort((a, b) => a.localeCompare(b));
}

function pickExistingWorkspaceSample(workspacePath: string): string {
  const sources = sourceFilesInWorkspace(workspacePath);
  if (sources.length === 0) {
    throw new Error(`No JOVIAL source files were found in ${workspacePath}`);
  }
  const ranked = sources
    .map((filePath) => ({
      filePath,
      bytes: fs.statSync(filePath).size,
      main: /^MAIN/i.test(path.basename(filePath)),
    }))
    .sort((a, b) => {
      if (a.main !== b.main) return a.main ? -1 : 1;
      if (a.bytes !== b.bytes) return b.bytes - a.bytes;
      return a.filePath.localeCompare(b.filePath);
    });
  return ranked[0].filePath;
}

function fileTimestamp(date = new Date()): string {
  return date
    .toISOString()
    .replace(/[-:]/g, "")
    .replace(/\.\d{3}Z$/, "Z");
}

function defaultReportPath(repoRoot: string, profile: E2eProfile): string {
  return path.join(
    repoRoot,
    "reports",
    "perf",
    `jovial-vscode-e2e-${profile}-${fileTimestamp()}.json`,
  );
}

function errorText(error: unknown): string {
  if (error instanceof Error) {
    return error.stack ?? error.message;
  }
  return String(error);
}

function formatLaunchFailure(error: unknown): string {
  const diag = launchDiagnostics;
  const lines = [
    "",
    "VS Code end-to-end performance benchmark could not launch.",
    "",
    "This benchmark launches the extension in a VS Code test host and starts the real LSP server through the extension settings, so it covers extension activation, LanguageClient processing, stdio JSON-RPC, and server processing.",
  ];

  if (diag) {
    lines.push(
      "",
      `Pinned VS Code version: ${diag.vscodeVersion}`,
      `VS Code source: ${diag.vscodeSource ?? "not resolved"}`,
      `VS Code executable: ${diag.vscodeExecutablePath ?? "not resolved"}`,
      `Server source: ${diag.serverSource ?? "not resolved"}`,
      `Server executable: ${diag.serverExecutablePath ?? "not resolved"}`,
      `Extension path: ${diag.extensionDevelopmentPath}`,
      `Test entrypoint: ${diag.extensionTestsPath}`,
      `Workspace fixture: ${diag.workspacePath}`,
      `Sample file: ${diag.samplePath}`,
      `Report path: ${diag.reportPath}`,
      `User data dir: ${diag.userDataDir}`,
      `Extensions dir: ${diag.extensionsDir}`,
    );
  }

  lines.push(
    "",
    "Actionable checks:",
    "- Run `npm --prefix apps/vscode-extension run build:server:host` to rebuild the server binary used by the benchmark.",
    "- Set `JOVIAL_E2E_SERVER_PATH=<path-to-jovial-lsp>` to benchmark a specific server build.",
    "- Set `VSCODE_TEST_VERSION=<version>` to test a different downloaded VS Code build.",
    "- Set `VSCODE_TEST_EXECUTABLE_PATH=<path-to-Code>` only when you intentionally want to bypass the downloaded test host.",
    "- On headless Linux, run under Xvfb or provide another display server.",
    "",
    "Underlying error:",
    errorText(error),
  );

  return lines.join("\n");
}

async function main(): Promise<void> {
  const options = parseRunnerOptions(process.argv.slice(2));
  const extensionDevelopmentPath = path.resolve(__dirname, "..", "..");
  const repoRoot = path.resolve(extensionDevelopmentPath, "..", "..");
  const extensionTestsPath = path.resolve(__dirname, "e2e_perf", "index.js");
  const tempRoot = fs.mkdtempSync(path.join(os.tmpdir(), "jovial-vscode-e2e-"));
  const workspacePath = options.workspaceRoot ?? path.join(tempRoot, "workspace");
  const userDataDir = path.join(tempRoot, "user-data");
  const extensionsDir = path.join(tempRoot, "extensions");
  const settingsPath = path.join(workspacePath, ".vscode", "settings.json");
  if (options.workspaceRoot && !fs.existsSync(options.workspaceRoot)) {
    throw new Error(`Configured workspace root does not exist: ${options.workspaceRoot}`);
  }
  const samplePath =
    options.workspaceRoot !== undefined
      ? options.samplePath ?? pickExistingWorkspaceSample(workspacePath)
      : writeBenchmarkWorkspace(workspacePath, options);
  if (!samplePath.startsWith(path.resolve(workspacePath))) {
    throw new Error(
      `Sample path must be inside the workspace root.\nSample: ${samplePath}\nWorkspace: ${workspacePath}`,
    );
  }
  if (!fs.existsSync(samplePath)) {
    throw new Error(`Configured sample path does not exist: ${samplePath}`);
  }
  const diagnosticSeed = options.injectDiagnostics
    ? writeDiagnosticSeedWorkspace(workspacePath)
    : undefined;
  const sampleBytes = fs.statSync(samplePath).size;
  const reportPath = path.resolve(
    process.env.JOVIAL_E2E_PERF_REPORT?.trim() ||
      defaultReportPath(repoRoot, options.profile),
  );
  const vscodeVersion = configuredVsCodeVersion(extensionDevelopmentPath);

  launchDiagnostics = {
    extensionDevelopmentPath,
    extensionTestsPath,
    workspacePath,
    samplePath,
    reportPath,
    userDataDir,
    extensionsDir,
    vscodeVersion,
  };

  ensureDir(userDataDir);
  ensureDir(extensionsDir);
  ensureDir(path.dirname(reportPath));

  const resolvedServer = resolveServerExecutable(
    repoRoot,
    extensionDevelopmentPath,
  );
  launchDiagnostics = {
    ...launchDiagnostics,
    serverExecutablePath: resolvedServer.executablePath,
    serverSource: resolvedServer.source,
  };

  const hugeLike = options.profile === "huge" || options.profile === "mixed";
  const sourceFileCount = sourceFilesInWorkspace(workspacePath).length;
  const hugeThresholdBytes =
    hugeLike
      ? Math.max(1 * MIB, Math.min(15 * MIB, options.hugeMainBytes - MIB))
      : 15 * MIB;
  const enableHugeFullParse =
    options.enableHugeFullParse ?? (options.profile === "huge" && !options.workspaceRoot);
  const fullParseMaxBytes =
    enableHugeFullParse && hugeLike ? sampleBytes + MIB : 15 * MIB;
  const commandSamples =
    process.env.JOVIAL_E2E_PERF_SAMPLES?.trim() ||
    (hugeLike ? "3" : "5");
  const maxStartupFiles =
    options.maxStartupFiles ??
    (options.workspaceRoot
      ? Math.max(sourceFileCount + 16, 1000)
      : options.profile === "huge"
        ? 25
        : 1000);
  const manualRootFile = path.relative(workspacePath, samplePath);
  const manualRootFiles = [
    manualRootFile,
    ...(diagnosticSeed
      ? [path.relative(workspacePath, diagnosticSeed.samplePath)]
      : []),
  ];

  writeJson(settingsPath, {
    "jovial.server.path": resolvedServer.executablePath,
    "jovial.server.args": [],
    "jovial.server.preferBundled": false,
    "jovial.trace": "off",
    "jovial.autostart": true,
    "jovial.workspaceDiagnostics.mode": "all",
    "jovial.workspace.profileMode": hugeLike ? "large" : "small",
    "jovial.workspace.rootModel": "manual",
    "jovial.workspace.manualRootFiles": manualRootFiles,
    "jovial.workspace.maxStartupFiles": maxStartupFiles,
    "jovial.performance.priorityMode": "navigationFirst",
    "jovial.performance.largeFileThresholdBytes": 131072,
    "jovial.performance.hugeFileThresholdBytes": hugeThresholdBytes,
    "jovial.performance.fullSemanticTokensMaxBytes": 1048576,
    "jovial.performance.fullParseMaxBytes": fullParseMaxBytes,
    "jovial.performance.enableHugeFileFullParse": enableHugeFullParse,
    "jovial.performance.backgroundParseWorkerCount": 2,
    "jovial.server.parseMaxFileBytes": fullParseMaxBytes,
    "jovial.background.indexBudgetMs": 8,
    "jovial.background.diagBatchSize": 16,
    "editor.inlayHints.enabled": "on",
  });

  const extensionTestsEnv = {
    ...process.env,
    ELECTRON_RUN_AS_NODE: undefined,
    JOVIAL_E2E_WORKSPACE: workspacePath,
    JOVIAL_E2E_SAMPLE: samplePath,
    JOVIAL_E2E_REPORT: reportPath,
    JOVIAL_E2E_SERVER_PATH: resolvedServer.executablePath,
    JOVIAL_E2E_PROFILE: options.profile,
    JOVIAL_E2E_SAMPLE_BYTES: String(sampleBytes),
    JOVIAL_E2E_HUGE_THRESHOLD_BYTES: String(hugeThresholdBytes),
    JOVIAL_E2E_FULL_PARSE_MAX_BYTES: String(fullParseMaxBytes),
    JOVIAL_E2E_PERF_SAMPLES: commandSamples,
    JOVIAL_E2E_DIAGNOSTIC_SAMPLE: diagnosticSeed?.samplePath ?? "",
    JOVIAL_E2E_DIAGNOSTIC_EXPECT: JSON.stringify(
      diagnosticSeed?.expectedMessages ?? [],
    ),
    JOVIAL_E2E_VIEWPORT_LINE_COUNT:
      process.env.JOVIAL_E2E_VIEWPORT_LINE_COUNT ??
      (options.profile === "huge" ? "30" : "0"),
  };
  const launchArgs = [
    `--folder-uri=${pathToFileURL(workspacePath).toString()}`,
    "--disable-updates",
    "--skip-welcome",
    "--skip-release-notes",
    "--disable-gpu",
    "--disable-workspace-trust",
    `--extensions-dir=${extensionsDir}`,
    `--user-data-dir=${userDataDir}`,
  ];
  const resolvedVsCode = await resolveVsCodeExecutable(
    extensionDevelopmentPath,
  );
  launchDiagnostics = {
    ...launchDiagnostics,
    vscodeExecutablePath: resolvedVsCode.executablePath,
    vscodeSource: resolvedVsCode.source,
    vscodeVersion: resolvedVsCode.version,
  };

  await runTests({
    vscodeExecutablePath: resolvedVsCode.executablePath,
    extensionDevelopmentPath,
    extensionTestsPath,
    extensionTestsEnv,
    launchArgs,
    reuseMachineInstall: false,
  });

  console.log(`Jovial VS Code end-to-end benchmark report: ${reportPath}`);
}

main().catch((error) => {
  console.error(formatLaunchFailure(error));
  process.exit(1);
});
