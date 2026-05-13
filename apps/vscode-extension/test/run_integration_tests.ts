import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { pathToFileURL } from "url";

import { downloadAndUnzipVSCode, runTests } from "@vscode/test-electron";

const DEFAULT_PINNED_VSCODE_VERSION = "1.85.2";

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

type LaunchDiagnostics = {
  extensionDevelopmentPath: string;
  extensionTestsPath: string;
  fixtureWorkspacePath: string;
  userDataDir: string;
  extensionsDir: string;
  vscodeExecutablePath?: string;
  vscodeSource?: string;
  vscodeVersion: string;
};

let launchDiagnostics: LaunchDiagnostics | undefined;

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
    "VS Code integration tests could not launch the pinned test host.",
    "",
    "This runner downloads a fixed VS Code build by default and launches it with a temporary user-data-dir and extensions-dir, so it should not depend on a locally updating VS Code installation.",
  ];

  if (diag) {
    lines.push(
      "",
      `Pinned VS Code version: ${diag.vscodeVersion}`,
      `VS Code source: ${diag.vscodeSource ?? "not resolved"}`,
      `VS Code executable: ${diag.vscodeExecutablePath ?? "not resolved"}`,
      `Extension path: ${diag.extensionDevelopmentPath}`,
      `Test entrypoint: ${diag.extensionTestsPath}`,
      `Workspace fixture: ${diag.fixtureWorkspacePath}`,
      `User data dir: ${diag.userDataDir}`,
      `Extensions dir: ${diag.extensionsDir}`,
    );
  }

  lines.push(
    "",
    "Actionable checks:",
    "- Re-run `npm run test:integration:pinned` to retry a failed or interrupted VS Code download.",
    "- Delete `apps/vscode-extension/.vscode-test` if the downloaded test host cache is corrupt.",
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
  const extensionDevelopmentPath = path.resolve(__dirname, "..", "..");
  const extensionTestsPath = path.resolve(__dirname, "integration", "index.js");
  const fixtureWorkspacePath = path.resolve(
    extensionDevelopmentPath,
    "test",
    "fixtures",
    "workspace",
    "sample.jov",
  );
  const fakeServerPath = path.resolve(
    extensionDevelopmentPath,
    "test",
    "fixtures",
    "fake_lsp_server.js",
  );

  const tempRoot = fs.mkdtempSync(path.join(os.tmpdir(), "jovial-vscode-int-"));
  const workspacePath = path.join(tempRoot, "workspace");
  const settingsPath = path.join(workspacePath, ".vscode", "settings.json");
  const samplePath = path.join(workspacePath, "sample.jov");
  const logPath = path.join(tempRoot, "fake-server.log");
  const userDataDir = path.join(tempRoot, "user-data");
  const extensionsDir = path.join(tempRoot, "extensions");
  const vscodeVersion = configuredVsCodeVersion(extensionDevelopmentPath);

  launchDiagnostics = {
    extensionDevelopmentPath,
    extensionTestsPath,
    fixtureWorkspacePath,
    userDataDir,
    extensionsDir,
    vscodeVersion,
  };

  ensureDir(workspacePath);
  ensureDir(userDataDir);
  ensureDir(extensionsDir);
  fs.copyFileSync(fixtureWorkspacePath, samplePath);
  writeJson(settingsPath, {
    "jovial.server.path": process.execPath,
    "jovial.server.args": [fakeServerPath, "--log", logPath],
    "jovial.server.preferBundled": false,
    "jovial.trace": "off",
    "jovial.autostart": true,
    "jovial.lsif.fastPath": true,
    "jovial.workspaceDiagnostics.mode": "all",
    "jovial.background.indexBudgetMs": 11,
    "jovial.background.diagBatchSize": 7,
    "jovial.workspace.profileMode": "medium",
    "jovial.workspace.rootModel": "manual",
    "jovial.workspace.manualRootFiles": ["sample.jov"],
    "jovial.workspace.maxStartupFiles": 25,
    "jovial.performance.largeFileThresholdBytes": 131072,
    "jovial.performance.hugeFileThresholdBytes": 20971520,
    "jovial.performance.fullSemanticTokensMaxBytes": 1048576,
    "jovial.performance.fullParseMaxBytes": 5242880,
    "jovial.performance.backgroundParseWorkerCount": 2,
    "jovial.server.parseMaxFileBytes": 123456,
    "jovial.server.pressureSoftMb": 256,
    "jovial.server.pressureCriticalMb": 384,
    "editor.inlayHints.enabled": "on",
  });
  fs.writeFileSync(logPath, "", "utf8");

  const extensionTestsEnv = {
    ...process.env,
    ELECTRON_RUN_AS_NODE: undefined,
    JOVIAL_INTEGRATION_LOG: logPath,
    JOVIAL_INTEGRATION_WORKSPACE: workspacePath,
    JOVIAL_INTEGRATION_SAMPLE: samplePath,
    JOVIAL_INTEGRATION_SAMPLE_URI: pathToFileURL(samplePath).toString(),
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
}

main().catch((error) => {
  console.error(formatLaunchFailure(error));
  process.exit(1);
});
