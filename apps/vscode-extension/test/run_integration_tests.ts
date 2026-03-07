import { spawn } from "child_process";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { pathToFileURL } from "url";

import { downloadAndUnzipVSCode } from "@vscode/test-electron";

function ensureDir(dirPath: string): void {
  fs.mkdirSync(dirPath, { recursive: true });
}

function writeJson(filePath: string, value: unknown): void {
  ensureDir(path.dirname(filePath));
  fs.writeFileSync(filePath, `${JSON.stringify(value, null, 2)}\n`, "utf8");
}

function findInstalledVsCode(): string | undefined {
  const candidates = [
    process.env.VSCODE_EXECUTABLE_PATH,
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
    "jovial.server.parseMaxFileBytes": 123456,
    "jovial.server.pressureSoftMb": 256,
    "jovial.server.pressureCriticalMb": 384,
  });
  fs.writeFileSync(logPath, "", "utf8");

  const vscodeExecutablePath =
    findInstalledVsCode() ?? (await downloadAndUnzipVSCode());
  const args = [
    `--folder-uri=${pathToFileURL(workspacePath).toString()}`,
    "--disable-updates",
    "--skip-welcome",
    "--skip-release-notes",
    "--disable-workspace-trust",
    `--extensionTestsPath=${extensionTestsPath}`,
    `--extensionDevelopmentPath=${extensionDevelopmentPath}`,
    `--extensions-dir=${extensionsDir}`,
    `--user-data-dir=${userDataDir}`,
  ];

  await new Promise<void>((resolve, reject) => {
    const env = {
      ...process.env,
      ELECTRON_RUN_AS_NODE: undefined,
      JOVIAL_INTEGRATION_LOG: logPath,
      JOVIAL_INTEGRATION_WORKSPACE: workspacePath,
      JOVIAL_INTEGRATION_SAMPLE: samplePath,
      JOVIAL_INTEGRATION_SAMPLE_URI: pathToFileURL(samplePath).toString(),
    };
    const child = spawn(vscodeExecutablePath, args, {
      env,
      stdio: "inherit",
      shell: false,
    });
    child.on("error", reject);
    child.on("exit", (code, signal) => {
      if (code === 0) {
        resolve();
        return;
      }
      reject(
        new Error(
          `VS Code integration test run failed with code=${code} signal=${signal}`,
        ),
      );
    });
  });
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
