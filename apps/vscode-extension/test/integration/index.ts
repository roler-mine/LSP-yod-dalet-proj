import { strict as assert } from "assert";
import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";

type LoggedEvent = {
  type: string;
  method?: string;
  command?: string;
  params?: {
    initializationOptions?: {
      jovial?: unknown;
    };
    changes?: Array<{ uri: string; type: number }>;
  };
  args?: unknown[];
};

const EXTENSION_ID = "roler-mine.jovial-lsp-client";
const LOG_PATH = process.env.JOVIAL_INTEGRATION_LOG;
const SAMPLE_PATH = process.env.JOVIAL_INTEGRATION_SAMPLE;
const WORKSPACE_PATH = process.env.JOVIAL_INTEGRATION_WORKSPACE;
const DEFAULT_WAIT_TIMEOUT_MS = process.env.CI === "true" ? 60000 : 30000;
const DEFAULT_QUIET_PERIOD_MS = process.env.CI === "true" ? 2500 : 1200;

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function requireEnv(name: string, value: string | undefined): string {
  assert.ok(value, `Missing required environment variable: ${name}`);
  return value;
}

function readEvents(): LoggedEvent[] {
  const logPath = requireEnv("JOVIAL_INTEGRATION_LOG", LOG_PATH);
  if (!fs.existsSync(logPath)) return [];
  return fs
    .readFileSync(logPath, "utf8")
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .flatMap((line) => {
      try {
        return [JSON.parse(line) as LoggedEvent];
      } catch {
        return [];
      }
    });
}

async function waitFor<T>(
  description: string,
  compute: () => T | undefined,
  timeoutMs = DEFAULT_WAIT_TIMEOUT_MS,
  intervalMs = 150,
): Promise<T> {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const value = compute();
    if (value !== undefined) return value;
    await sleep(intervalMs);
  }
  throw new Error(`Timed out waiting for ${description}`);
}

async function waitForCountStable(
  description: string,
  computeCount: () => number,
  quietMs = DEFAULT_QUIET_PERIOD_MS,
  timeoutMs = DEFAULT_WAIT_TIMEOUT_MS,
  intervalMs = 150,
): Promise<number> {
  const deadline = Date.now() + timeoutMs;
  let lastCount = computeCount();
  let stableSince = Date.now();
  while (Date.now() < deadline) {
    await sleep(intervalMs);
    const nextCount = computeCount();
    if (nextCount !== lastCount) {
      lastCount = nextCount;
      stableSince = Date.now();
      continue;
    }
    if (Date.now() - stableSince >= quietMs) {
      return nextCount;
    }
  }
  throw new Error(`Timed out waiting for stable ${description}`);
}

async function activateExtension(): Promise<void> {
  const samplePath = requireEnv("JOVIAL_INTEGRATION_SAMPLE", SAMPLE_PATH);
  const document = await vscode.workspace.openTextDocument(
    vscode.Uri.file(samplePath),
  );
  await vscode.window.showTextDocument(document);

  const extension = vscode.extensions.getExtension(EXTENSION_ID);
  assert.ok(extension, `Extension '${EXTENSION_ID}' was not found`);
  if (!extension.isActive) {
    await extension.activate();
  }
}

async function waitForInitializeCount(count: number): Promise<LoggedEvent[]> {
  return waitFor(`initialize count >= ${count}`, () => {
    const events = readEvents();
    const initializes = events.filter((event) => event.method === "initialize");
    return initializes.length >= count ? initializes : undefined;
  });
}

function initializeCount(): number {
  return readEvents().filter((event) => event.method === "initialize").length;
}

function shutdownCount(): number {
  return readEvents().filter((event) => event.method === "shutdown").length;
}

function lsifRefreshCount(): number {
  return readEvents().filter(
    (event) =>
      event.type === "executeCommand" &&
      event.command === "jovial.dumpLsifIndex",
  ).length;
}

function inlayHintRequestCount(): number {
  return readEvents().filter(
    (event) => event.method === "textDocument/inlayHint",
  ).length;
}

async function refreshEditorInlayHints(): Promise<void> {
  try {
    await vscode.commands.executeCommand("editor.action.inlayHints.refresh");
  } catch {
    // Older VS Code builds may not expose this command to tests.
  }
}

async function setConfig<T>(key: string, value: T): Promise<void> {
  await vscode.workspace
    .getConfiguration("jovial")
    .update(key, value, vscode.ConfigurationTarget.Workspace);
}

async function testAutostartAndInitializePayload(): Promise<void> {
  await activateExtension();
  const initializes = await waitForInitializeCount(1);
  const first = initializes[0];
  const jovial = first.params?.initializationOptions?.jovial as
    | {
        workspace?: Record<string, unknown>;
        background?: Record<string, unknown>;
        server?: Record<string, unknown>;
        performance?: Record<string, unknown>;
      }
    | undefined;
  assert.ok(
    jovial,
    "initialize payload is missing initializationOptions.jovial",
  );
  assert.equal(jovial?.workspace?.["diagnosticsMode"], "all");
  assert.equal(jovial?.workspace?.["profileMode"], "medium");
  assert.equal(jovial?.workspace?.["rootModel"], "manual");
  assert.deepEqual(jovial?.workspace?.["manualRootFiles"], ["sample.jov"]);
  assert.equal(jovial?.workspace?.["maxStartupFiles"], 25);
  assert.equal(jovial?.background?.["indexBudgetMs"], 11);
  assert.equal(jovial?.background?.["diagBatchSize"], 7);
  assert.equal(jovial?.performance?.["fullParseMaxBytes"], 5242880);
  assert.equal(jovial?.performance?.["backgroundParseWorkerCount"], 2);
  assert.equal(jovial?.server?.["parseMaxFileBytes"], 123456);
  assert.equal(jovial?.server?.["pressureSoftMb"], 256);
  assert.equal(jovial?.server?.["pressureCriticalMb"], 384);
  await waitFor("initial inlay hint request", () => {
    const count = inlayHintRequestCount();
    return count > 0 ? count : undefined;
  });
}

async function testConfigRestartResendsSettings(): Promise<void> {
  const before = await waitForCountStable("initialize events", initializeCount);
  await setConfig("workspace.profileMode", "large");
  const initializes = await waitForInitializeCount(before + 1);
  const last = initializes[initializes.length - 1];
  const jovial = last.params?.initializationOptions?.jovial as
    | {
        workspace?: Record<string, unknown>;
      }
    | undefined;
  assert.equal(jovial?.workspace?.["profileMode"], "large");
}

async function testFeatureToggleRestartResendsSettings(): Promise<void> {
  const samplePath = requireEnv("JOVIAL_INTEGRATION_SAMPLE", SAMPLE_PATH);
  const document = await vscode.workspace.openTextDocument(
    vscode.Uri.file(samplePath),
  );
  await vscode.window.showTextDocument(document);

  const before = await waitForCountStable("initialize events", initializeCount);
  await setConfig("features.hover", false);
  const initializes = await waitForInitializeCount(before + 1);
  const last = initializes[initializes.length - 1];
  const jovial = last.params?.initializationOptions?.jovial as
    | {
        features?: Record<string, unknown>;
        startup?: Record<string, unknown>;
      }
    | undefined;
  assert.equal(jovial?.features?.["hover"], false);
  const disabledHovers =
    (await vscode.commands.executeCommand<readonly vscode.Hover[]>(
      "vscode.executeHoverProvider",
      document.uri,
      new vscode.Position(0, 0),
    )) ?? [];
  assert.equal(
    disabledHovers.length,
    0,
    "Hover provider should be disabled after client restart",
  );

  const beforeStartup = await waitForCountStable(
    "initialize events after feature toggle",
    initializeCount,
  );
  await setConfig("startup.priorityMode", "infoFirst");
  const afterStartup = await waitForInitializeCount(beforeStartup + 1);
  const startupPayload = afterStartup[afterStartup.length - 1].params
    ?.initializationOptions?.jovial as
    | {
        startup?: Record<string, unknown>;
      }
    | undefined;
  assert.equal(startupPayload?.startup?.["priorityMode"], "infoFirst");

  const beforeDisableInlayHints = await waitForCountStable(
    "initialize events before inlay-hint disable",
    initializeCount,
  );
  const beforeDisableShutdowns = shutdownCount();
  await setConfig("features.inlayHints", false);
  const afterDisableInlayHints = await waitForInitializeCount(
    beforeDisableInlayHints + 1,
  );
  await waitFor("shutdown before inlay-hint restart", () => {
    const count = shutdownCount();
    return count > beforeDisableShutdowns ? count : undefined;
  });
  const disabledPayload = afterDisableInlayHints[
    afterDisableInlayHints.length - 1
  ].params?.initializationOptions?.jovial as
    | {
        features?: Record<string, unknown>;
      }
    | undefined;
  assert.equal(disabledPayload?.features?.["inlayHints"], false);
  const beforeDisabledFetchInlayHintRequests = inlayHintRequestCount();
  const disabledInlayHints = await fetchInlayHints(document);
  assert.equal(
    disabledInlayHints.length,
    0,
    "Inlay hints should be disabled after client restart",
  );
  assert.equal(
    inlayHintRequestCount(),
    beforeDisabledFetchInlayHintRequests,
    "Disabled inlay hints should not reach the server or any LSIF fallback",
  );

  const beforeRestoreHover = await waitForCountStable(
    "initialize events before hover restore",
    initializeCount,
  );
  await setConfig("features.hover", true);
  await waitForInitializeCount(beforeRestoreHover + 1);

  const beforeRestoreStartup = await waitForCountStable(
    "initialize events before startup restore",
    initializeCount,
  );
  await setConfig("startup.priorityMode", "balanced");
  await waitForInitializeCount(beforeRestoreStartup + 1);

  const beforeRestoreInlayHints = await waitForCountStable(
    "initialize events before inlay-hint restore",
    initializeCount,
  );
  const beforeRestoreInlayHintRequests = inlayHintRequestCount();
  await setConfig("features.inlayHints", true);
  const afterRestoreInlayHints = await waitForInitializeCount(
    beforeRestoreInlayHints + 1,
  );
  const restoredPayload = afterRestoreInlayHints[
    afterRestoreInlayHints.length - 1
  ].params?.initializationOptions?.jovial as
    | {
        features?: Record<string, unknown>;
      }
    | undefined;
  assert.equal(restoredPayload?.features?.["inlayHints"], true);
  await refreshEditorInlayHints();
  await waitFor("restored inlay hint request", () => {
    const count = inlayHintRequestCount();
    return count > beforeRestoreInlayHintRequests ? count : undefined;
  });
}

async function testDiagnosticsFeatureSettingDoesNotRestartHints(): Promise<void> {
  const beforeInitializes = await waitForCountStable(
    "initialize events before ignored diagnostics toggle",
    initializeCount,
  );
  const beforeInlayHintRequests = await waitForCountStable(
    "inlay hint requests before ignored diagnostics toggle",
    inlayHintRequestCount,
  );

  await setConfig("features.diagnostics", false);

  const afterInitializes = await waitForCountStable(
    "initialize events after ignored diagnostics toggle",
    initializeCount,
  );
  const afterInlayHintRequests = await waitForCountStable(
    "inlay hint requests after ignored diagnostics toggle",
    inlayHintRequestCount,
  );
  assert.equal(
    afterInitializes,
    beforeInitializes,
    "Ignored diagnostics feature setting should not restart the server",
  );
  assert.equal(
    afterInlayHintRequests,
    beforeInlayHintRequests,
    "Diagnostics feature setting should not refresh or request inlay hints",
  );

  await setConfig("features.diagnostics", true);
  const restoredInitializes = await waitForCountStable(
    "initialize events after restoring ignored diagnostics toggle",
    initializeCount,
  );
  assert.equal(
    restoredInitializes,
    beforeInitializes,
    "Restoring ignored diagnostics feature setting should not restart the server",
  );
}

async function testCustomFeatureProfileRestartResendsSettings(): Promise<void> {
  const samplePath = requireEnv("JOVIAL_INTEGRATION_SAMPLE", SAMPLE_PATH);
  const document = await vscode.workspace.openTextDocument(
    vscode.Uri.file(samplePath),
  );
  await vscode.window.showTextDocument(document);

  const beforeCustomFeatures = await waitForCountStable(
    "initialize events before custom feature list",
    initializeCount,
  );
  await setConfig("features.custom.enabledFeatures", ["inlayHints"]);
  await waitForInitializeCount(beforeCustomFeatures + 1);

  const beforeCustomProfile = await waitForCountStable(
    "initialize events before custom feature profile",
    initializeCount,
  );
  const beforeCustomInlayHintRequests = inlayHintRequestCount();
  await setConfig("features.profile", "custom");
  const initializes = await waitForInitializeCount(beforeCustomProfile + 1);
  const jovial = initializes[initializes.length - 1].params
    ?.initializationOptions?.jovial as
    | {
        features?: Record<string, unknown>;
      }
    | undefined;
  assert.equal(jovial?.features?.["profile"], "custom");
  assert.equal(jovial?.features?.["diagnostics"], true);
  assert.equal(jovial?.features?.["hover"], false);
  assert.equal(jovial?.features?.["completion"], false);
  assert.equal(jovial?.features?.["inlayHints"], true);
  assert.equal(jovial?.features?.["semanticTokens"], false);

  const disabledHovers =
    (await vscode.commands.executeCommand<readonly vscode.Hover[]>(
      "vscode.executeHoverProvider",
      document.uri,
      new vscode.Position(0, 0),
    )) ?? [];
  assert.equal(
    disabledHovers.length,
    0,
    "Custom feature profile should disable hover when hover is not selected",
  );

  await refreshEditorInlayHints();
  await waitFor("custom profile inlay hint request", () => {
    const count = inlayHintRequestCount();
    return count > beforeCustomInlayHintRequests ? count : undefined;
  });

  const beforeRestoreProfile = await waitForCountStable(
    "initialize events before feature profile restore",
    initializeCount,
  );
  await setConfig("features.profile", "full");
  await waitForInitializeCount(beforeRestoreProfile + 1);
}

async function testRestartCommand(): Promise<void> {
  const before = await waitForCountStable("initialize events", initializeCount);
  await vscode.commands.executeCommand("jovial.restartServer");
  await waitForInitializeCount(before + 1);
}

async function testWatchedFileNotifications(): Promise<void> {
  const workspacePath = requireEnv(
    "JOVIAL_INTEGRATION_WORKSPACE",
    WORKSPACE_PATH,
  );
  const target = vscode.Uri.file(
    path.join(workspacePath, "watched-change.jov"),
  );
  await vscode.workspace.fs.writeFile(target, Buffer.from("FOO\n", "utf8"));
  await waitFor("watched-file notification", () => {
    const hit = readEvents().find(
      (event) =>
        event.method === "workspace/didChangeWatchedFiles" &&
        Array.isArray(event.params?.changes) &&
        event.params?.changes.some(
          (change) => change.uri === target.toString(),
        ),
    );
    return hit;
  });
}

async function testLsifRefreshCommand(): Promise<void> {
  const before = await waitForCountStable(
    "LSIF refresh events",
    lsifRefreshCount,
  );

  await setConfig("lsif.fastPath", false);
  await vscode.commands.executeCommand("jovial.refreshLsifCache");
  const disabledCount = await waitForCountStable(
    "LSIF refresh events after disabled refresh command",
    lsifRefreshCount,
  );
  assert.equal(
    disabledCount,
    before,
    "LSIF refresh should not run while fast path is disabled",
  );

  await setConfig("lsif.fastPath", true);
  await activateExtension();
  await vscode.commands.executeCommand("jovial.refreshLsifCache");
  await waitFor("LSIF refresh executeCommand", () => {
    const count = lsifRefreshCount();
    return count > before ? count : undefined;
  });
}

function normalizeLocations(
  value:
    | vscode.Location
    | vscode.LocationLink
    | readonly vscode.Location[]
    | readonly vscode.LocationLink[]
    | undefined,
): vscode.Location[] {
  if (!value) return [];
  const items = Array.isArray(value) ? value : [value];
  return items.flatMap((item) => {
    if (item instanceof vscode.Location) return [item];
    return [
      new vscode.Location(
        item.targetUri,
        item.targetSelectionRange ?? item.targetRange,
      ),
    ];
  });
}

function hoverText(hovers: readonly vscode.Hover[]): string {
  return hovers
    .flatMap((hover) => hover.contents)
    .map((content) => {
      if (typeof content === "string") return content;
      if (content instanceof vscode.MarkdownString) return content.value;
      return content.value;
    })
    .join("\n");
}

async function fetchInlayHints(
  document: vscode.TextDocument,
): Promise<readonly vscode.InlayHint[]> {
  return (
    (await vscode.commands.executeCommand<readonly vscode.InlayHint[]>(
      "vscode.executeInlayHintProvider",
      document.uri,
      new vscode.Range(
        new vscode.Position(0, 0),
        new vscode.Position(0, Math.max(1, document.lineAt(0).text.length)),
      ),
    )) ?? []
  );
}

async function testMiddlewareLsifFallback(): Promise<void> {
  await setConfig("lsif.fastPath", true);
  await activateExtension();
  await vscode.commands.executeCommand("jovial.refreshLsifCache");
  await waitFor("LSIF cache refresh", () => {
    const count = lsifRefreshCount();
    return count > 0 ? count : undefined;
  });

  const samplePath = requireEnv("JOVIAL_INTEGRATION_SAMPLE", SAMPLE_PATH);
  const document = await vscode.workspace.openTextDocument(
    vscode.Uri.file(samplePath),
  );
  await vscode.window.showTextDocument(document);
  const position = new vscode.Position(0, 0);

  const definitions = normalizeLocations(
    await vscode.commands.executeCommand<
      | vscode.Location
      | vscode.LocationLink
      | readonly vscode.Location[]
      | readonly vscode.LocationLink[]
    >("vscode.executeDefinitionProvider", document.uri, position),
  );
  assert.ok(
    definitions.length > 0,
    "Definition fallback returned no locations",
  );
  assert.equal(definitions[0].uri.fsPath, document.uri.fsPath);

  const hovers =
    (await vscode.commands.executeCommand<readonly vscode.Hover[]>(
      "vscode.executeHoverProvider",
      document.uri,
      position,
    )) ?? [];
  assert.ok(hovers.length > 0, "Hover fallback returned no hover");
  const text = hoverText(hovers);
  assert.match(text, /FOO/);
  assert.doesNotMatch(
    text,
    /cached definitions|place of origin|workspace index/i,
  );
}

export async function run(): Promise<void> {
  requireEnv("JOVIAL_INTEGRATION_LOG", LOG_PATH);
  requireEnv("JOVIAL_INTEGRATION_SAMPLE", SAMPLE_PATH);
  requireEnv("JOVIAL_INTEGRATION_WORKSPACE", WORKSPACE_PATH);

  await testAutostartAndInitializePayload();
  await testConfigRestartResendsSettings();
  await testFeatureToggleRestartResendsSettings();
  await testDiagnosticsFeatureSettingDoesNotRestartHints();
  await testCustomFeatureProfileRestartResendsSettings();
  await testRestartCommand();
  await testWatchedFileNotifications();
  await testLsifRefreshCommand();
  await testMiddlewareLsifFallback();
}
