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
  timeoutMs = 30000,
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
  assert.equal(jovial?.background?.["indexBudgetMs"], 11);
  assert.equal(jovial?.background?.["diagBatchSize"], 7);
  assert.equal(jovial?.server?.["parseMaxFileBytes"], 123456);
  assert.equal(jovial?.server?.["pressureSoftMb"], 256);
  assert.equal(jovial?.server?.["pressureCriticalMb"], 384);
}

async function testConfigRestartResendsSettings(): Promise<void> {
  const before = (await waitForInitializeCount(1)).length;
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

async function testRestartCommand(): Promise<void> {
  const before = (await waitForInitializeCount(1)).length;
  await sleep(1000);
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
  const before = readEvents().filter(
    (event) =>
      event.type === "executeCommand" &&
      event.command === "jovial.dumpLsifIndex",
  ).length;

  await setConfig("lsif.fastPath", false);
  await sleep(500);
  await vscode.commands.executeCommand("jovial.refreshLsifCache");
  await sleep(500);
  const disabledCount = readEvents().filter(
    (event) =>
      event.type === "executeCommand" &&
      event.command === "jovial.dumpLsifIndex",
  ).length;
  assert.equal(
    disabledCount,
    before,
    "LSIF refresh should not run while fast path is disabled",
  );

  await setConfig("lsif.fastPath", true);
  await activateExtension();
  await vscode.commands.executeCommand("jovial.refreshLsifCache");
  await waitFor("LSIF refresh executeCommand", () => {
    const events = readEvents().filter(
      (event) =>
        event.type === "executeCommand" &&
        event.command === "jovial.dumpLsifIndex",
    );
    return events.length > before ? events : undefined;
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

async function testMiddlewareLsifFallback(): Promise<void> {
  await setConfig("lsif.fastPath", true);
  await activateExtension();
  await vscode.commands.executeCommand("jovial.refreshLsifCache");
  await waitFor("LSIF cache refresh", () => {
    const events = readEvents().filter(
      (event) =>
        event.type === "executeCommand" &&
        event.command === "jovial.dumpLsifIndex",
    );
    return events.length > 0 ? events : undefined;
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
  assert.match(hoverText(hovers), /FOO|Cached definitions/);
}

export async function run(): Promise<void> {
  requireEnv("JOVIAL_INTEGRATION_LOG", LOG_PATH);
  requireEnv("JOVIAL_INTEGRATION_SAMPLE", SAMPLE_PATH);
  requireEnv("JOVIAL_INTEGRATION_WORKSPACE", WORKSPACE_PATH);

  await testAutostartAndInitializePayload();
  await testConfigRestartResendsSettings();
  await testRestartCommand();
  await testWatchedFileNotifications();
  await testLsifRefreshCommand();
  await testMiddlewareLsifFallback();
}
