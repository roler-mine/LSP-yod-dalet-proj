// Module overview: Main VS Code extension entrypoint for client lifecycle, diagnostics, file watching, and LSIF fallback navigation.

import * as vscode from "vscode";
import * as cp from "child_process";
import * as fs from "fs";
import * as path from "path";
import { Worker } from "worker_threads";

import {
  LanguageClient,
  CloseAction,
  ErrorAction,
  ExecuteCommandRequest,
  Trace,
} from "vscode-languageclient/node";
import { DocumentDiagnosticRequest } from "vscode-languageserver-protocol";
import type {
  Diagnostic as ProtocolDiagnostic,
  DocumentDiagnosticParams,
  DocumentDiagnosticReport,
} from "vscode-languageserver-protocol";
import type {
  ExecuteCommandParams,
  LanguageClientOptions,
  StreamInfo,
} from "vscode-languageclient/node";
import { parseLsifDeltaPayload, parseLsifIndexPayload } from "./lsif_codec";
import { raceServerWithFallback } from "./provider_race";
import type {
  LsifLocationData,
  LsifReferenceData,
  LsifSymbolEntryData,
  ParsedLsifDelta,
  ParsedLsifIndex,
} from "./lsif_codec";
import { buildInitializationOptions, readJovialConfig } from "./jovial_config";
import type { JovialConfig, JovialSourceFileSet } from "./jovial_config";
import {
  WATCH_CHANGE_CHANGED,
  WATCH_CHANGE_CREATED,
  WATCH_CHANGE_DELETED,
  queueWatchedFileChange as enqueueWatchedFileChange,
  shouldIgnoreWatchedPath,
  takeWatchedFileBatch,
} from "./watched_file_queue";
import type { PendingWatchedFileChange } from "./watched_file_queue";
import {
  dirnamePath,
  lowestCommonAncestorPath,
  pathWithinRoot,
  pickBestWorkspaceRoot,
  watchPathKey,
} from "./workspace_paths";
import {
  DEFAULT_JOVIAL_SOURCE_EXTENSIONS,
  hasJovialSourceExtension,
  watcherGlobForSourceExtensions,
} from "./source_extensions";
import { sourceDiscoveryExcludeGlob } from "./ignored_paths";
import { resolveServerPath } from "./server_path";
import { registerExtensionHooks } from "./commands";
import { asFiniteInt, asRecord } from "./unknown_utils";
import {
  bindStartupNotifications,
  clearStartupStatus,
  refreshStartupStatusBar,
  setStatus,
  STARTUP_DIAG_TARGET_DEFAULT_MS,
  STARTUP_NAV_TARGET_DEFAULT_MS,
} from "./startup_status";
import {
  dumpAstUi as dumpAstUiView,
  dumpCstUi as dumpCstUiView,
  showSyntaxTreesUi as showSyntaxTreesUiView,
} from "./syntax_tree_ui";

// Extension-wide singleton state. Activation creates one client, one watcher,
// and background queues that are torn down together on stop.
let client: LanguageClient | undefined;
let fileWatcher: vscode.FileSystemWatcher | undefined;
let serverProcess: cp.ChildProcess | undefined;
let serverProcessGeneration = 0;

// Timing and chunk-size guardrails keep large-workspace background work from
// monopolizing the extension host.
const WATCH_FLUSH_DELAY_MS = 150;
const WATCH_CHUNK_SIZE = 256;
const WATCH_FORCE_FLUSH_SIZE = 2000;
const AUTO_RESTART_DELAY_MS = 1200;
const AUTO_RESTART_WINDOW_MS = 120000;
const AUTO_RESTART_MAX_ATTEMPTS = 5;
const SERVER_STOP_TIMEOUT_MS = 30000;
const SERVER_KILL_TIMEOUT_MS = 15000;
const LSIF_REFRESH_DEBOUNCE_MS = 800;
const LSIF_REFRESH_MIN_INTERVAL_MS = 1200;
const LSIF_MAX_FAST_RESULTS = 300;
const LSIF_FALLBACK_RACE_BUDGET_MS = 1000;
const LSIF_HOVER_FALLBACK_RACE_BUDGET_MS = 1000;
const LSIF_FALLBACK_SERVER_GRACE_MS = 0;
const PROVIDER_LATE_BUDGET_MS = 1500;
const DIAGNOSTIC_REFRESH_INTERVAL_MS = 30000;
const DIAGNOSTIC_REFRESH_STARTUP_DELAY_MS = 1000;
const DIAGNOSTIC_REFRESH_EDITOR_DELAY_MS = 1000;
const DIAGNOSTIC_REFRESH_EDIT_DELAY_MS = 200;
const DIAGNOSTIC_REFRESH_SOURCE_CHUNK_SIZE = 64;
const DIAGNOSTIC_REFRESH_PENDING_CHUNK_SIZE = 128;
const DIAGNOSTIC_REFRESH_NOTIFY_CHUNK_SIZE = 96;
const JOVIAL_SOURCE_PROBE_BYTES = 16384;

// File watching and diagnostics receive bursty input, so this module coalesces
// events before forwarding them to the language server.
const pendingWatchedFileChanges = new Map<string, PendingWatchedFileChange>();
let pendingWatchedFileFlushTimer: NodeJS.Timeout | undefined;
let watchedFileFlushInFlight = false;
let serverStopRequested = false;
let pendingAutoRestartTimer: NodeJS.Timeout | undefined;
let autoRestartAttempts: number[] = [];
let startInProgress = false;
let currentSourceFileSets: JovialSourceFileSet[] = [];
let diagnosticRefreshTimer: NodeJS.Timeout | undefined;
let diagnosticRefreshInFlight = false;
let diagnosticRefreshPending = false;
let diagnosticSourceRefreshCursor = 0;
const pendingDiagnosticRefreshUris = new Set<string>();
let liveEditDiagnosticCollection: vscode.DiagnosticCollection | undefined;

// The LSIF cache is shaped for fast editor-provider lookups: resolve a position
// to a symbol id, then use that symbol's precomputed navigation locations.
type LsifReference = LsifReferenceData;
type LsifSymbolEntry = LsifSymbolEntryData;
type LsifOccurrenceEntry = {
  symbolId: string;
  startLine: number;
  startCharacter: number;
  endLine: number;
  endCharacter: number;
};

type LsifIndexCache = {
  symbolsById: Map<string, LsifSymbolEntry>;
  symbolIdsByKey: Map<string, string[]>;
  occurrenceIndexByDoc: Map<string, Map<number, LsifOccurrenceEntry[]>>;
  symbolCount: number;
  docCount: number;
  revision: number;
};

type LsifDeltaPayload = ParsedLsifDelta;
type LsifRootState = {
  cache: LsifIndexCache | undefined;
  refreshTimer: NodeJS.Timeout | undefined;
  refreshInFlight: boolean;
  refreshPending: boolean;
  pendingReason: string | undefined;
  lastRefreshMs: number;
  lastContextUri: vscode.Uri | undefined;
};

type LsifWorkerTask = "parseIndex" | "parseDelta";
type LsifWorkerPendingRequest = {
  resolve: (value: unknown) => void;
  reject: (reason?: unknown) => void;
};

// Multi-root workspaces keep independent LSIF refresh state so one root's cache
// refreshes do not invalidate another root's fast-path answers.
const lsifRootStates = new Map<string, LsifRootState>();
let lsifWorker: Worker | undefined;
let lsifWorkerRequestSeq = 0;
let lsifWorkerShutdownRequested = false;
let lsifWorkerFallbackLogged = false;
const lsifWorkerPending = new Map<number, LsifWorkerPendingRequest>();

// Root keys use normalized filesystem paths because VS Code may surface the
// same path with different casing or separators on different platforms.
function rootKeyForFolder(folder: vscode.WorkspaceFolder): string {
  return `folder:${watchPathKey(folder.uri.fsPath)}`;
}

function rootKeyForFileUri(uri: vscode.Uri): string {
  return `file:${watchPathKey(uri.fsPath)}`;
}

function normalizeLsifDocUri(uriRaw: string): string | undefined {
  try {
    return normalizeNavUri(vscode.Uri.parse(uriRaw)).toString();
  } catch {
    return undefined;
  }
}

function pickBestWorkspaceFolderForUri(
  uri: vscode.Uri,
): vscode.WorkspaceFolder | undefined {
  const folders = vscode.workspace.workspaceFolders ?? [];
  if (folders.length === 0) return undefined;
  const direct = vscode.workspace.getWorkspaceFolder(uri);
  if (direct) return direct;
  if (uri.scheme !== "file") return folders[0];
  const bestRoot = pickBestWorkspaceRoot(
    uri.fsPath,
    folders.map((folder) => folder.uri.fsPath),
  );
  if (!bestRoot) return folders[0];
  const bestKey = watchPathKey(bestRoot);
  return (
    folders.find((folder) => watchPathKey(folder.uri.fsPath) === bestKey) ??
    folders[0]
  );
}

function firstPreferredLsifUri(): vscode.Uri | undefined {
  const active = vscode.window.activeTextEditor?.document;
  if (
    active &&
    active.languageId === "jovial" &&
    active.uri.scheme === "file"
  ) {
    return normalizeNavUri(active.uri);
  }
  for (const doc of vscode.workspace.textDocuments) {
    if (doc.languageId === "jovial" && doc.uri.scheme === "file") {
      return normalizeNavUri(doc.uri);
    }
  }
  const firstFolder = vscode.workspace.workspaceFolders?.[0];
  if (firstFolder) return normalizeNavUri(firstFolder.uri);
  return undefined;
}

type LsifRootContext = {
  rootKey: string;
  contextUri: vscode.Uri;
};

type ExecuteCommandRegistrationData = {
  id: string;
  registerOptions: {
    commands?: string[];
  };
};

type PatchableExecuteCommandFeature = {
  register?: (data: ExecuteCommandRegistrationData) => void;
  unregister?: (id: string) => void;
  clear?: () => void;
  __jovialSafeRegistrationInstalled?: boolean;
};

// Pick the LSIF cache bucket for a URI. Commands that are not tied to a file can
// fall back to the active Jovial editor or first workspace folder.
function resolveLsifRootContext(
  preferredUri?: vscode.Uri,
  allowFallback = true,
): LsifRootContext | undefined {
  const uri =
    preferredUri && preferredUri.scheme === "file"
      ? normalizeNavUri(preferredUri)
      : allowFallback
        ? firstPreferredLsifUri()
        : undefined;
  if (!uri || uri.scheme !== "file") return undefined;
  const folder = pickBestWorkspaceFolderForUri(uri);
  if (folder) {
    return {
      rootKey: rootKeyForFolder(folder),
      contextUri: normalizeNavUri(uri),
    };
  }
  return { rootKey: rootKeyForFileUri(uri), contextUri: normalizeNavUri(uri) };
}

function isCommandAlreadyRegisteredError(e: unknown): boolean {
  const message = e instanceof Error ? e.message : String(e);
  return message.includes("command") && message.includes("already exists");
}

function executeCommandFeatureOf(
  languageClient: LanguageClient,
): PatchableExecuteCommandFeature | undefined {
  return languageClient.getFeature(ExecuteCommandRequest.method) as
    | PatchableExecuteCommandFeature
    | undefined;
}

function installSafeExecuteCommandRegistration(
  languageClient: LanguageClient,
  output: vscode.OutputChannel,
): void {
  // Some server commands share ids with extension-owned UI commands. Patching
  // the dynamic registration hook lets startup continue while preserving the
  // command binding that VS Code already has.
  const feature = executeCommandFeatureOf(languageClient);
  if (!feature || feature.__jovialSafeRegistrationInstalled) return;

  const registrations = new Map<string, vscode.Disposable[]>();

  const disposeRegistration = (id: string): void => {
    const disposables = registrations.get(id);
    if (!disposables) return;
    for (const disposable of disposables) {
      disposable.dispose();
    }
    registrations.delete(id);
  };

  const executeServerBackedCommand = (
    command: string,
    args: unknown[],
  ): Thenable<unknown> => {
    const executeCommand = (nextCommand: string, nextArgs: unknown[]) => {
      const params: ExecuteCommandParams = {
        command: nextCommand,
        arguments: nextArgs,
      };
      return languageClient.sendRequest(ExecuteCommandRequest.type, params);
    };
    const middleware = languageClient.middleware.executeCommand;
    if (middleware) {
      return Promise.resolve(middleware(command, args, executeCommand));
    }
    return executeCommand(command, args);
  };

  feature.register = (data: ExecuteCommandRegistrationData): void => {
    disposeRegistration(data.id);

    const disposables: vscode.Disposable[] = [];
    const commands = data.registerOptions.commands ?? [];
    for (const command of commands) {
      try {
        disposables.push(
          vscode.commands.registerCommand(command, (...args: unknown[]) =>
            executeServerBackedCommand(command, args),
          ),
        );
      } catch (e) {
        if (isCommandAlreadyRegisteredError(e)) {
          output.appendLine(
            `Server command '${command}' is already registered; keeping the existing VS Code command binding.`,
          );
          continue;
        }

        for (const disposable of disposables) {
          disposable.dispose();
        }
        throw e;
      }
    }

    registrations.set(data.id, disposables);
  };

  feature.unregister = (id: string): void => {
    disposeRegistration(id);
  };

  feature.clear = (): void => {
    for (const id of [...registrations.keys()]) {
      disposeRegistration(id);
    }
  };

  feature.__jovialSafeRegistrationInstalled = true;
}

function clearExecuteCommandRegistrations(
  languageClient: LanguageClient | undefined,
  output: vscode.OutputChannel,
): void {
  if (!languageClient) return;
  try {
    executeCommandFeatureOf(languageClient)?.clear?.();
  } catch (e) {
    output.appendLine(
      `Failed to clear server command registrations: ${String(e)}`,
    );
  }
}

function isOpenFileDocumentPath(fsPath: string): boolean {
  const target = watchPathKey(fsPath);
  return vscode.workspace.textDocuments.some(
    (doc) =>
      doc.uri.scheme === "file" && watchPathKey(doc.uri.fsPath) === target,
  );
}

function isJovialDiagnosticDocument(doc: vscode.TextDocument): boolean {
  return (
    doc.languageId === "jovial" &&
    (doc.uri.scheme === "file" || doc.uri.scheme === "untitled")
  );
}

function updateHugeLiveEditDiagnostics(doc: vscode.TextDocument): void {
  const collection = liveEditDiagnosticCollection;
  if (!collection) return;
  collection.delete(doc.uri);
}

function reconcileLiveEditDiagnostics(uri: vscode.Uri): void {
  liveEditDiagnosticCollection?.delete(uri);
}

// Diagnostics are refreshed in priority order: open editors first, then files
// touched by recent events, then a round-robin slice of the known source set.
function knownSourceDiagnosticUris(skipUris: ReadonlySet<string>): string[] {
  const seen = new Set<string>();
  const uris: string[] = [];
  for (const set of currentSourceFileSets) {
    for (const raw of set.fileUris) {
      try {
        const uri = vscode.Uri.parse(raw).toString();
        if (skipUris.has(uri) || seen.has(uri)) continue;
        seen.add(uri);
        uris.push(uri);
      } catch {
        continue;
      }
    }
  }
  uris.sort((a, b) => a.localeCompare(b));
  return uris;
}

function nextKnownSourceDiagnosticUris(
  skipUris: ReadonlySet<string>,
): string[] {
  const uris = knownSourceDiagnosticUris(skipUris);
  if (uris.length === 0) {
    diagnosticSourceRefreshCursor = 0;
    return [];
  }

  const chunkSize = Math.min(DIAGNOSTIC_REFRESH_SOURCE_CHUNK_SIZE, uris.length);
  const start = diagnosticSourceRefreshCursor % uris.length;
  const out: string[] = [];
  for (let i = 0; i < chunkSize; i += 1) {
    out.push(uris[(start + i) % uris.length]);
  }
  diagnosticSourceRefreshCursor = (start + chunkSize) % uris.length;
  return out;
}

function takePendingDiagnosticRefreshUris(
  skipUris: ReadonlySet<string>,
): string[] {
  const out: string[] = [];
  for (const uri of pendingDiagnosticRefreshUris) {
    pendingDiagnosticRefreshUris.delete(uri);
    if (skipUris.has(uri)) continue;
    out.push(uri);
    if (out.length >= DIAGNOSTIC_REFRESH_PENDING_CHUNK_SIZE) break;
  }
  return out;
}

function clearDiagnosticRefreshTimer(): void {
  if (!diagnosticRefreshTimer) return;
  clearTimeout(diagnosticRefreshTimer);
  diagnosticRefreshTimer = undefined;
}

function diagnosticRefreshUris(): string[] {
  const docs = vscode.workspace.textDocuments.filter(
    isJovialDiagnosticDocument,
  );
  const openUris = docs.map((doc) => doc.uri.toString());
  const openUriSet = new Set(openUris);
  const pendingUris = takePendingDiagnosticRefreshUris(openUriSet);
  const skippedUris = new Set([...openUris, ...pendingUris]);
  // Round-robin background refresh prevents very large workspaces from starving
  // open-editor diagnostics.
  const sourceUris = nextKnownSourceDiagnosticUris(skippedUris);
  return [...openUris, ...pendingUris, ...sourceUris];
}

function chunked<T>(items: readonly T[], chunkSize: number): T[][] {
  const chunks: T[][] = [];
  for (let i = 0; i < items.length; i += chunkSize) {
    chunks.push(items.slice(i, i + chunkSize));
  }
  return chunks;
}

async function sendDiagnosticRefreshNotifications(
  languageClient: LanguageClient,
  output: vscode.OutputChannel,
  reason: string,
  uris: readonly string[],
): Promise<void> {
  for (const batch of chunked(uris, DIAGNOSTIC_REFRESH_NOTIFY_CHUNK_SIZE)) {
    if (client !== languageClient) break;
    try {
      // This extension/server notification is intentionally lighter than
      // textDocument/diagnostic: the server revalidates and publishes once,
      // without echoing the same diagnostics back in a discarded response body.
      await languageClient.sendNotification("jovial/refreshDiagnostics", {
        reason,
        uris: batch,
      });
    } catch (e) {
      output.appendLine(
        `Jovial diagnostic refresh (${reason}) failed for ${batch.length} file(s): ${String(e)}`,
      );
    }
  }
}

async function refreshOpenDocumentDiagnostics(
  output: vscode.OutputChannel,
  reason: string,
): Promise<void> {
  // Collapse overlapping requests into at most one follow-up pass. That keeps
  // save storms responsive without dropping the newest refresh signal.
  const c = client;
  if (!c) return;
  if (diagnosticRefreshInFlight) {
    diagnosticRefreshPending = true;
    return;
  }

  diagnosticRefreshInFlight = true;
  try {
    do {
      diagnosticRefreshPending = false;
      const uris = diagnosticRefreshUris();
      if (uris.length > 0) {
        await sendDiagnosticRefreshNotifications(c, output, reason, uris);
      }
    } while (diagnosticRefreshPending && client === c);
  } finally {
    diagnosticRefreshInFlight = false;
  }
}

async function refreshDiagnosticsNow(
  output: vscode.OutputChannel,
  reason: string,
  preferredUri?: vscode.Uri,
): Promise<void> {
  if (preferredUri) {
    const c = client;
    if (!c) return;
    const uri = normalizeNavUri(preferredUri).toString();
    pendingDiagnosticRefreshUris.delete(uri);
    await sendDiagnosticRefreshNotifications(c, output, reason, [uri]);
    return;
  }
  await refreshOpenDocumentDiagnostics(output, reason);
}

type DiagnosticPullSummary = {
  kind: string;
  count: number;
  messages: string[];
};

function protocolDiagnosticMessage(diag: ProtocolDiagnostic): string {
  return typeof diag.message === "string"
    ? diag.message
    : JSON.stringify(diag.message);
}

async function pullDiagnosticsNow(
  preferredUri: vscode.Uri,
): Promise<DiagnosticPullSummary> {
  if (!client) {
    throw new Error("Jovial LSP client is not running.");
  }
  const params: DocumentDiagnosticParams = {
    textDocument: { uri: preferredUri.toString() },
  };
  const protocolClient = client as unknown as {
    sendRequest<T>(type: unknown, params: unknown): Promise<T>;
  };
  const report = await protocolClient.sendRequest<DocumentDiagnosticReport>(
    DocumentDiagnosticRequest.type,
    params,
  );
  if (report.kind !== "full") {
    return { kind: report.kind, count: 0, messages: [] };
  }
  return {
    kind: report.kind,
    count: report.items.length,
    messages: report.items.map(protocolDiagnosticMessage),
  };
}

function scheduleDiagnosticRefresh(
  output: vscode.OutputChannel,
  delayMs = DIAGNOSTIC_REFRESH_INTERVAL_MS,
  force = false,
  reason = "periodic",
): void {
  if (!client) return;
  if (force) clearDiagnosticRefreshTimer();
  if (diagnosticRefreshTimer) return;
  diagnosticRefreshTimer = setTimeout(
    () => {
      diagnosticRefreshTimer = undefined;
      void refreshOpenDocumentDiagnostics(output, reason).finally(() => {
        if (client) {
          scheduleDiagnosticRefresh(output, DIAGNOSTIC_REFRESH_INTERVAL_MS);
        }
      });
    },
    Math.max(0, delayMs),
  );
}

function clearWatchedFileFlushTimer(): void {
  if (!pendingWatchedFileFlushTimer) return;
  clearTimeout(pendingWatchedFileFlushTimer);
  pendingWatchedFileFlushTimer = undefined;
}

function resetWatchedFileStreamingState(): void {
  clearWatchedFileFlushTimer();
  pendingWatchedFileChanges.clear();
  watchedFileFlushInFlight = false;
}

async function flushWatchedFileChanges(
  output: vscode.OutputChannel,
): Promise<void> {
  // Stream watched-file events in bounded batches. If a change reshapes source
  // roots, defer the cheap cache update and run full source discovery instead.
  clearWatchedFileFlushTimer();
  if (
    watchedFileFlushInFlight ||
    pendingWatchedFileChanges.size === 0 ||
    !client
  )
    return;

  watchedFileFlushInFlight = true;
  let sentAny = false;
  let lastSentUri: vscode.Uri | undefined;
  let sourceFileSetRefreshReason: string | undefined;
  const cfg = getConfig();
  try {
    while (client && pendingWatchedFileChanges.size > 0) {
      const rawBatch = takeWatchedFileBatch(
        pendingWatchedFileChanges,
        WATCH_CHUNK_SIZE,
      );
      const batch = await filterWatchedSourceChanges(rawBatch, cfg, output);
      if (batch.length === 0) break;
      lastSentUri = vscode.Uri.file(batch[0].fsPath);
      if (!sourceFileSetRefreshReason) {
        sourceFileSetRefreshReason = sourceFileSetRefreshReasonForChanges(
          batch,
          cfg,
        );
      }
      const params = {
        changes: batch.map((c) => ({
          uri: vscode.Uri.file(c.fsPath).toString(),
          type: c.type,
        })),
      };
      for (const change of params.changes) {
        pendingDiagnosticRefreshUris.add(change.uri);
      }
      await client.sendNotification("workspace/didChangeWatchedFiles", params);
      if (!sourceFileSetRefreshReason) {
        updateCachedSourceFileSetsForChanges(batch, cfg);
      }
      sentAny = true;
    }
    if (sourceFileSetRefreshReason && client) {
      await refreshSourceFileSets(output, sourceFileSetRefreshReason);
    }
  } catch (e) {
    output.appendLine(`Watched-file flush failed: ${String(e)}`);
  } finally {
    watchedFileFlushInFlight = false;
    if (sentAny) {
      // Refresh LSIF cache after filesystem-level changes so nav fast path stays current.
      scheduleLsifRefresh(output, "watched-files", lastSentUri);
      scheduleDiagnosticRefresh(
        output,
        DIAGNOSTIC_REFRESH_EDITOR_DELAY_MS,
        true,
        "watched-files",
      );
    }
    if (pendingWatchedFileChanges.size > 0 && client) {
      scheduleWatchedFileFlush(output);
    }
  }
}

function scheduleWatchedFileFlush(output: vscode.OutputChannel): void {
  if (pendingWatchedFileChanges.size === 0) return;
  if (pendingWatchedFileChanges.size >= WATCH_FORCE_FLUSH_SIZE) {
    void flushWatchedFileChanges(output);
    return;
  }
  if (pendingWatchedFileFlushTimer) return;
  pendingWatchedFileFlushTimer = setTimeout(() => {
    pendingWatchedFileFlushTimer = undefined;
    void flushWatchedFileChanges(output);
  }, WATCH_FLUSH_DELAY_MS);
}

function getConfig() {
  return readJovialConfig(vscode.workspace.getConfiguration("jovial"));
}

function ensureServerExecutable(
  filePath: string,
  output: vscode.OutputChannel,
): void {
  if (process.platform === "win32") return;

  try {
    const stat = fs.statSync(filePath);
    if ((stat.mode & 0o111) !== 0) return;

    fs.chmodSync(filePath, stat.mode | 0o755);
    output.appendLine(
      `Set executable permission on server binary: ${filePath}`,
    );
  } catch (err) {
    output.appendLine(
      `Warning: failed to ensure server executable permission: ${String(err)}`,
    );
  }
}

function parseFileUriFsPath(raw: string | undefined): string | undefined {
  if (!raw) return undefined;
  try {
    const uri = vscode.Uri.parse(raw);
    return uri.scheme === "file" ? uri.fsPath : undefined;
  } catch {
    return undefined;
  }
}

function sourceFileSetRootPath(set: JovialSourceFileSet): string | undefined {
  return parseFileUriFsPath(set.rootUri);
}

function sourceFileSetWorkspacePath(
  set: JovialSourceFileSet,
): string | undefined {
  return parseFileUriFsPath(set.workspaceUri);
}

function sourceFileSetForWorkspaceFolder(
  folder: vscode.WorkspaceFolder,
): JovialSourceFileSet | undefined {
  const folderKey = watchPathKey(folder.uri.fsPath);
  return currentSourceFileSets.find((set) => {
    const workspacePath = sourceFileSetWorkspacePath(set);
    if (workspacePath) return watchPathKey(workspacePath) === folderKey;
    const rootPath = sourceFileSetRootPath(set);
    return rootPath ? pathWithinRoot(watchPathKey(rootPath), folderKey) : false;
  });
}

function sourceFileSetContainsPath(
  set: JovialSourceFileSet,
  fsPath: string,
): boolean {
  const pathKey = watchPathKey(fsPath);
  const uris = [...set.fileUris, ...(set.assemblyFileUris ?? [])];
  return uris.some((raw) => {
    const filePath = parseFileUriFsPath(raw);
    return filePath ? watchPathKey(filePath) === pathKey : false;
  });
}

function addFileUriToList(list: string[], uri: vscode.Uri): void {
  const pathKey = watchPathKey(uri.fsPath);
  if (
    list.some((raw) => {
      const filePath = parseFileUriFsPath(raw);
      return filePath ? watchPathKey(filePath) === pathKey : false;
    })
  ) {
    return;
  }
  list.push(uri.toString());
  list.sort((a, b) => a.localeCompare(b));
}

function addFileUriToCachedSet(
  set: JovialSourceFileSet,
  uri: vscode.Uri,
): void {
  addFileUriToList(set.fileUris, uri);
}

function addAssemblyFileUriToCachedSet(
  set: JovialSourceFileSet,
  uri: vscode.Uri,
): void {
  if (!set.assemblyFileUris) set.assemblyFileUris = [];
  addFileUriToList(set.assemblyFileUris, uri);
}

function removeFilePathFromCachedSet(
  set: JovialSourceFileSet,
  fsPath: string,
): void {
  const pathKey = watchPathKey(fsPath);
  set.fileUris = set.fileUris.filter((raw) => {
    const filePath = parseFileUriFsPath(raw);
    return filePath ? watchPathKey(filePath) !== pathKey : true;
  });
  if (set.assemblyFileUris) {
    set.assemblyFileUris = set.assemblyFileUris.filter((raw) => {
      const filePath = parseFileUriFsPath(raw);
      return filePath ? watchPathKey(filePath) !== pathKey : true;
    });
  }
}

function isDefaultJovialSourcePath(fsPath: string): boolean {
  return hasJovialSourceExtension(fsPath, DEFAULT_JOVIAL_SOURCE_EXTENSIONS);
}

function isConfiguredExtraSourcePath(
  fsPath: string,
  cfg: JovialConfig,
): boolean {
  return (
    !isDefaultJovialSourcePath(fsPath) &&
    hasJovialSourceExtension(fsPath, cfg.sourceExtensions)
  );
}

function isAssemblySourcePath(fsPath: string, cfg: JovialConfig): boolean {
  return hasJovialSourceExtension(fsPath, cfg.assemblyExtensions);
}

function watchedWorkspaceExtensions(cfg: JovialConfig): string[] {
  return Array.from(new Set([...cfg.sourceExtensions, ...cfg.assemblyExtensions]));
}

function configuredExtraSourceExtensions(cfg: JovialConfig): string[] {
  const defaults = new Set(DEFAULT_JOVIAL_SOURCE_EXTENSIONS);
  return cfg.extraSourceFileExtensions.filter((ext) => !defaults.has(ext));
}

function isKnownSourcePath(fsPath: string): boolean {
  return currentSourceFileSets.some((set) =>
    sourceFileSetContainsPath(set, fsPath),
  );
}

const jovialSourceSignatureWords = new Set([
  "START",
  "PROGRAM",
  "COMPOOL",
  "ICOMPOOL",
  "DEFINE",
  "TYPE",
  "BLOCK",
  "DEF",
  "REF",
  "PROC",
  "ITEM",
  "TABLE",
  "STATIC",
  "CONSTANT",
  "READONLY",
  "INLINE",
  "OVERLAY",
]);

function probeFirstWordUpper(text: string): string {
  const match = /^[A-Za-z_$][A-Za-z0-9_$']*/.exec(text.trimStart());
  return match ? match[0].toUpperCase() : "";
}

function looksLikeJovialSourceSnippet(rawText: string): boolean {
  if (!rawText || rawText.includes("\u0000")) return false;
  const text = rawText.replace(/^\uFEFF/, "");
  const upper = text.toUpperCase();
  if (
    /\bSTART\b/.test(upper) &&
    /\b(TERM|PROGRAM|COMPOOL|ICOMPOOL|PROC|ITEM|TABLE)\b/.test(upper)
  ) {
    return true;
  }

  const lines = text.split(/\r?\n/, 32);
  for (const rawLine of lines) {
    const trimmed = rawLine.trim();
    if (!trimmed || trimmed.startsWith("%") || trimmed.startsWith('"')) {
      continue;
    }
    const directive = trimmed.startsWith("!") ? trimmed.slice(1) : trimmed;
    if (jovialSourceSignatureWords.has(probeFirstWordUpper(directive))) {
      return true;
    }
    const colon = directive.indexOf(":");
    if (
      colon >= 0 &&
      jovialSourceSignatureWords.has(
        probeFirstWordUpper(directive.slice(colon + 1)),
      )
    ) {
      return true;
    }
  }
  return false;
}

async function readFilePrefix(fsPath: string): Promise<string | undefined> {
  let handle: fs.promises.FileHandle | undefined;
  try {
    handle = await fs.promises.open(fsPath, "r");
    const buffer = Buffer.alloc(JOVIAL_SOURCE_PROBE_BYTES);
    const { bytesRead } = await handle.read(buffer, 0, buffer.length, 0);
    return buffer.subarray(0, bytesRead).toString("utf8");
  } catch {
    return undefined;
  } finally {
    await handle?.close().catch(() => undefined);
  }
}

async function shouldAcceptSourceUri(
  uri: vscode.Uri,
  cfg: JovialConfig,
): Promise<boolean> {
  if (uri.scheme !== "file") return false;
  if (!isConfiguredExtraSourcePath(uri.fsPath, cfg)) return true;
  const prefix = await readFilePrefix(uri.fsPath);
  return prefix !== undefined && looksLikeJovialSourceSnippet(prefix);
}

async function filterDiscoveredSourceUris(
  folder: vscode.WorkspaceFolder,
  uris: readonly vscode.Uri[],
  cfg: JovialConfig,
  output: vscode.OutputChannel,
): Promise<vscode.Uri[]> {
  const accepted: vscode.Uri[] = [];
  let skippedExtra = 0;
  for (const uri of uris) {
    if (await shouldAcceptSourceUri(uri, cfg)) {
      accepted.push(uri);
    } else if (
      uri.scheme === "file" &&
      isConfiguredExtraSourcePath(uri.fsPath, cfg)
    ) {
      skippedExtra += 1;
    }
  }
  if (skippedExtra > 0) {
    output.appendLine(
      `Jovial source discovery: skipped ${skippedExtra} configured-extension file(s) under ${folder.uri.fsPath} that did not look like Jovial source.`,
    );
  }
  return accepted;
}

async function shouldForwardWatchedSourceChange(
  change: PendingWatchedFileChange,
  cfg: JovialConfig,
): Promise<boolean> {
  if (!isConfiguredExtraSourcePath(change.fsPath, cfg)) return true;
  if (isKnownSourcePath(change.fsPath)) return true;
  if (change.type === WATCH_CHANGE_DELETED) return false;
  const prefix = await readFilePrefix(change.fsPath);
  return prefix !== undefined && looksLikeJovialSourceSnippet(prefix);
}

async function filterWatchedSourceChanges(
  batch: readonly PendingWatchedFileChange[],
  cfg: JovialConfig,
  output: vscode.OutputChannel,
): Promise<PendingWatchedFileChange[]> {
  const accepted: PendingWatchedFileChange[] = [];
  let skippedExtra = 0;
  for (const change of batch) {
    if (await shouldForwardWatchedSourceChange(change, cfg)) {
      accepted.push(change);
    } else {
      skippedExtra += 1;
    }
  }
  if (skippedExtra > 0) {
    output.appendLine(
      `Jovial watcher: skipped ${skippedExtra} configured-extension change(s) that did not look like Jovial source.`,
    );
  }
  return accepted;
}

function sourceFileSetForFolder(
  folder: vscode.WorkspaceFolder,
  uris: readonly vscode.Uri[],
  assemblyUris: readonly vscode.Uri[],
  cfg: JovialConfig,
  searchTruncated: boolean,
): JovialSourceFileSet | undefined {
  const rootKey = watchPathKey(folder.uri.fsPath);
  const seen = new Set<string>();
  const fileUris: string[] = [];
  const assemblyFileUris: string[] = [];
  const filePaths: string[] = [];

  const addUri = (
    uri: vscode.Uri,
    targetUris: string[],
    extensions: readonly string[],
  ) => {
    if (uri.scheme !== "file") return;
    const pathKey = watchPathKey(uri.fsPath);
    if (!pathWithinRoot(pathKey, rootKey)) return;
    if (
      shouldIgnoreWatchedPath(
        uri.fsPath,
        process.platform,
        extensions,
      )
    )
      return;
    if (seen.has(pathKey)) return;
    seen.add(pathKey);
    targetUris.push(uri.toString());
    filePaths.push(uri.fsPath);
  };

  for (const uri of uris) addUri(uri, fileUris, cfg.sourceExtensions);
  for (const uri of assemblyUris)
    addUri(uri, assemblyFileUris, cfg.assemblyExtensions);

  fileUris.sort((a, b) => a.localeCompare(b));
  assemblyFileUris.sort((a, b) => a.localeCompare(b));
  filePaths.sort((a, b) => watchPathKey(a).localeCompare(watchPathKey(b)));
  if (filePaths.length === 0) return undefined;

  const rootPath = lowestCommonAncestorPath(
    filePaths.map((filePath) => dirnamePath(filePath)),
  );
  return {
    workspaceUri: folder.uri.toString(),
    rootUri: vscode.Uri.file(rootPath).toString(),
    fileUris,
    assemblyFileUris,
    searchTruncated,
    inferred: true,
    reason: "minimal-common-container",
    flatNamespace: true,
  };
}

async function discoverSourceFileSets(
  cfg: JovialConfig,
  output: vscode.OutputChannel,
): Promise<JovialSourceFileSet[]> {
  // The server gets an inferred source-set snapshot before initialize so it can
  // start indexing without waiting for workspace notifications.
  const folders = vscode.workspace.workspaceFolders ?? [];
  if (folders.length === 0) return [];

  const exclude = sourceDiscoveryExcludeGlob();
  const defaultGlob = watcherGlobForSourceExtensions(
    DEFAULT_JOVIAL_SOURCE_EXTENSIONS,
  );
  const extraExtensions = configuredExtraSourceExtensions(cfg);
  const extraGlob =
    extraExtensions.length > 0
      ? watcherGlobForSourceExtensions(extraExtensions)
      : undefined;
  const assemblyGlob = watcherGlobForSourceExtensions(cfg.assemblyExtensions);
  const sets: JovialSourceFileSet[] = [];
  for (const folder of folders) {
    const defaultInclude = new vscode.RelativePattern(folder, defaultGlob);
    const defaultUris = await vscode.workspace.findFiles(
      defaultInclude,
      exclude,
      cfg.maxStartupFiles,
    );
    const remainingExtraBudget = Math.max(
      0,
      cfg.maxStartupFiles - defaultUris.length,
    );
    const extraUris =
      extraGlob && remainingExtraBudget > 0
        ? await vscode.workspace.findFiles(
            new vscode.RelativePattern(folder, extraGlob),
            exclude,
            remainingExtraBudget,
          )
        : [];
    const assemblyStartupBudget = Math.max(128, cfg.maxStartupFiles);
    const assemblyUris = await vscode.workspace.findFiles(
      new vscode.RelativePattern(folder, assemblyGlob),
      exclude,
      assemblyStartupBudget,
    );
    const searchTruncated =
      defaultUris.length >= cfg.maxStartupFiles ||
      (extraGlob !== undefined && extraUris.length >= remainingExtraBudget) ||
      assemblyUris.length >= assemblyStartupBudget;
    const sourceUris = await filterDiscoveredSourceUris(
      folder,
      [...defaultUris, ...extraUris],
      cfg,
      output,
    );
    const set = sourceFileSetForFolder(
      folder,
      sourceUris,
      assemblyUris,
      cfg,
      searchTruncated,
    );
    if (!set) {
      output.appendLine(
        `Jovial source discovery: 0 files under ${folder.uri.fsPath}`,
      );
      continue;
    }
    const rootPath = sourceFileSetRootPath(set) ?? set.rootUri;
    output.appendLine(
      `[JOVIAL] inferred source root: ${rootPath}\n` +
        `[JOVIAL] reason: minimal-common-container\n` +
        `[JOVIAL] files: ${set.fileUris.length}, asm: ${
          set.assemblyFileUris?.length ?? 0
        }${
          searchTruncated ? " (truncated)" : ""
        }`,
    );
    sets.push(set);
  }
  return sets;
}

function sourceFileSetRefreshReasonForChanges(
  changes: readonly PendingWatchedFileChange[],
  cfg: JovialConfig,
): string | undefined {
  // A changed/deleted source/support file can affect dependency shape; a new file only
  // forces rediscovery when it sits outside the currently inferred root.
  const watchedExtensions = watchedWorkspaceExtensions(cfg);
  for (const change of changes) {
    if (
      shouldIgnoreWatchedPath(
        change.fsPath,
        process.platform,
        watchedExtensions,
      )
    ) {
      continue;
    }

    if (change.type === WATCH_CHANGE_DELETED) return "source file deletion";
    if (change.type === WATCH_CHANGE_CHANGED) return "source file change";
    if (change.type !== WATCH_CHANGE_CREATED) continue;
    if (isAssemblySourcePath(change.fsPath, cfg)) {
      return "new assembly file in workspace";
    }

    const folder = pickBestWorkspaceFolderForUri(
      vscode.Uri.file(change.fsPath),
    );
    if (!folder) continue;
    const set = sourceFileSetForWorkspaceFolder(folder);
    if (!set) return "new source file in workspace";

    const rootPath = sourceFileSetRootPath(set);
    if (!rootPath) return "new source file in workspace";
    if (!pathWithinRoot(watchPathKey(change.fsPath), watchPathKey(rootPath))) {
      return "new source file outside current root";
    }
  }
  return undefined;
}

function updateCachedSourceFileSetsForChanges(
  changes: readonly PendingWatchedFileChange[],
  cfg: JovialConfig,
): void {
  // When roots are stable, keep the cached file list fresh without running a
  // workspace-wide file search.
  const watchedExtensions = watchedWorkspaceExtensions(cfg);
  for (const change of changes) {
    if (
      shouldIgnoreWatchedPath(
        change.fsPath,
        process.platform,
        watchedExtensions,
      )
    ) {
      continue;
    }
    if (change.type === WATCH_CHANGE_DELETED) {
      for (const set of currentSourceFileSets) {
        removeFilePathFromCachedSet(set, change.fsPath);
      }
      continue;
    }
    if (change.type !== WATCH_CHANGE_CREATED) continue;

    const folder = pickBestWorkspaceFolderForUri(
      vscode.Uri.file(change.fsPath),
    );
    if (!folder) continue;
    const set = sourceFileSetForWorkspaceFolder(folder);
    const rootPath = set ? sourceFileSetRootPath(set) : undefined;
    if (
      set &&
      rootPath &&
      pathWithinRoot(watchPathKey(change.fsPath), watchPathKey(rootPath))
    ) {
      const uri = vscode.Uri.file(change.fsPath);
      if (isAssemblySourcePath(change.fsPath, cfg)) {
        addAssemblyFileUriToCachedSet(set, uri);
      } else {
        addFileUriToCachedSet(set, uri);
      }
    }
  }
}

function workspaceKeyForSourceSet(set: JovialSourceFileSet): string {
  const workspacePath = sourceFileSetWorkspacePath(set);
  if (workspacePath) return watchPathKey(workspacePath);
  const rootPath = sourceFileSetRootPath(set);
  return rootPath ? watchPathKey(rootPath) : set.rootUri;
}

function logSourceFileSetRootChanges(
  output: vscode.OutputChannel,
  oldSets: readonly JovialSourceFileSet[],
  newSets: readonly JovialSourceFileSet[],
  reason: string,
): void {
  const oldByWorkspace = new Map(
    oldSets.map((set) => [workspaceKeyForSourceSet(set), set]),
  );
  const newByWorkspace = new Map(
    newSets.map((set) => [workspaceKeyForSourceSet(set), set]),
  );
  const keys = new Set([...oldByWorkspace.keys(), ...newByWorkspace.keys()]);
  for (const key of keys) {
    const oldSet = oldByWorkspace.get(key);
    const newSet = newByWorkspace.get(key);
    const oldRoot = oldSet ? sourceFileSetRootPath(oldSet) : undefined;
    const newRoot = newSet ? sourceFileSetRootPath(newSet) : undefined;
    if (oldRoot === newRoot) continue;
    output.appendLine(
      `[JOVIAL] source root changed after ${reason}\n` +
        `old root: ${oldRoot ?? "<none>"}\n` +
        `new root: ${newRoot ?? "<none>"}\n` +
        `files: ${newSet?.fileUris.length ?? 0}, asm: ${
          newSet?.assemblyFileUris?.length ?? 0
        }`,
    );
  }
}

async function refreshSourceFileSets(
  output: vscode.OutputChannel,
  reason: string,
): Promise<void> {
  // Full rediscovery is reserved for changes that may alter the source-root
  // boundary; the resulting snapshot is pushed to the running server.
  if (!client) return;
  const cfg = getConfig();
  const oldSets = currentSourceFileSets;
  const sets = await discoverSourceFileSets(cfg, output);
  currentSourceFileSets = sets;
  diagnosticSourceRefreshCursor = 0;
  logSourceFileSetRootChanges(output, oldSets, sets, reason);
  const truncated = sets.some((set) => set.searchTruncated);
  try {
    await client.sendNotification("jovial/sourceFileSets", {
      sets,
      sourceDiscoveryTruncated: truncated,
    });
  } catch (e) {
    output.appendLine(`Failed to send source file sets: ${String(e)}`);
  }
}

function lsifRootStateFor(rootKey: string): LsifRootState {
  const prev = lsifRootStates.get(rootKey);
  if (prev) return prev;
  const state: LsifRootState = {
    cache: undefined,
    refreshTimer: undefined,
    refreshInFlight: false,
    refreshPending: false,
    pendingReason: undefined,
    lastRefreshMs: 0,
    lastContextUri: undefined,
  };
  lsifRootStates.set(rootKey, state);
  return state;
}

function clearLsifRootTimer(state: LsifRootState): void {
  if (!state.refreshTimer) return;
  clearTimeout(state.refreshTimer);
  state.refreshTimer = undefined;
}

function resetLsifState(): void {
  for (const state of lsifRootStates.values()) {
    clearLsifRootTimer(state);
    state.refreshInFlight = false;
    state.refreshPending = false;
    state.pendingReason = undefined;
    state.cache = undefined;
    state.lastRefreshMs = 0;
    state.lastContextUri = undefined;
  }
  lsifRootStates.clear();
  disposeLsifWorker();
}

function rejectLsifWorkerPendingRequests(reason: string): void {
  if (lsifWorkerPending.size === 0) return;
  const err = new Error(reason);
  for (const pending of lsifWorkerPending.values()) {
    pending.reject(err);
  }
  lsifWorkerPending.clear();
}

function disposeLsifWorker(): void {
  if (!lsifWorker) return;
  const worker = lsifWorker;
  lsifWorker = undefined;
  lsifWorkerShutdownRequested = true;
  rejectLsifWorkerPendingRequests("LSIF worker disposed.");
  void worker.terminate().finally(() => {
    lsifWorkerShutdownRequested = false;
  });
}

function ensureLsifWorker(output: vscode.OutputChannel): Worker | undefined {
  // LSIF payloads can be large, so production builds parse them in a worker when
  // available and fall back to extension-thread parsing during development.
  if (lsifWorker) return lsifWorker;

  const workerPath = path.join(__dirname, "lsif_worker.js");
  if (!fs.existsSync(workerPath)) {
    if (!lsifWorkerFallbackLogged) {
      output.appendLine(
        "LSIF worker script is missing; falling back to extension-thread LSIF parsing.",
      );
      lsifWorkerFallbackLogged = true;
    }
    return undefined;
  }

  try {
    const worker = new Worker(workerPath);
    lsifWorkerFallbackLogged = false;
    worker.on("message", (raw: unknown) => {
      const rec = asRecord(raw);
      if (!rec) return;
      const idRaw = rec["id"];
      if (typeof idRaw !== "number") return;
      const pending = lsifWorkerPending.get(Math.trunc(idRaw));
      if (!pending) return;
      lsifWorkerPending.delete(Math.trunc(idRaw));
      if (rec["ok"] === true) {
        pending.resolve(rec["result"]);
        return;
      }
      const errRaw = rec["error"];
      pending.reject(
        new Error(
          typeof errRaw === "string"
            ? errRaw
            : "Unknown LSIF worker response error.",
        ),
      );
    });
    worker.on("error", (err) => {
      if (lsifWorker === worker) {
        lsifWorker = undefined;
      }
      output.appendLine(`LSIF worker error: ${String(err)}`);
      rejectLsifWorkerPendingRequests(`LSIF worker error: ${String(err)}`);
    });
    worker.on("exit", (code) => {
      if (lsifWorker === worker) {
        lsifWorker = undefined;
      }
      if (lsifWorkerShutdownRequested) {
        lsifWorkerShutdownRequested = false;
        return;
      }
      if (code !== 0) {
        output.appendLine(`LSIF worker exited unexpectedly with code ${code}.`);
      }
      rejectLsifWorkerPendingRequests(`LSIF worker exited with code ${code}.`);
    });
    lsifWorker = worker;
    return worker;
  } catch (e) {
    if (!lsifWorkerFallbackLogged) {
      output.appendLine(`Failed to start LSIF worker: ${String(e)}`);
      lsifWorkerFallbackLogged = true;
    }
    return undefined;
  }
}

async function runLsifWorkerTask<T>(
  output: vscode.OutputChannel,
  kind: LsifWorkerTask,
  payload: unknown,
  fallback: () => T | undefined,
): Promise<T | undefined> {
  const worker = ensureLsifWorker(output);
  if (!worker) return fallback();

  const id = ++lsifWorkerRequestSeq;
  try {
    const result = await new Promise<unknown>((resolve, reject) => {
      lsifWorkerPending.set(id, { resolve, reject });
      try {
        worker.postMessage({ id, kind, payload });
      } catch (e) {
        lsifWorkerPending.delete(id);
        reject(e);
      }
    });
    return (result ?? undefined) as T | undefined;
  } catch (e) {
    output.appendLine(
      `LSIF worker task '${kind}' failed; falling back to extension-thread parse: ${String(e)}`,
    );
    disposeLsifWorker();
    return fallback();
  }
}

function toLsifIndexCache(parsed: ParsedLsifIndex): LsifIndexCache {
  // Convert the server payload into derived indexes once so provider calls avoid
  // scanning every symbol/reference on each cursor movement.
  const symbolsById = new Map<string, LsifSymbolEntry>();
  for (const entry of parsed.symbols) symbolsById.set(entry.id, entry);

  const symbolIdsByKey = new Map<string, string[]>();
  for (const entry of parsed.keyIndex) {
    const ids = entry.symbolIds.filter((id) => symbolsById.has(id));
    if (ids.length <= 0) continue;
    symbolIdsByKey.set(
      entry.key,
      Array.from(new Set(ids)).sort((a, b) => a.localeCompare(b)),
    );
  }
  for (const sym of symbolsById.values()) {
    const prev = symbolIdsByKey.get(sym.key) ?? [];
    if (!prev.includes(sym.id)) prev.push(sym.id);
    prev.sort((a, b) => a.localeCompare(b));
    symbolIdsByKey.set(sym.key, prev);
  }

  const occurrenceIndexByDoc = new Map<
    string,
    Map<number, LsifOccurrenceEntry[]>
  >();
  const appendOccurrence = (
    docUri: string,
    line: number,
    entry: LsifOccurrenceEntry,
  ): void => {
    let byLine = occurrenceIndexByDoc.get(docUri);
    if (!byLine) {
      byLine = new Map<number, LsifOccurrenceEntry[]>();
      occurrenceIndexByDoc.set(docUri, byLine);
    }
    const prev = byLine.get(line) ?? [];
    prev.push(entry);
    byLine.set(line, prev);
  };
  for (const sym of symbolsById.values()) {
    for (const ref of sym.references) {
      const docUri = normalizeLsifDocUri(ref.location.uri);
      if (!docUri) continue;
      const startLine = Math.max(0, Math.trunc(ref.location.startLine));
      const endLine = Math.max(startLine, Math.trunc(ref.location.endLine));
      const startCharacter = Math.max(
        0,
        Math.trunc(ref.location.startCharacter),
      );
      const endCharacter = Math.max(0, Math.trunc(ref.location.endCharacter));
      const entry: LsifOccurrenceEntry = {
        symbolId: sym.id,
        startLine,
        startCharacter,
        endLine,
        endCharacter,
      };
      const lineSpan = endLine - startLine;
      if (lineSpan > 256) {
        appendOccurrence(docUri, startLine, entry);
      } else {
        for (let line = startLine; line <= endLine; line += 1) {
          appendOccurrence(docUri, line, entry);
        }
      }
    }
  }
  return {
    symbolsById,
    symbolIdsByKey,
    occurrenceIndexByDoc,
    symbolCount: parsed.symbolCount,
    docCount: parsed.docCount,
    revision: parsed.revision,
  };
}

async function parseLsifIndex(
  output: vscode.OutputChannel,
  payload: unknown,
): Promise<LsifIndexCache | undefined> {
  const parsed = await runLsifWorkerTask<ParsedLsifIndex>(
    output,
    "parseIndex",
    payload,
    () => parseLsifIndexPayload(payload),
  );
  return parsed ? toLsifIndexCache(parsed) : undefined;
}

async function parseLsifDelta(
  output: vscode.OutputChannel,
  payload: unknown,
): Promise<LsifDeltaPayload | undefined> {
  return runLsifWorkerTask<LsifDeltaPayload>(
    output,
    "parseDelta",
    payload,
    () => parseLsifDeltaPayload(payload),
  );
}

function applyLsifDelta(
  cache: LsifIndexCache,
  delta: LsifDeltaPayload,
): LsifIndexCache {
  // Delta payloads replace symbol records. Rebuilding derived indexes keeps the
  // lookup logic consistent with full-index parsing.
  const symbolsById = new Map(cache.symbolsById);
  for (const symbolId of delta.deletes) {
    symbolsById.delete(symbolId);
  }
  for (const entry of delta.upserts) {
    symbolsById.set(entry.id, entry);
  }
  const parsed: ParsedLsifIndex = {
    symbols: Array.from(symbolsById.values()),
    keyIndex: [],
    symbolCount: symbolsById.size,
    docCount: cache.docCount,
    revision: delta.revision,
  };
  const merged = toLsifIndexCache(parsed);
  if (merged.symbolCount !== symbolsById.size) {
    merged.symbolCount = symbolsById.size;
  }
  return merged;
}

function toVscodeLocation(loc: LsifLocationData): vscode.Location | undefined {
  try {
    const uri = normalizeNavUri(vscode.Uri.parse(loc.uri));
    const start = new vscode.Position(loc.startLine, loc.startCharacter);
    const end = new vscode.Position(loc.endLine, loc.endCharacter);
    return new vscode.Location(uri, new vscode.Range(start, end));
  } catch {
    return undefined;
  }
}

function toVscodeLocations(
  locations: readonly LsifLocationData[],
): vscode.Location[] {
  const out: vscode.Location[] = [];
  for (const loc of locations) {
    const parsed = toVscodeLocation(loc);
    if (parsed) out.push(parsed);
  }
  return out;
}

function normalizeSymbolKeyAtPosition(
  document: vscode.TextDocument,
  position: vscode.Position,
): string | undefined {
  // JOVIAL symbol lookup is case-insensitive, and identifiers may contain the
  // apostrophe form used by some dialects.
  const identRange =
    document.getWordRangeAtPosition(position, /[A-Za-z_$'][A-Za-z0-9_$']*/) ??
    document.getWordRangeAtPosition(position);
  if (!identRange) return undefined;
  const raw = document.getText(identRange).trim();
  if (!raw) return undefined;
  return raw.toUpperCase();
}

function isLsifRangeContainingPosition(
  entry: LsifOccurrenceEntry,
  pos: vscode.Position,
): boolean {
  if (pos.line < entry.startLine || pos.line > entry.endLine) return false;
  if (entry.startLine === entry.endLine) {
    return (
      pos.character >= entry.startCharacter &&
      pos.character <= entry.endCharacter
    );
  }
  if (pos.line === entry.startLine)
    return pos.character >= entry.startCharacter;
  if (pos.line === entry.endLine) return pos.character <= entry.endCharacter;
  return true;
}

function lsifOccurrenceSpan(entry: LsifOccurrenceEntry): number {
  const lineDelta = entry.endLine - entry.startLine;
  const charDelta = entry.endCharacter - entry.startCharacter;
  return lineDelta * 100000 + charDelta;
}

function lsifCacheForDocument(
  document: vscode.TextDocument,
): LsifIndexCache | undefined {
  if (!getConfig().lsifFastPath) return undefined;
  if (document.languageId !== "jovial") return undefined;
  const ctx = resolveLsifRootContext(document.uri, false);
  if (!ctx) return undefined;
  return lsifRootStates.get(ctx.rootKey)?.cache;
}

function lsifSymbolIdAtPosition(
  cache: LsifIndexCache,
  docUri: string,
  position: vscode.Position,
): string | undefined {
  // Prefer the narrowest occurrence span under the cursor so overlapping ranges
  // resolve to the most specific symbol.
  const byLine = cache.occurrenceIndexByDoc.get(docUri);
  if (!byLine) return undefined;
  const entries = byLine.get(position.line);
  if (!entries || entries.length === 0) return undefined;
  let best: LsifOccurrenceEntry | undefined;
  for (const entry of entries) {
    if (!isLsifRangeContainingPosition(entry, position)) continue;
    if (!best) {
      best = entry;
      continue;
    }
    const span = lsifOccurrenceSpan(entry);
    const cur = lsifOccurrenceSpan(best);
    if (
      span < cur ||
      (span === cur && entry.symbolId.localeCompare(best.symbolId) < 0)
    ) {
      best = entry;
    }
  }
  return best?.symbolId;
}

type LsifResolution = {
  symbols: LsifSymbolEntry[];
  key: string | undefined;
  fromOccurrence: boolean;
};

function resolveLsifSymbolsAt(
  document: vscode.TextDocument,
  position: vscode.Position,
): LsifResolution | undefined {
  // Exact occurrence hits are authoritative. Text-key lookup is a fallback for
  // stale or sparse indexes where the cursor is not in the occurrence map.
  const cache = lsifCacheForDocument(document);
  if (!cache) return undefined;
  const docUri = normalizeNavUri(document.uri).toString();
  const occSymbolId = lsifSymbolIdAtPosition(cache, docUri, position);
  if (occSymbolId) {
    const hit = cache.symbolsById.get(occSymbolId);
    if (hit) {
      return { symbols: [hit], key: hit.key, fromOccurrence: true };
    }
  }
  const key = normalizeSymbolKeyAtPosition(document, position);
  if (!key) return undefined;
  const ids = cache.symbolIdsByKey.get(key);
  if (!ids || ids.length === 0) return undefined;
  const symbols: LsifSymbolEntry[] = [];
  for (const id of ids) {
    const sym = cache.symbolsById.get(id);
    if (sym) symbols.push(sym);
  }
  if (symbols.length === 0) return undefined;
  symbols.sort((a, b) => a.id.localeCompare(b.id));
  return { symbols, key, fromOccurrence: false };
}

function dedupeLsifLocations(
  locations: readonly LsifLocationData[],
): LsifLocationData[] {
  const seen = new Set<string>();
  const out: LsifLocationData[] = [];
  for (const loc of locations) {
    const k = `${loc.uri}|${loc.startLine}|${loc.startCharacter}|${loc.endLine}|${loc.endCharacter}`;
    if (seen.has(k)) continue;
    seen.add(k);
    out.push(loc);
    if (out.length >= LSIF_MAX_FAST_RESULTS) break;
  }
  return out;
}

function collectLsifLocations(
  symbols: readonly LsifSymbolEntry[],
  select: (sym: LsifSymbolEntry) => readonly LsifLocationData[],
): LsifLocationData[] {
  // Cap merged results before and after dedupe so large reference sets stay
  // cheap enough for VS Code's provider path.
  const merged: LsifLocationData[] = [];
  for (const sym of symbols) {
    const next = select(sym);
    for (const loc of next) {
      merged.push(loc);
      if (merged.length >= LSIF_MAX_FAST_RESULTS * 2) break;
    }
    if (merged.length >= LSIF_MAX_FAST_RESULTS * 2) break;
  }
  return dedupeLsifLocations(merged).slice(0, LSIF_MAX_FAST_RESULTS);
}

function isLsifExternalRefImport(sym: LsifSymbolEntry): boolean {
  return (
    sym.externalKind === "external REF import" ||
    sym.declarationRole === "external REF import"
  );
}

function lsifCompanionSymbolsForKey(
  document: vscode.TextDocument,
  key: string | undefined,
): LsifSymbolEntry[] {
  if (!key) return [];
  const cache = lsifCacheForDocument(document);
  if (!cache) return [];
  const ids = cache.symbolIdsByKey.get(key);
  if (!ids || ids.length === 0) return [];
  const out: LsifSymbolEntry[] = [];
  for (const id of ids) {
    const sym = cache.symbolsById.get(id);
    if (sym) out.push(sym);
  }
  return out;
}

function lsifDefinitionFastPath(
  document: vscode.TextDocument,
  position: vscode.Position,
): vscode.Location[] | undefined {
  const resolved = resolveLsifSymbolsAt(document, position);
  if (!resolved) return undefined;
  if (!resolved.fromOccurrence && resolved.symbols.length !== 1) {
    return undefined;
  }
  const companionSymbols = lsifCompanionSymbolsForKey(document, resolved.key);
  const defs = collectLsifLocations(resolved.symbols, (sym) => {
    if (!isLsifExternalRefImport(sym)) return sym.definitions;
    if (sym.implementations.length > 0) return sym.implementations;
    return collectLsifLocations(companionSymbols, (candidate) =>
      isLsifExternalRefImport(candidate)
        ? []
        : candidate.implementations.length > 0
          ? candidate.implementations
          : candidate.definitions,
    );
  });
  if (defs.length === 0) return undefined;
  const locations = toVscodeLocations(defs);
  return locations.length > 0 ? locations : undefined;
}

function lsifDeclarationFastPath(
  document: vscode.TextDocument,
  position: vscode.Position,
): vscode.Location[] | undefined {
  return lsifDefinitionFastPath(document, position);
}

function lsifImplementationFastPath(
  document: vscode.TextDocument,
  position: vscode.Position,
): vscode.Location[] | undefined {
  const resolved = resolveLsifSymbolsAt(document, position);
  if (!resolved) return undefined;
  if (!resolved.fromOccurrence && resolved.symbols.length !== 1) {
    return undefined;
  }
  const impls = collectLsifLocations(resolved.symbols, (sym) =>
    sym.implementations.length > 0
      ? sym.implementations
      : isLsifExternalRefImport(sym)
        ? []
        : sym.definitions,
  );
  if (impls.length === 0) return undefined;
  const locations = toVscodeLocations(impls);
  return locations.length > 0 ? locations : undefined;
}

function isLsifTypeLikeKind(kind: number): boolean {
  return kind === 5;
}

function lsifTypeDefinitionFastPath(
  document: vscode.TextDocument,
  position: vscode.Position,
): vscode.Location[] | undefined {
  const resolved = resolveLsifSymbolsAt(document, position);
  if (!resolved) return undefined;
  const typeSymbols = resolved.symbols.filter((sym) =>
    isLsifTypeLikeKind(sym.kind),
  );
  if (typeSymbols.length === 0) return undefined;
  const defs = collectLsifLocations(typeSymbols, (sym) => sym.definitions);
  if (defs.length === 0) return undefined;
  const locations = toVscodeLocations(defs);
  return locations.length > 0 ? locations : undefined;
}

function lsifReferencesFastPath(
  document: vscode.TextDocument,
  position: vscode.Position,
  includeDeclaration: boolean,
): vscode.Location[] | undefined {
  const resolved = resolveLsifSymbolsAt(document, position);
  if (!resolved) return undefined;
  const locations: LsifLocationData[] = [];
  for (const sym of resolved.symbols) {
    for (const ref of sym.references) {
      if (!includeDeclaration && ref.declaration) continue;
      locations.push(ref.location);
    }
  }
  if (locations.length === 0) return undefined;
  const parsed = toVscodeLocations(dedupeLsifLocations(locations));
  return parsed.length > 0 ? parsed : undefined;
}

function formatLsifLocationForHover(loc: LsifLocationData): string {
  try {
    const uri = vscode.Uri.parse(loc.uri);
    const pathPart =
      uri.scheme === "file" ? uri.fsPath.replace(/\\/g, "/") : uri.toString();
    return `${pathPart}:${loc.startLine + 1}:${loc.startCharacter + 1}`;
  } catch {
    return `${loc.uri}:${loc.startLine + 1}:${loc.startCharacter + 1}`;
  }
}

function lsifSymbolKindLabel(kind: number): string {
  switch (kind) {
    case vscode.SymbolKind.Module:
      return "module";
    case vscode.SymbolKind.Namespace:
      return "compool";
    case vscode.SymbolKind.Function:
    case vscode.SymbolKind.Method:
      return "procedure";
    case vscode.SymbolKind.Class:
    case vscode.SymbolKind.TypeParameter:
      return "type";
    case vscode.SymbolKind.Field:
    case vscode.SymbolKind.Property:
      return "field";
    case vscode.SymbolKind.Constant:
      return "constant";
    case vscode.SymbolKind.Array:
      return "table";
    case vscode.SymbolKind.Variable:
      return "item";
    default:
      return "symbol";
  }
}

function lsifHoverFastPath(
  document: vscode.TextDocument,
  position: vscode.Position,
): vscode.Hover | undefined {
  // The fast hover is deliberately metadata-heavy: it explains what the cache
  // knows while the full server hover may still be catching up.
  const resolved = resolveLsifSymbolsAt(document, position);
  if (!resolved || resolved.symbols.length === 0) return undefined;
  if (!resolved.fromOccurrence && resolved.symbols.length !== 1) {
    return undefined;
  }
  const sym = resolved.symbols[0];
  const defs =
    sym.definitions.length > 0 ? sym.definitions : sym.implementations;
  const primary = defs[0];
  const kindLabel = lsifSymbolKindLabel(sym.kind);
  const classification = sym.classification ?? kindLabel;
  const declarationRole = sym.declarationRole ?? "pending semantic index";
  const definitionCount = sym.definitions.length;
  const implementationCount = sym.implementations.length;
  const referenceCount = sym.references.length;
  const importReferenceCount = sym.importReferences.length;
  const nonDeclRefs = sym.references.filter((ref) => !ref.declaration);
  const firstUse = nonDeclRefs[0]?.location;
  const md = new vscode.MarkdownString();
  md.appendMarkdown(`### \`${sym.key}\`\n\n`);
  md.appendMarkdown("_JOVIAL LSIF fast-path result_\n\n");
  md.appendMarkdown("| Field | Value |\n|---|---|\n");
  md.appendMarkdown(`| Classification | ${classification} |\n`);
  md.appendMarkdown(`| Declaration role | ${declarationRole} |\n`);
  if (sym.externalKind) {
    md.appendMarkdown(`| External kind | ${sym.externalKind} |\n`);
  }
  if (sym.typeDisplay) {
    md.appendMarkdown(`| Type | \`${sym.typeDisplay}\` |\n`);
  }
  if (sym.resolvedTypeDisplay) {
    md.appendMarkdown(`| Resolved type | \`${sym.resolvedTypeDisplay}\` |\n`);
  }
  md.appendMarkdown(
    primary
      ? `| Definition/implementation target | \`${formatLsifLocationForHover(primary)}\` |\n`
      : "| Definition/implementation target | not indexed |\n",
  );
  md.appendMarkdown(`| Definitions | ${definitionCount} |\n`);
  md.appendMarkdown(`| Implementations | ${implementationCount} |\n`);
  md.appendMarkdown(`| References | ${referenceCount} |\n`);
  md.appendMarkdown(`| Import/reference count | ${importReferenceCount} |\n`);
  md.appendMarkdown(`| Non-declaration uses | ${nonDeclRefs.length} |\n`);
  md.appendMarkdown(
    firstUse
      ? `| First use | \`${formatLsifLocationForHover(firstUse)}\` |\n`
      : "| First use | not indexed |\n",
  );
  md.appendMarkdown("\n**Symbol key:**\n");
  md.appendCodeblock(sym.key, "jovial");
  // md.appendMarkdown(
  //   "\n**Change impact:** Use Find References before renaming or changing type/signature. Cross-file references may exist through COMPOOL imports or external DEF/REF declarations.\n\n",
  // );
  // md.appendMarkdown(
  //   "**Note:** This is LSIF fast-path hover. Server hover may add source declaration, scope, implementation preview, and deeper JOVIAL semantics once indexing catches up.",
  // );

  const range =
    document.getWordRangeAtPosition(position, /[A-Za-z_$'][A-Za-z0-9_$']*/) ??
    document.getWordRangeAtPosition(position);
  return new vscode.Hover(md, range);
}

function hasProviderResult(value: unknown): boolean {
  if (value === null || value === undefined) return false;
  if (Array.isArray(value)) return value.length > 0;
  return true;
}

function normalizeNavUri(uri: vscode.Uri): vscode.Uri {
  if (uri.scheme !== "file") return uri;
  try {
    return vscode.Uri.file(uri.fsPath);
  } catch {
    return uri;
  }
}

function toVscodeUri(value: unknown): vscode.Uri | undefined {
  if (value instanceof vscode.Uri) return value;
  if (typeof value === "string") {
    try {
      return vscode.Uri.parse(value);
    } catch {
      return undefined;
    }
  }
  return undefined;
}

function toVscodeRange(value: unknown): vscode.Range | undefined {
  if (value instanceof vscode.Range) return value;
  if (!value || typeof value !== "object") return undefined;
  const rec = value as Record<string, unknown>;
  const start = rec["start"];
  const end = rec["end"];
  if (!start || typeof start !== "object" || !end || typeof end !== "object")
    return undefined;
  const startRec = start as Record<string, unknown>;
  const endRec = end as Record<string, unknown>;
  const sl = startRec["line"];
  const sc = startRec["character"];
  const el = endRec["line"];
  const ec = endRec["character"];
  if (
    typeof sl !== "number" ||
    typeof sc !== "number" ||
    typeof el !== "number" ||
    typeof ec !== "number"
  ) {
    return undefined;
  }
  return new vscode.Range(
    new vscode.Position(
      Math.max(0, Math.trunc(sl)),
      Math.max(0, Math.trunc(sc)),
    ),
    new vscode.Position(
      Math.max(0, Math.trunc(el)),
      Math.max(0, Math.trunc(ec)),
    ),
  );
}

function isLocationLinkLike(value: unknown): value is vscode.LocationLink {
  if (!value || typeof value !== "object") return false;
  const rec = value as Record<string, unknown>;
  return (
    toVscodeUri(rec["targetUri"]) !== undefined &&
    toVscodeRange(rec["targetRange"]) !== undefined &&
    toVscodeRange(rec["targetSelectionRange"]) !== undefined
  );
}

function normalizeNavResult<T>(value: T): T {
  // Normalize provider results that cross the server/fallback boundary so URI
  // casing and parsed Range objects behave consistently in VS Code.
  const normalizeOne = (item: unknown): unknown => {
    if (item && typeof item === "object") {
      const rec = item as Record<string, unknown>;
      const locUri = toVscodeUri(rec["uri"]);
      const locRange = toVscodeRange(rec["range"]);
      if (locUri && locRange) {
        return new vscode.Location(normalizeNavUri(locUri), locRange);
      }
    }
    if (isLocationLinkLike(item)) {
      const rec = item as unknown as Record<string, unknown>;
      const targetUri = toVscodeUri(rec["targetUri"]);
      const targetRange = toVscodeRange(rec["targetRange"]);
      const targetSelectionRange = toVscodeRange(rec["targetSelectionRange"]);
      if (!targetUri || !targetRange || !targetSelectionRange) return item;
      return {
        ...(item as unknown as Record<string, unknown>),
        targetUri: normalizeNavUri(targetUri),
        targetRange,
        targetSelectionRange,
      };
    }
    return item;
  };

  if (Array.isArray(value)) {
    return value.map((item) => normalizeOne(item)) as T;
  }
  return normalizeOne(value) as T;
}

async function raceLsifProviderFallback<T>(
  output: vscode.OutputChannel,
  providerName: string,
  token: vscode.CancellationToken,
  server: () => T | PromiseLike<T>,
  fallback: () => T | undefined,
  normalizeResult?: (value: T) => T,
  budgetMs = LSIF_FALLBACK_RACE_BUDGET_MS,
  lateBudgetMs = PROVIDER_LATE_BUDGET_MS,
): Promise<T | undefined> {
  if (!getConfig().lsifFastPath) {
    try {
      const value = await Promise.resolve(server());
      return normalizeResult ? normalizeResult(value) : value;
    } catch (e) {
      output.appendLine(`Server ${providerName} failed: ${String(e)}`);
      return undefined;
    }
  }

  // Give the authoritative server a short head start, then use the LSIF cache as
  // a responsive fallback while still accepting late server results.
  return raceServerWithFallback({
    budgetMs,
    lateBudgetMs,
    token,
    server,
    fallback,
    hasResult: hasProviderResult,
    normalizeServerResult: normalizeResult,
    normalizeFallbackResult: normalizeResult,
    preferLateServerResult: true,
    fallbackServerBudgetMs: LSIF_FALLBACK_SERVER_GRACE_MS,
    onServerError: (e) => {
      output.appendLine(
        `Server ${providerName} failed; trying LSIF fallback: ${String(e)}`,
      );
    },
  });
}

function scheduleLsifRefreshForContext(
  output: vscode.OutputChannel,
  reason: string,
  context: LsifRootContext,
  delayMs: number,
): void {
  const state = lsifRootStateFor(context.rootKey);
  state.refreshPending = true;
  state.pendingReason = reason;
  state.lastContextUri = context.contextUri;
  clearLsifRootTimer(state);
  state.refreshTimer = setTimeout(
    () => {
      state.refreshTimer = undefined;
      void refreshLsifIndex(output, reason, context.contextUri);
    },
    Math.max(0, Math.trunc(delayMs)),
  );
}

async function refreshLsifIndex(
  output: vscode.OutputChannel,
  reason: string,
  preferredUri?: vscode.Uri,
): Promise<void> {
  // Prefer incremental LSIF deltas when the server can produce them; fall back
  // to a full cache snapshot whenever the revision chain is broken.
  if (!client || !getConfig().lsifFastPath) return;
  const context = resolveLsifRootContext(preferredUri, true);
  if (!context) return;
  const state = lsifRootStateFor(context.rootKey);
  state.lastContextUri = context.contextUri;

  if (state.refreshInFlight) {
    state.refreshPending = true;
    state.pendingReason = reason;
    return;
  }

  const now = Date.now();
  const elapsed = now - state.lastRefreshMs;
  if (elapsed < LSIF_REFRESH_MIN_INTERVAL_MS) {
    state.refreshPending = true;
    state.pendingReason = reason;
    scheduleLsifRefreshForContext(
      output,
      reason,
      context,
      LSIF_REFRESH_MIN_INTERVAL_MS - elapsed,
    );
    return;
  }

  clearLsifRootTimer(state);
  state.refreshInFlight = true;
  state.refreshPending = false;
  state.pendingReason = undefined;
  try {
    if (state.cache && state.cache.revision > 0) {
      try {
        const deltaRaw = await executeServerCommand("jovial.dumpLsifDelta", [
          context.contextUri.toString(),
          state.cache.revision,
        ]);
        const delta = await parseLsifDelta(output, deltaRaw);
        if (
          delta &&
          delta.baseRevision === state.cache.revision &&
          !delta.reset
        ) {
          state.cache = applyLsifDelta(state.cache, delta);
          state.lastRefreshMs = Date.now();
          output.appendLine(
            `LSIF cache delta refreshed (${reason}, ${context.rootKey}): rev ${delta.baseRevision} -> ${delta.revision}, ` +
              `${delta.upserts.length} upserts, ${delta.deletes.length} deletes.`,
          );
          return;
        }
        if (delta?.reset) {
          output.appendLine(
            `LSIF delta requested full refresh (${reason}, ${context.rootKey}) at base rev ${delta.baseRevision}.`,
          );
        }
      } catch (deltaErr) {
        output.appendLine(
          `LSIF delta refresh fallback (${reason}, ${context.rootKey}): ${String(deltaErr)}`,
        );
      }
    }

    const result = await executeServerCommand("jovial.dumpLsifIndex", [
      context.contextUri.toString(),
    ]);
    const parsed = await parseLsifIndex(output, result);
    if (!parsed) {
      output.appendLine("LSIF cache refresh skipped: invalid server payload.");
      return;
    }
    state.cache = parsed;
    state.lastRefreshMs = Date.now();
    output.appendLine(
      `LSIF cache refreshed (${reason}, ${context.rootKey}): rev ${parsed.revision}, ` +
        `${parsed.symbolCount} symbols across ${parsed.docCount} docs.`,
    );
  } catch (e) {
    output.appendLine(
      `LSIF cache refresh failed (${reason}, ${context.rootKey}): ${String(e)}`,
    );
  } finally {
    state.refreshInFlight = false;
    if (state.refreshPending && client && getConfig().lsifFastPath) {
      const pendingReason = state.pendingReason ?? `${reason}-pending`;
      state.refreshPending = false;
      state.pendingReason = undefined;
      const followUri = state.lastContextUri ?? context.contextUri;
      const followContext = resolveLsifRootContext(followUri, true);
      if (followContext) {
        scheduleLsifRefreshForContext(
          output,
          pendingReason,
          followContext,
          LSIF_REFRESH_DEBOUNCE_MS,
        );
      }
    }
  }
}

function scheduleLsifRefresh(
  output: vscode.OutputChannel,
  reason: string,
  preferredUri?: vscode.Uri,
  delayMs = LSIF_REFRESH_DEBOUNCE_MS,
): void {
  if (!client || !getConfig().lsifFastPath) return;
  const context = resolveLsifRootContext(preferredUri, true);
  if (!context) return;
  scheduleLsifRefreshForContext(output, reason, context, delayMs);
}

function clearAutoRestartTimer(): void {
  if (!pendingAutoRestartTimer) return;
  clearTimeout(pendingAutoRestartTimer);
  pendingAutoRestartTimer = undefined;
}

function shouldAttemptAutoRestart(): boolean {
  const now = Date.now();
  autoRestartAttempts = autoRestartAttempts.filter(
    (t) => now - t <= AUTO_RESTART_WINDOW_MS,
  );
  if (autoRestartAttempts.length >= AUTO_RESTART_MAX_ATTEMPTS) {
    return false;
  }
  autoRestartAttempts.push(now);
  return true;
}

function toClientTraceLevel(level: "off" | "messages" | "verbose"): Trace {
  switch (level) {
    case "verbose":
      return Trace.Verbose;
    case "messages":
      return Trace.Messages;
    default:
      return Trace.Off;
  }
}

function applyTraceSetting(output: vscode.OutputChannel): void {
  if (!client) return;
  const trace = getConfig().trace;
  client.setTrace(toClientTraceLevel(trace));
  output.appendLine(`LSP trace level: ${trace}`);
}

function isProcessRunning(child: cp.ChildProcess): boolean {
  return child.exitCode === null && child.signalCode === null;
}

function waitForProcessExit(
  child: cp.ChildProcess,
  timeoutMs: number,
): Promise<boolean> {
  if (!isProcessRunning(child)) return Promise.resolve(true);
  return new Promise((resolve) => {
    let settled = false;
    const done = (exited: boolean): void => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      child.off("exit", onExit);
      child.off("close", onExit);
      resolve(exited);
    };
    const onExit = (): void => done(true);
    const timer = setTimeout(() => done(false), timeoutMs);
    child.once("exit", onExit);
    child.once("close", onExit);
  });
}

async function stopLanguageClient(
  languageClient: LanguageClient,
  output?: vscode.OutputChannel,
): Promise<boolean> {
  let timer: NodeJS.Timeout | undefined;
  const stopPromise = languageClient.stop().then(
    () => true,
    (e) => {
      output?.appendLine(`Language client stop failed: ${String(e)}`);
      return false;
    },
  );
  const timeoutPromise = new Promise<boolean>((resolve) => {
    timer = setTimeout(() => resolve(false), SERVER_STOP_TIMEOUT_MS);
  });
  const stopped = await Promise.race([stopPromise, timeoutPromise]);
  if (timer) clearTimeout(timer);
  if (!stopped) {
    output?.appendLine(
      `Language client stop did not finish within ${SERVER_STOP_TIMEOUT_MS}ms; terminating server process.`,
    );
  }
  return stopped;
}

async function terminateServerProcess(
  output: vscode.OutputChannel | undefined,
  reason: string,
  child: cp.ChildProcess | undefined = serverProcess,
): Promise<void> {
  if (!child) return;
  if (serverProcess === child) serverProcess = undefined;
  if (!isProcessRunning(child)) return;

  output?.appendLine(`Terminating Jovial LSP process (${reason}).`);
  try {
    child.kill("SIGTERM");
  } catch (e) {
    output?.appendLine(`Failed to signal Jovial LSP process: ${String(e)}`);
  }
  if (await waitForProcessExit(child, SERVER_KILL_TIMEOUT_MS)) return;

  output?.appendLine("Jovial LSP process did not exit after SIGTERM; killing.");
  try {
    child.kill("SIGKILL");
  } catch (e) {
    output?.appendLine(`Failed to kill Jovial LSP process: ${String(e)}`);
  }
  await waitForProcessExit(child, SERVER_KILL_TIMEOUT_MS);
}

async function stopClient(
  status: vscode.StatusBarItem,
  output?: vscode.OutputChannel,
) {
  // Stop owns every background timer and queue so an explicit restart starts
  // from a clean extension-host state.
  serverStopRequested = true;
  clearAutoRestartTimer();
  clearDiagnosticRefreshTimer();
  diagnosticRefreshPending = false;
  diagnosticSourceRefreshCursor = 0;
  pendingDiagnosticRefreshUris.clear();
  resetLsifState();
  clearStartupStatus();

  if (fileWatcher) {
    fileWatcher.dispose();
    fileWatcher = undefined;
  }
  resetWatchedFileStreamingState();
  currentSourceFileSets = [];

  if (!client) {
    await terminateServerProcess(output, "stop without active client");
    setStatus(status, "stopped");
    return;
  }
  const c = client;
  const child = serverProcess;
  client = undefined;
  try {
    await stopLanguageClient(c, output);
  } finally {
    await terminateServerProcess(output, "client stop cleanup", child);
    clearStartupStatus();
    setStatus(status, "stopped");
  }
}

function scheduleAutoRestart(
  context: vscode.ExtensionContext,
  output: vscode.OutputChannel,
  status: vscode.StatusBarItem,
  reason: string,
): void {
  if (serverStopRequested) return;
  if (!getConfig().autostart) return;
  if (pendingAutoRestartTimer) return;
  if (!shouldAttemptAutoRestart()) {
    const msg = `Auto-restart limit reached (${AUTO_RESTART_MAX_ATTEMPTS} failures in ${Math.floor(
      AUTO_RESTART_WINDOW_MS / 1000,
    )}s).`;
    output.appendLine(msg);
    setStatus(status, "error", `${msg} Server connection could not be restored.`);
    return;
  }

  output.appendLine(
    `Scheduling Jovial LSP restart (${reason}) in ${AUTO_RESTART_DELAY_MS}ms.`,
  );
  setStatus(status, "starting", `Restarting after ${reason}`);
  pendingAutoRestartTimer = setTimeout(() => {
    pendingAutoRestartTimer = undefined;
    void startClient(context, output, status, "auto-restart");
  }, AUTO_RESTART_DELAY_MS);
}

async function startClient(
  context: vscode.ExtensionContext,
  output: vscode.OutputChannel,
  status: vscode.StatusBarItem,
  source: "manual" | "auto-restart" | "config-change" = "manual",
) {
  // Startup resolves the server binary, discovers initial sources, wires file
  // watching, then creates the LanguageClient over the server's stdio streams.
  if (startInProgress) {
    output.appendLine(
      `Start request (${source}) ignored: startup already in progress.`,
    );
    return;
  }

  startInProgress = true;
  clearAutoRestartTimer();
  serverStopRequested = false;
  clearStartupStatus();

  try {
    const cfg = getConfig();
    const resolvedServer = resolveServerPath(
      context,
      cfg.serverPath,
      cfg.preferBundled,
      context.extensionMode === vscode.ExtensionMode.Development,
    );
    const serverPath = resolvedServer.path;

    output.appendLine(
      `Resolved server path (${resolvedServer.source}): ${serverPath ?? "<none>"}`,
    );
    if (resolvedServer.note) {
      output.appendLine(resolvedServer.note);
    }

    if (!serverPath || !fs.existsSync(serverPath)) {
      const msg =
        "Server executable not found. Bundle runtime binaries under runtime/server/<platform-arch>/ or set jovial.server.path.";
      setStatus(status, "stopped", msg);
      if (source !== "auto-restart") {
        vscode.window.showErrorMessage(
          "Jovial LSP: server executable not found. Bundle runtime binaries or set jovial.server.path in Settings (can be relative to workspace).",
        );
      }
      return;
    }

    ensureServerExecutable(serverPath, output);
    setStatus(status, "starting", `Starting: ${serverPath}`);

    if (client) {
      await stopClient(status, output);
      serverStopRequested = false;
    } else if (serverProcess) {
      await terminateServerProcess(output, "pre-start cleanup");
    }

    output.appendLine(`Starting Jovial LSP (${source}): ${serverPath}`);

    if (fileWatcher) {
      fileWatcher.dispose();
      fileWatcher = undefined;
    }
    resetWatchedFileStreamingState();
    currentSourceFileSets = [];
    const watchedExtensions = watchedWorkspaceExtensions(cfg);
    fileWatcher = vscode.workspace.createFileSystemWatcher(
      watcherGlobForSourceExtensions(watchedExtensions),
    );
    context.subscriptions.push(fileWatcher);
    fileWatcher.onDidCreate((uri) => {
      enqueueWatchedFileChange(
        pendingWatchedFileChanges,
        { fsPath: uri.fsPath, type: WATCH_CHANGE_CREATED },
        {
          isOpenFilePath: isOpenFileDocumentPath,
          sourceExtensions: watchedExtensions,
        },
      );
      scheduleWatchedFileFlush(output);
    });
    fileWatcher.onDidChange((uri) => {
      enqueueWatchedFileChange(
        pendingWatchedFileChanges,
        { fsPath: uri.fsPath, type: WATCH_CHANGE_CHANGED },
        {
          isOpenFilePath: isOpenFileDocumentPath,
          sourceExtensions: watchedExtensions,
        },
      );
      scheduleWatchedFileFlush(output);
    });
    fileWatcher.onDidDelete((uri) => {
      enqueueWatchedFileChange(
        pendingWatchedFileChanges,
        { fsPath: uri.fsPath, type: WATCH_CHANGE_DELETED },
        {
          isOpenFilePath: isOpenFileDocumentPath,
          sourceExtensions: watchedExtensions,
        },
      );
      scheduleWatchedFileFlush(output);
    });

    const initialSourceFileSets = await discoverSourceFileSets(
      cfg,
      output,
    ).catch((e) => {
      output.appendLine(`Jovial source discovery failed: ${String(e)}`);
      return [] as JovialSourceFileSet[];
    });

    currentSourceFileSets = initialSourceFileSets;
    diagnosticSourceRefreshCursor = 0;

    const initialFileCount = initialSourceFileSets.reduce(
      (acc, set) =>
        acc + set.fileUris.length + (set.assemblyFileUris?.length ?? 0),
      0,
    );

    const initialDiscoveryTruncated = initialSourceFileSets.some(
      (set) => set.searchTruncated,
    );

    output.appendLine(
      `Discovered Jovial source file sets before initialize: ${initialFileCount} files across ${initialSourceFileSets.length} root(s)${
        initialDiscoveryTruncated ? " (truncated)" : ""
      }.`,
    );

    setStatus(
      status,
      "starting",
      `Discovered ${initialFileCount} source files${
        initialDiscoveryTruncated ? " (truncated)" : ""
      }; starting server`,
    );

    const serverOptions = async (): Promise<StreamInfo> => {
      const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;

      // On Windows, overlapped stdio avoids pipe deadlocks with Node's child
      // process handling; POSIX platforms use ordinary pipes.
      const stdioMode =
        process.platform === "win32"
          ? (["overlapped", "overlapped", "overlapped"] as cp.StdioOptions)
          : (["pipe", "pipe", "pipe"] as cp.StdioOptions);

      const child = cp.spawn(serverPath, cfg.serverArgs, {
        cwd: workspaceRoot,
        env: {
          ...process.env,
        },
        windowsHide: true,
        stdio: stdioMode,
      });
      const generation = ++serverProcessGeneration;
      serverProcess = child;

      if (child.stderr) {
        child.stderr.setEncoding("utf8");
        child.stderr.on("data", (chunk: string) =>
          output.appendLine(chunk.toString()),
        );
      }

      child.on("exit", (code, signal) => {
        output.appendLine(`Jovial LSP exited: code=${code} signal=${signal}`);
        if (serverProcess === child) serverProcess = undefined;
        if (serverProcessGeneration === generation && !serverStopRequested) {
          setStatus(
            status,
            "error",
            `Server connection closed unexpectedly: code=${code} signal=${signal}`,
          );
        }
      });

      child.on("error", (err) => {
        output.appendLine(`Failed to start Jovial LSP: ${String(err)}`);
        if (serverProcess === child) serverProcess = undefined;
        setStatus(status, "stopped", `Failed to start: ${String(err)}`);
      });

      const reader = child.stdout;
      const writer = child.stdin;
      if (!reader || !writer) {
        throw new Error("Failed to initialize LSP stdio streams.");
      }
      return { reader, writer };
    };

    const clientOptions: LanguageClientOptions = {
      documentSelector: [
        { scheme: "file", language: "jovial" },
        { scheme: "untitled", language: "jovial" },
      ],
      initializationOptions: buildInitializationOptions(
        cfg,
        initialSourceFileSets,
      ),
      outputChannel: output,
      middleware: {
        provideDocumentSymbols: (document, token, next) => {
          return next(document, token);
        },
        // Navigation requests race the canonical server answer against the
        // local LSIF cache so large workspaces still feel immediate.
        provideDefinition: async (document, position, token, next) => {
          return raceLsifProviderFallback(
            output,
            "definition",
            token,
            () => next(document, position, token),
            () =>
              lsifDefinitionFastPath(document, position) as
                | Awaited<ReturnType<typeof next>>
                | undefined,
            normalizeNavResult,
          );
        },
        provideDeclaration: async (document, position, token, next) => {
          return raceLsifProviderFallback(
            output,
            "declaration",
            token,
            () => next(document, position, token),
            () =>
              lsifDeclarationFastPath(document, position) as
                | Awaited<ReturnType<typeof next>>
                | undefined,
            normalizeNavResult,
          );
        },
        provideTypeDefinition: async (document, position, token, next) => {
          return raceLsifProviderFallback(
            output,
            "typeDefinition",
            token,
            () => next(document, position, token),
            () =>
              lsifTypeDefinitionFastPath(document, position) as
                | Awaited<ReturnType<typeof next>>
                | undefined,
            normalizeNavResult,
          );
        },
        provideImplementation: async (document, position, token, next) => {
          return raceLsifProviderFallback(
            output,
            "implementation",
            token,
            () => next(document, position, token),
            () =>
              lsifImplementationFastPath(document, position) as
                | Awaited<ReturnType<typeof next>>
                | undefined,
            normalizeNavResult,
          );
        },
        provideReferences: async (document, position, context, token, next) => {
          return raceLsifProviderFallback(
            output,
            "references",
            token,
            () => next(document, position, context, token),
            () =>
              lsifReferencesFastPath(
                document,
                position,
                context.includeDeclaration,
              ) as Awaited<ReturnType<typeof next>> | undefined,
            normalizeNavResult,
          );
        },
        provideHover: async (document, position, token, next) => {
          return raceLsifProviderFallback(
            output,
            "hover",
            token,
            () => next(document, position, token),
            () =>
              lsifHoverFastPath(document, position) as
                | Awaited<ReturnType<typeof next>>
                | undefined,
            undefined,
            LSIF_HOVER_FALLBACK_RACE_BUDGET_MS,
          );
        },
        provideCompletionItem: (document, position, context, token, next) => {
          return next(document, position, context, token);
        },
        provideInlayHints: async (document, range, token, next) => {
          const cfg = getConfig();
          if (!cfg.features.inlayHints) return [];
          try {
            return await next(document, range, token);
          } catch (e) {
            output.appendLine(`Server inlay hints failed: ${String(e)}`);
            return [];
          }
        },
        provideDocumentRangeSemanticTokens: (document, range, token, next) => {
          const cfg = getConfig();
          if (!cfg.features.semanticTokens) {
            return new vscode.SemanticTokens(new Uint32Array());
          }
          return next(document, range, token);
        },
      },
      errorHandler: {
        // Transport closures are restartable unless the user explicitly stopped
        // the server; protocol errors are logged and allowed to continue.
        error: (error, message, count) => {
          output.appendLine(
            `Client error (${count ?? 0}): ${message ?? ""} ${String(error)}`,
          );
          return { action: ErrorAction.Continue };
        },
        closed: () => {
          output.appendLine("Client closed.");
          setStatus(
            status,
            serverStopRequested ? "stopped" : "error",
            serverStopRequested
              ? "Client closed."
              : "Server connection closed unexpectedly.",
          );
          clearExecuteCommandRegistrations(client, output);
          client = undefined;
          void terminateServerProcess(output, "transport closure");
          clearDiagnosticRefreshTimer();
          diagnosticRefreshPending = false;
          diagnosticSourceRefreshCursor = 0;
          pendingDiagnosticRefreshUris.clear();
          if (!serverStopRequested) {
            scheduleAutoRestart(context, output, status, "transport closure");
          }
          return { action: CloseAction.DoNotRestart };
        },
      },
    };

    // Keep the client id stable; VS Code uses it for trace settings keys.
    client = new LanguageClient(
      "jovialLsp",
      "Jovial Language Server",
      serverOptions,
      clientOptions,
    );
    installSafeExecuteCommandRegistration(client, output);

    const startupDiagTargetMs = STARTUP_DIAG_TARGET_DEFAULT_MS;
    const startupNavTargetMs = STARTUP_NAV_TARGET_DEFAULT_MS;
    bindStartupNotifications({
      client,
      output,
      status,
      startupDiagTargetMs,
      startupNavTargetMs,
    });
    await client.start();
    applyTraceSetting(output);
    await flushWatchedFileChanges(output);
    scheduleLsifRefresh(output, "startup", firstPreferredLsifUri(), 200);
    scheduleDiagnosticRefresh(
      output,
      DIAGNOSTIC_REFRESH_STARTUP_DELAY_MS,
      true,
      "startup",
    );
    autoRestartAttempts = [];
    output.appendLine("Jovial LSP client started.");
    setStatus(status, "running", `ServerReady; background indexing active`);
  } catch (e) {
    output.appendLine(`Client failed to start: ${String(e)}`);
    setStatus(status, "error", `Client failed to start: ${String(e)}`);
    clearExecuteCommandRegistrations(client, output);
    client = undefined;
    await terminateServerProcess(output, "startup failure cleanup");
    if (!serverStopRequested) {
      scheduleAutoRestart(context, output, status, "startup failure");
    }
  } finally {
    startInProgress = false;
  }
}

async function executeServerCommand(
  command: string,
  args: unknown[],
): Promise<unknown> {
  // UI commands share this helper so command payload validation remains on the
  // server side and the extension only handles transport concerns.
  if (!client) {
    throw new Error("Jovial LSP client is not running.");
  }
  const params: ExecuteCommandParams = { command, arguments: args };
  return client.sendRequest(ExecuteCommandRequest.type, params);
}

async function explainSymbolResolutionUi(
  output: vscode.OutputChannel,
): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor || !isJovialDiagnosticDocument(editor.document)) {
    vscode.window.showInformationMessage(
      "Jovial: open a Jovial document and place the cursor on a symbol first.",
    );
    return;
  }
  const pos = editor.selection.active;
  const uri = normalizeNavUri(editor.document.uri).toString();
  const result = await executeServerCommand("jovial.explainSymbolResolution", [
    uri,
    pos.line,
    pos.character,
  ]);
  const text = JSON.stringify(result, null, 2);
  output.appendLine("Jovial symbol resolution explanation:");
  output.appendLine(text);
  output.show(true);
  const doc = await vscode.workspace.openTextDocument({
    content: text,
    language: "json",
  });
  await vscode.window.showTextDocument(doc, { preview: true });
}

export async function activate(context: vscode.ExtensionContext) {
  // VS Code calls activate once per extension host. Register all UI hooks and
  // lightweight editor listeners before optional autostart.
  const output = vscode.window.createOutputChannel("Jovial LSP");
  context.subscriptions.push(output);
  liveEditDiagnosticCollection =
    vscode.languages.createDiagnosticCollection("jovial-live");
  context.subscriptions.push(liveEditDiagnosticCollection);

  const status = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Left,
    100,
  );
  status.command = "jovial.restartServer";
  status.show();
  context.subscriptions.push(status);
  setStatus(status, "stopped", "Click to start / restart Jovial LSP");

  context.subscriptions.push(
    // Editor events keep diagnostics warm for the files the user is actively
    // touching, even when the periodic background refresh has not fired yet.
    vscode.workspace.onDidOpenTextDocument((doc) => {
      if (isJovialDiagnosticDocument(doc)) {
        updateHugeLiveEditDiagnostics(doc);
        scheduleDiagnosticRefresh(
          output,
          DIAGNOSTIC_REFRESH_EDITOR_DELAY_MS,
          true,
          "open-editor",
        );
      }
    }),
    vscode.workspace.onDidChangeTextDocument((event) => {
      if (
        event.contentChanges.length > 0 &&
        isJovialDiagnosticDocument(event.document)
      ) {
        updateHugeLiveEditDiagnostics(event.document);
        pendingDiagnosticRefreshUris.add(event.document.uri.toString());
        scheduleDiagnosticRefresh(
          output,
          DIAGNOSTIC_REFRESH_EDIT_DELAY_MS,
          true,
          "edit",
        );
      }
    }),
    vscode.workspace.onDidCloseTextDocument((doc) => {
      liveEditDiagnosticCollection?.delete(doc.uri);
    }),
    vscode.languages.onDidChangeDiagnostics((event) => {
      for (const uri of event.uris) {
        reconcileLiveEditDiagnostics(uri);
      }
    }),
    vscode.window.onDidChangeActiveTextEditor((editor) => {
      if (editor && isJovialDiagnosticDocument(editor.document)) {
        updateHugeLiveEditDiagnostics(editor.document);
        scheduleDiagnosticRefresh(
          output,
          DIAGNOSTIC_REFRESH_EDITOR_DELAY_MS,
          true,
          "active-editor",
        );
      }
    }),
  );

  // Extension-owned UI commands avoid the language client's dynamic command
  // registrations.
  registerExtensionHooks({
    context,
    output,
    status,
    getConfig,
    setStatus,
    startClient,
    stopClient,
    refreshLsifIndex,
    refreshDiagnosticsNow,
    pullDiagnosticsNow,
    firstPreferredLsifUri,
    scheduleLsifRefresh,
    refreshStartupStatusBar,
    resetLsifState,
    applyTraceSetting,
    dumpAstUi: () =>
      dumpAstUiView({
        executeServerCommand,
        output,
        isServerRunning: () => Boolean(client),
      }),
    dumpCstUi: () =>
      dumpCstUiView({
        executeServerCommand,
        output,
        isServerRunning: () => Boolean(client),
      }),
    showSyntaxTreesUi: () =>
      showSyntaxTreesUiView({
        executeServerCommand,
        output,
        isServerRunning: () => Boolean(client),
      }),
    explainSymbolResolution: () => explainSymbolResolutionUi(output),
  });

  if (getConfig().autostart) {
    await startClient(context, output, status);
  }
}

export async function deactivate() {
  // Mirror explicit stop cleanup so extension-host shutdown does not leave
  // timers, workers, or server processes alive.
  serverStopRequested = true;
  clearAutoRestartTimer();
  clearDiagnosticRefreshTimer();
  diagnosticRefreshPending = false;
  diagnosticSourceRefreshCursor = 0;
  pendingDiagnosticRefreshUris.clear();
  resetLsifState();
  if (fileWatcher) {
    fileWatcher.dispose();
    fileWatcher = undefined;
  }
  resetWatchedFileStreamingState();
  currentSourceFileSets = [];
  liveEditDiagnosticCollection?.dispose();
  liveEditDiagnosticCollection = undefined;
  const c = client;
  const child = serverProcess;
  client = undefined;
  if (c) await stopLanguageClient(c);
  await terminateServerProcess(undefined, "extension deactivation", child);
}
