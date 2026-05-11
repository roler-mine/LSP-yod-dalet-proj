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
import { watcherGlobForSourceExtensions } from "./source_extensions";
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

let client: LanguageClient | undefined;
let fileWatcher: vscode.FileSystemWatcher | undefined;
const WATCH_FLUSH_DELAY_MS = 150;
const WATCH_CHUNK_SIZE = 256;
const WATCH_FORCE_FLUSH_SIZE = 2000;
const AUTO_RESTART_DELAY_MS = 1200;
const AUTO_RESTART_WINDOW_MS = 120000;
const AUTO_RESTART_MAX_ATTEMPTS = 5;
const LSIF_REFRESH_DEBOUNCE_MS = 800;
const LSIF_REFRESH_MIN_INTERVAL_MS = 1200;
const LSIF_MAX_FAST_RESULTS = 300;
const LSIF_FALLBACK_RACE_BUDGET_MS = 75;
const LSIF_HOVER_FALLBACK_RACE_BUDGET_MS = 250;
const DIAGNOSTIC_REFRESH_INTERVAL_MS = 30000;
const DIAGNOSTIC_REFRESH_STARTUP_DELAY_MS = 5000;
const DIAGNOSTIC_REFRESH_EDITOR_DELAY_MS = 1000;
const DIAGNOSTIC_REFRESH_SOURCE_CHUNK_SIZE = 64;
const DIAGNOSTIC_REFRESH_PENDING_CHUNK_SIZE = 128;

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
const diagnosticPullResultIds = new Map<string, string>();
let diagnosticSourceRefreshCursor = 0;
const pendingDiagnosticRefreshUris = new Set<string>();

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

const lsifRootStates = new Map<string, LsifRootState>();
let lsifWorker: Worker | undefined;
let lsifWorkerRequestSeq = 0;
let lsifWorkerShutdownRequested = false;
let lsifWorkerFallbackLogged = false;
const lsifWorkerPending = new Map<number, LsifWorkerPendingRequest>();

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

function diagnosticReportResultId(report: unknown): string | undefined {
  const rec = asRecord(report);
  const resultId = rec?.resultId;
  return typeof resultId === "string" ? resultId : undefined;
}

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

async function refreshOpenDocumentDiagnostics(
  output: vscode.OutputChannel,
  reason: string,
): Promise<void> {
  const c = client;
  if (!c || diagnosticRefreshInFlight) return;
  const docs = vscode.workspace.textDocuments.filter(
    isJovialDiagnosticDocument,
  );
  const openUris = docs.map((doc) => doc.uri.toString());
  const openUriSet = new Set(openUris);
  const pendingUris = takePendingDiagnosticRefreshUris(openUriSet);
  const skippedUris = new Set([...openUris, ...pendingUris]);
  const sourceUris = nextKnownSourceDiagnosticUris(skippedUris);
  const uris = [...openUris, ...pendingUris, ...sourceUris];
  if (uris.length === 0) return;

  diagnosticRefreshInFlight = true;
  try {
    for (const uri of uris) {
      if (client !== c) break;
      const previousResultId = diagnosticPullResultIds.get(uri);
      const params: {
        textDocument: { uri: string };
        previousResultId?: string;
      } = {
        textDocument: { uri },
      };
      if (previousResultId) params.previousResultId = previousResultId;

      try {
        const report = await c.sendRequest("textDocument/diagnostic", params);
        const resultId = diagnosticReportResultId(report);
        if (resultId) diagnosticPullResultIds.set(uri, resultId);
      } catch (e) {
        output.appendLine(
          `Jovial diagnostic refresh (${reason}) failed for ${uri}: ${String(e)}`,
        );
      }
    }
  } finally {
    diagnosticRefreshInFlight = false;
  }
}

function scheduleDiagnosticRefresh(
  output: vscode.OutputChannel,
  delayMs = DIAGNOSTIC_REFRESH_INTERVAL_MS,
  force = false,
): void {
  if (!client) return;
  if (force) clearDiagnosticRefreshTimer();
  if (diagnosticRefreshTimer) return;
  diagnosticRefreshTimer = setTimeout(
    () => {
      diagnosticRefreshTimer = undefined;
      void refreshOpenDocumentDiagnostics(output, "periodic").finally(() => {
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
      const batch = takeWatchedFileBatch(
        pendingWatchedFileChanges,
        WATCH_CHUNK_SIZE,
      );
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
  return set.fileUris.some((raw) => {
    const filePath = parseFileUriFsPath(raw);
    return filePath ? watchPathKey(filePath) === pathKey : false;
  });
}

function addFileUriToCachedSet(
  set: JovialSourceFileSet,
  uri: vscode.Uri,
): void {
  const pathKey = watchPathKey(uri.fsPath);
  if (
    set.fileUris.some((raw) => {
      const filePath = parseFileUriFsPath(raw);
      return filePath ? watchPathKey(filePath) === pathKey : false;
    })
  ) {
    return;
  }
  set.fileUris.push(uri.toString());
  set.fileUris.sort((a, b) => a.localeCompare(b));
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
}

function sourceFileSetForFolder(
  folder: vscode.WorkspaceFolder,
  uris: readonly vscode.Uri[],
  cfg: JovialConfig,
  searchTruncated: boolean,
): JovialSourceFileSet | undefined {
  const rootKey = watchPathKey(folder.uri.fsPath);
  const seen = new Set<string>();
  const fileUris: string[] = [];
  const filePaths: string[] = [];

  for (const uri of uris) {
    if (uri.scheme !== "file") continue;
    const pathKey = watchPathKey(uri.fsPath);
    if (!pathWithinRoot(pathKey, rootKey)) continue;
    if (
      shouldIgnoreWatchedPath(
        uri.fsPath,
        process.platform,
        cfg.sourceExtensions,
      )
    )
      continue;
    if (seen.has(pathKey)) continue;
    seen.add(pathKey);
    fileUris.push(uri.toString());
    filePaths.push(uri.fsPath);
  }

  fileUris.sort((a, b) => a.localeCompare(b));
  filePaths.sort((a, b) => watchPathKey(a).localeCompare(watchPathKey(b)));
  if (filePaths.length === 0) return undefined;

  const rootPath = lowestCommonAncestorPath(
    filePaths.map((filePath) => dirnamePath(filePath)),
  );
  return {
    workspaceUri: folder.uri.toString(),
    rootUri: vscode.Uri.file(rootPath).toString(),
    fileUris,
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
  const folders = vscode.workspace.workspaceFolders ?? [];
  if (folders.length === 0) return [];

  const glob = watcherGlobForSourceExtensions(cfg.sourceExtensions);
  const exclude = sourceDiscoveryExcludeGlob();
  const sets: JovialSourceFileSet[] = [];
  for (const folder of folders) {
    const include = new vscode.RelativePattern(folder, glob);
    const uris = await vscode.workspace.findFiles(
      include,
      exclude,
      cfg.maxStartupFiles,
    );
    const searchTruncated = uris.length >= cfg.maxStartupFiles;
    const set = sourceFileSetForFolder(folder, uris, cfg, searchTruncated);
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
        `[JOVIAL] files: ${set.fileUris.length}${
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
  for (const change of changes) {
    if (
      shouldIgnoreWatchedPath(
        change.fsPath,
        process.platform,
        cfg.sourceExtensions,
      )
    ) {
      continue;
    }

    if (change.type === WATCH_CHANGE_DELETED) return "JOVIAL file deletion";
    if (change.type === WATCH_CHANGE_CHANGED) return "JOVIAL file change";
    if (change.type !== WATCH_CHANGE_CREATED) continue;

    const folder = pickBestWorkspaceFolderForUri(
      vscode.Uri.file(change.fsPath),
    );
    if (!folder) continue;
    const set = sourceFileSetForWorkspaceFolder(folder);
    if (!set) return "new JOVIAL file in workspace";

    const rootPath = sourceFileSetRootPath(set);
    if (!rootPath) return "new JOVIAL file in workspace";
    if (!pathWithinRoot(watchPathKey(change.fsPath), watchPathKey(rootPath))) {
      return "new JOVIAL file outside current root";
    }
  }
  return undefined;
}

function updateCachedSourceFileSetsForChanges(
  changes: readonly PendingWatchedFileChange[],
  cfg: JovialConfig,
): void {
  for (const change of changes) {
    if (
      shouldIgnoreWatchedPath(
        change.fsPath,
        process.platform,
        cfg.sourceExtensions,
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
      addFileUriToCachedSet(set, vscode.Uri.file(change.fsPath));
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
        `files: ${newSet?.fileUris.length ?? 0}`,
    );
  }
}

async function refreshSourceFileSets(
  output: vscode.OutputChannel,
  reason: string,
): Promise<void> {
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

function lsifDefinitionFastPath(
  document: vscode.TextDocument,
  position: vscode.Position,
): vscode.Location[] | undefined {
  const resolved = resolveLsifSymbolsAt(document, position);
  if (!resolved) return undefined;
  const defs = collectLsifLocations(resolved.symbols, (sym) => sym.definitions);
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
  const impls = collectLsifLocations(resolved.symbols, (sym) =>
    sym.implementations.length > 0 ? sym.implementations : sym.definitions,
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
  md.appendMarkdown(
    "\n**Change impact:** Use Find References before renaming or changing type/signature. Cross-file references may exist through COMPOOL imports or external DEF/REF declarations.\n\n",
  );
  md.appendMarkdown(
    "**Note:** This is LSIF fast-path hover. Server hover may add source declaration, scope, implementation preview, and deeper JOVIAL semantics once indexing catches up.",
  );

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
): Promise<T | undefined> {
  return raceServerWithFallback({
    budgetMs,
    token,
    server,
    fallback,
    hasResult: hasProviderResult,
    normalizeServerResult: normalizeResult,
    normalizeFallbackResult: normalizeResult,
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

async function stopClient(status: vscode.StatusBarItem) {
  serverStopRequested = true;
  clearAutoRestartTimer();
  clearDiagnosticRefreshTimer();
  diagnosticPullResultIds.clear();
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
    setStatus(status, "stopped");
    return;
  }
  const c = client;
  client = undefined;
  try {
    await c.stop();
  } finally {
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
    setStatus(status, "error", msg);
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
      setStatus(status, "error", msg);
      if (source !== "auto-restart") {
        vscode.window.showErrorMessage(
          "Jovial LSP: server executable not found. Bundle runtime binaries or set jovial.server.path in Settings (can be relative to workspace).",
        );
      }
      return;
    }

    setStatus(status, "starting", `Starting: ${serverPath}`);

    if (client) {
      await stopClient(status);
      serverStopRequested = false;
    }

    output.appendLine(`Starting Jovial LSP (${source}): ${serverPath}`);

    if (fileWatcher) {
      fileWatcher.dispose();
      fileWatcher = undefined;
    }
    resetWatchedFileStreamingState();
    currentSourceFileSets = [];
    fileWatcher = vscode.workspace.createFileSystemWatcher(
      watcherGlobForSourceExtensions(cfg.sourceExtensions),
    );
    context.subscriptions.push(fileWatcher);
    fileWatcher.onDidCreate((uri) => {
      enqueueWatchedFileChange(
        pendingWatchedFileChanges,
        { fsPath: uri.fsPath, type: WATCH_CHANGE_CREATED },
        {
          isOpenFilePath: isOpenFileDocumentPath,
          sourceExtensions: cfg.sourceExtensions,
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
          sourceExtensions: cfg.sourceExtensions,
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
          sourceExtensions: cfg.sourceExtensions,
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
      (acc, set) => acc + set.fileUris.length,
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

      if (child.stderr) {
        child.stderr.setEncoding("utf8");
        child.stderr.on("data", (chunk: string) =>
          output.appendLine(chunk.toString()),
        );
      }

      child.on("exit", (code, signal) => {
        output.appendLine(`Jovial LSP exited: code=${code} signal=${signal}`);
        setStatus(status, "stopped", `Exited: code=${code} signal=${signal}`);
      });

      child.on("error", (err) => {
        output.appendLine(`Failed to start Jovial LSP: ${String(err)}`);
        setStatus(status, "error", `Failed to start: ${String(err)}`);
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
        provideInlayHints: async (document, range, token, next) => {
          if (!getConfig().features.inlayHints) return [];
          try {
            return await next(document, range, token);
          } catch (e) {
            output.appendLine(`Server inlay hints failed: ${String(e)}`);
            return [];
          }
        },
      },
      errorHandler: {
        error: (error, message, count) => {
          output.appendLine(
            `Client error (${count ?? 0}): ${message ?? ""} ${String(error)}`,
          );
          return { action: ErrorAction.Continue };
        },
        closed: () => {
          output.appendLine("Client closed.");
          setStatus(status, "stopped", "Client closed.");
          client = undefined;
          clearDiagnosticRefreshTimer();
          diagnosticPullResultIds.clear();
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
    );
    autoRestartAttempts = [];
    output.appendLine("Jovial LSP client started.");
    setStatus(status, "running", `ServerReady; background indexing active`);
  } catch (e) {
    output.appendLine(`Client failed to start: ${String(e)}`);
    setStatus(status, "error", `Client failed to start: ${String(e)}`);
    client = undefined;
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
  if (!client) {
    throw new Error("Jovial LSP client is not running.");
  }
  const params: ExecuteCommandParams = { command, arguments: args };
  return client.sendRequest(ExecuteCommandRequest.type, params);
}

export async function activate(context: vscode.ExtensionContext) {
  const output = vscode.window.createOutputChannel("Jovial LSP");
  context.subscriptions.push(output);

  const status = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Left,
    100,
  );
  status.command = "jovial.restartServer";
  status.show();
  context.subscriptions.push(status);
  setStatus(status, "stopped", "Click to start / restart Jovial LSP");

  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument((doc) => {
      if (isJovialDiagnosticDocument(doc)) {
        scheduleDiagnosticRefresh(
          output,
          DIAGNOSTIC_REFRESH_EDITOR_DELAY_MS,
          true,
        );
      }
    }),
    vscode.window.onDidChangeActiveTextEditor((editor) => {
      if (editor && isJovialDiagnosticDocument(editor.document)) {
        scheduleDiagnosticRefresh(
          output,
          DIAGNOSTIC_REFRESH_EDITOR_DELAY_MS,
          true,
        );
      }
    }),
  );

  // UI command (renamed) — does NOT collide with languageclient’s auto registration
  registerExtensionHooks({
    context,
    output,
    status,
    getConfig,
    setStatus,
    startClient,
    stopClient,
    refreshLsifIndex,
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
  });

  if (getConfig().autostart) {
    await startClient(context, output, status);
  }
}

export async function deactivate() {
  serverStopRequested = true;
  clearAutoRestartTimer();
  clearDiagnosticRefreshTimer();
  diagnosticPullResultIds.clear();
  diagnosticSourceRefreshCursor = 0;
  pendingDiagnosticRefreshUris.clear();
  resetLsifState();
  if (fileWatcher) {
    fileWatcher.dispose();
    fileWatcher = undefined;
  }
  resetWatchedFileStreamingState();
  currentSourceFileSets = [];
  if (client) await client.stop();
}
