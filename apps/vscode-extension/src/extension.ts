import * as vscode from "vscode";
import * as cp from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
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
import type {
  LsifLocationData,
  LsifReferenceData,
  LsifSymbolEntryData,
  ParsedLsifDelta,
  ParsedLsifIndex,
} from "./lsif_codec";
import { buildInitializationOptions, readJovialConfig } from "./jovial_config";
import {
  WATCH_CHANGE_CHANGED,
  WATCH_CHANGE_CREATED,
  WATCH_CHANGE_DELETED,
  queueWatchedFileChange as enqueueWatchedFileChange,
  takeWatchedFileBatch,
} from "./watched_file_queue";
import type { PendingWatchedFileChange } from "./watched_file_queue";
import { pickBestWorkspaceRoot, watchPathKey } from "./workspace_paths";
import { registerExtensionHooks } from "./commands";
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

const pendingWatchedFileChanges = new Map<string, PendingWatchedFileChange>();
let pendingWatchedFileFlushTimer: NodeJS.Timeout | undefined;
let watchedFileFlushInFlight = false;
let serverStopRequested = false;
let pendingAutoRestartTimer: NodeJS.Timeout | undefined;
let autoRestartAttempts: number[] = [];
let startInProgress = false;

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
  try {
    while (client && pendingWatchedFileChanges.size > 0) {
      const batch = takeWatchedFileBatch(
        pendingWatchedFileChanges,
        WATCH_CHUNK_SIZE,
      );
      if (batch.length === 0) break;
      lastSentUri = vscode.Uri.file(batch[0].fsPath);
      const params = {
        changes: batch.map((c) => ({
          uri: vscode.Uri.file(c.fsPath).toString(),
          type: c.type,
        })),
      };
      await client.sendNotification("workspace/didChangeWatchedFiles", params);
      sentAny = true;
    }
  } catch (e) {
    output.appendLine(`Watched-file flush failed: ${String(e)}`);
  } finally {
    watchedFileFlushInFlight = false;
    if (sentAny) {
      // Refresh LSIF cache after filesystem-level changes so nav fast path stays current.
      scheduleLsifRefresh(output, "watched-files", lastSentUri);
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

function asRecord(v: unknown): Record<string, unknown> | undefined {
  return v !== null && typeof v === "object"
    ? (v as Record<string, unknown>)
    : undefined;
}

function asFiniteInt(value: unknown): number | undefined {
  if (typeof value !== "number" || !Number.isFinite(value)) return undefined;
  return Math.max(0, Math.trunc(value));
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

function lsifHoverFastPath(
  document: vscode.TextDocument,
  position: vscode.Position,
): vscode.Hover | undefined {
  const resolved = resolveLsifSymbolsAt(document, position);
  if (!resolved || resolved.symbols.length === 0) return undefined;
  const sym = resolved.symbols[0];
  const defs =
    sym.definitions.length > 0 ? sym.definitions : sym.implementations;
  if (defs.length === 0) return undefined;

  const md = new vscode.MarkdownString();
  md.appendMarkdown(`**${sym.key}**\n\n`);
  md.appendMarkdown(
    `Cached definitions (${Math.min(defs.length, 3)} shown):\n`,
  );
  for (const loc of defs.slice(0, 3)) {
    md.appendMarkdown(`- \`${formatLsifLocationForHover(loc)}\`\n`);
  }
  if (defs.length > 3) {
    md.appendMarkdown(`- ...and ${defs.length - 3} more\n`);
  }

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

function uniquePaths(xs: string[]): string[] {
  const seen = new Set<string>();
  const out: string[] = [];
  for (const x of xs) {
    const norm = path.normalize(x);
    if (!seen.has(norm)) {
      seen.add(norm);
      out.push(norm);
    }
  }
  return out;
}

function stripWrappingQuotes(s: string): string {
  if (s.length >= 2) {
    const a = s[0];
    const b = s[s.length - 1];
    if ((a === '"' && b === '"') || (a === "'" && b === "'")) {
      return s.slice(1, -1);
    }
  }
  return s;
}

function expandEnvVars(s: string): string {
  const a = s.replace(
    /\$\{env:([^}]+)\}/g,
    (_m, name: string) => process.env[name] ?? "",
  );
  return a.replace(/%([^%]+)%/g, (_m, name: string) => process.env[name] ?? "");
}

function expandHomeDir(s: string): string {
  if (s === "~") return os.homedir();
  if (s.startsWith("~/") || s.startsWith("~\\")) {
    return path.join(os.homedir(), s.slice(2));
  }
  return s;
}

function normalizeServerPathForPlatform(s: string): string {
  const norm = path.normalize(s);
  if (process.platform === "win32") {
    return norm.replace(/\//g, "\\");
  }
  return norm;
}

function pushCandidate(candidates: string[], p: string): void {
  if (!p) return;
  candidates.push(normalizeServerPathForPlatform(p));
}

function addRelativeCandidates(
  candidates: string[],
  baseDir: string,
  relPath: string,
  maxParentDepth = 3,
): void {
  let cur = normalizeServerPathForPlatform(baseDir);
  for (let i = 0; i <= maxParentDepth; i += 1) {
    pushCandidate(candidates, path.join(cur, relPath));
    const parent = path.dirname(cur);
    if (parent === cur) break;
    cur = parent;
  }
}

type ServerPathSource =
  | "configured"
  | "bundled-exact"
  | "bundled-fallback"
  | "development";
type ServerPathResolution = {
  path: string | undefined;
  source: ServerPathSource;
  note?: string;
};

function bundledRuntimeRelPaths(): { exact: string[]; fallback: string[] } {
  if (process.platform !== "win32") {
    return { exact: [], fallback: [] };
  }
  if (process.arch === "arm64") {
    return {
      exact: [path.join("runtime", "server", "win32-arm64", "jovial-lsp.exe")],
      fallback: [path.join("runtime", "server", "win32-x64", "jovial-lsp.exe")],
    };
  }
  return {
    exact: [path.join("runtime", "server", "win32-x64", "jovial-lsp.exe")],
    fallback: [],
  };
}

function findExistingCandidate(candidates: string[]): string | undefined {
  return uniquePaths(candidates).find((p) => fs.existsSync(p));
}

function collectRelativeProbeCandidates(
  context: vscode.ExtensionContext,
  folders: readonly vscode.WorkspaceFolder[],
  repoRoot: string | undefined,
  relPaths: string[],
): string[] {
  const out: string[] = [];
  for (const rel of relPaths) {
    pushCandidate(out, context.asAbsolutePath(rel));
    addRelativeCandidates(out, context.extensionPath, rel, 2);
    if (repoRoot) {
      pushCandidate(out, path.join(repoRoot, rel));
    }
    for (const f of folders) {
      addRelativeCandidates(out, f.uri.fsPath, rel, 4);
    }
  }
  return uniquePaths(out);
}

function isRepoRootDir(dir: string): boolean {
  const hasGit = fs.existsSync(path.join(dir, ".git"));
  const hasAppsServer = fs.existsSync(path.join(dir, "apps", "lsp-server"));
  const hasAppsExtension = fs.existsSync(
    path.join(dir, "apps", "vscode-extension"),
  );
  const hasLegacyServer = fs.existsSync(path.join(dir, "server_proj"));
  const hasLegacyExtension = fs.existsSync(path.join(dir, "extension_proj"));
  return (
    hasGit ||
    (hasAppsServer && hasAppsExtension) ||
    (hasLegacyServer && hasLegacyExtension)
  );
}

function findRepoRoot(
  folders: readonly vscode.WorkspaceFolder[],
  extensionPath: string,
): string | undefined {
  const seeds = [
    ...folders.map((f) => f.uri.fsPath),
    extensionPath,
    path.dirname(extensionPath),
  ];

  for (const seed of seeds) {
    let cur = normalizeServerPathForPlatform(seed);
    for (let i = 0; i < 12; i += 1) {
      if (isRepoRootDir(cur)) {
        return normalizeServerPathForPlatform(cur);
      }
      const parent = path.dirname(cur);
      if (parent === cur) break;
      cur = parent;
    }
  }
  return undefined;
}

function expandWorkspaceVars(
  s: string,
  folders: readonly vscode.WorkspaceFolder[],
): string[] {
  const named = s.replace(
    /\$\{workspaceFolder:([^}]+)\}/g,
    (_m, name: string) => {
      const hit = folders.find((f) => f.name === name);
      return hit ? hit.uri.fsPath : "";
    },
  );

  if (!named.includes("${workspaceFolder}")) {
    return [named];
  }

  if (folders.length === 0) {
    return [named.replace(/\$\{workspaceFolder\}/g, "")];
  }

  return folders.map((f) =>
    named.replace(/\$\{workspaceFolder\}/g, f.uri.fsPath),
  );
}

function resolveServerPath(
  context: vscode.ExtensionContext,
  configured: string,
  preferBundled: boolean,
): ServerPathResolution {
  const cfgRaw = stripWrappingQuotes((configured ?? "").trim());
  const folders = vscode.workspace.workspaceFolders ?? [];
  const repoRoot = findRepoRoot(folders, context.extensionPath);

  const resolveAutoPath = (): ServerPathResolution => {
    const bundledRuntime = bundledRuntimeRelPaths();
    const bundledExactHit = findExistingCandidate(
      collectRelativeProbeCandidates(
        context,
        folders,
        repoRoot,
        bundledRuntime.exact,
      ),
    );
    if (bundledExactHit) {
      return {
        path: normalizeServerPathForPlatform(bundledExactHit),
        source: "bundled-exact",
      };
    }

    const bundledFallbackHit = findExistingCandidate(
      collectRelativeProbeCandidates(
        context,
        folders,
        repoRoot,
        bundledRuntime.fallback,
      ),
    );
    if (bundledFallbackHit) {
      return {
        path: normalizeServerPathForPlatform(bundledFallbackHit),
        source: "bundled-fallback",
        note: "Bundled win32-arm64 binary was not found; using bundled win32-x64 fallback.",
      };
    }

    const exes =
      process.platform === "win32"
        ? ["jovial-lsp.exe", "Main.exe"]
        : ["jovial-lsp", "Main"];
    const legacyBundledRelCandidates = exes.map((e) => path.join("server", e));
    const legacyBundledHit = findExistingCandidate(
      collectRelativeProbeCandidates(
        context,
        folders,
        repoRoot,
        legacyBundledRelCandidates,
      ),
    );
    if (legacyBundledHit) {
      return {
        path: normalizeServerPathForPlatform(legacyBundledHit),
        source: "bundled-fallback",
        note: "Using legacy bundled server path for compatibility; migrate to runtime/server/<platform-arch>/.",
      };
    }

    const devRelCandidates = [
      ...exes.map((e) =>
        path.join("apps", "lsp-server", "_build", "default", "bin", e),
      ),
      ...exes.map((e) =>
        path.join(
          "apps",
          "lsp-server",
          "_build",
          "install",
          "default",
          "bin",
          e,
        ),
      ),
      ...exes.map((e) =>
        path.join("lsp-server", "_build", "default", "bin", e),
      ),
      ...exes.map((e) =>
        path.join("lsp-server", "_build", "install", "default", "bin", e),
      ),
      ...exes.map((e) =>
        path.join("server_proj", "_build", "default", "bin", e),
      ),
      ...exes.map((e) =>
        path.join("server_proj", "_build", "install", "default", "bin", e),
      ),
    ];
    const devHit = findExistingCandidate(
      collectRelativeProbeCandidates(
        context,
        folders,
        repoRoot,
        devRelCandidates,
      ),
    );
    if (devHit) {
      return {
        path: normalizeServerPathForPlatform(devHit),
        source: "development",
      };
    }
    return { path: undefined, source: "development" };
  };

  const auto = resolveAutoPath();
  if (cfgRaw.length === 0) {
    return auto;
  }

  const expanded = expandWorkspaceVars(
    expandHomeDir(expandEnvVars(cfgRaw)),
    folders,
  );
  const candidates: string[] = [];

  for (const item of expanded) {
    const rawItem = item.trim();
    if (!rawItem) continue;

    if (/^file:\/\//i.test(rawItem)) {
      try {
        candidates.push(
          normalizeServerPathForPlatform(vscode.Uri.parse(rawItem).fsPath),
        );
      } catch {
        // ignore malformed URI
      }
      continue;
    }

    const c = normalizeServerPathForPlatform(rawItem);
    if (!c) continue;

    if (path.isAbsolute(c)) {
      pushCandidate(candidates, c);
      continue;
    }

    // Preferred behavior: resolve relative server paths from repo root first.
    if (repoRoot) {
      pushCandidate(candidates, path.join(repoRoot, c));
    }
    for (const f of folders) {
      addRelativeCandidates(candidates, f.uri.fsPath, c, 4);
    }
    addRelativeCandidates(candidates, context.extensionPath, c, 2);
    pushCandidate(candidates, context.asAbsolutePath(c));
    pushCandidate(candidates, path.resolve(c));
  }

  const ordered = uniquePaths(candidates);
  const configuredHit = ordered.find((p) => fs.existsSync(p));
  if (!configuredHit) {
    if (auto.path) {
      return {
        path: auto.path,
        source: auto.source,
        note: "Configured jovial.server.path was not found; using automatic server resolution instead.",
      };
    }
    return {
      path:
        ordered.length > 0
          ? normalizeServerPathForPlatform(ordered[0])
          : undefined,
      source: "configured",
      note: "Configured jovial.server.path was not found and no bundled/development fallback is available.",
    };
  }

  if (
    preferBundled &&
    auto.path &&
    (auto.source === "bundled-exact" || auto.source === "bundled-fallback") &&
    watchPathKey(auto.path) !== watchPathKey(configuredHit)
  ) {
    return {
      path: auto.path,
      source: auto.source,
      note: "Ignoring jovial.server.path because jovial.server.preferBundled is enabled and a bundled server is available.",
    };
  }

  return {
    path: normalizeServerPathForPlatform(configuredHit),
    source: "configured",
  };
}

async function stopClient(status: vscode.StatusBarItem) {
  serverStopRequested = true;
  clearAutoRestartTimer();
  resetLsifState();
  clearStartupStatus();

  if (fileWatcher) {
    fileWatcher.dispose();
    fileWatcher = undefined;
  }
  resetWatchedFileStreamingState();

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
    fileWatcher = vscode.workspace.createFileSystemWatcher(
      "**/*.{jov,j73,jvl,j}",
    );
    context.subscriptions.push(fileWatcher);
    fileWatcher.onDidCreate((uri) => {
      enqueueWatchedFileChange(
        pendingWatchedFileChanges,
        { fsPath: uri.fsPath, type: WATCH_CHANGE_CREATED },
        { isOpenFilePath: isOpenFileDocumentPath },
      );
      scheduleWatchedFileFlush(output);
    });
    fileWatcher.onDidChange((uri) => {
      enqueueWatchedFileChange(
        pendingWatchedFileChanges,
        { fsPath: uri.fsPath, type: WATCH_CHANGE_CHANGED },
        { isOpenFilePath: isOpenFileDocumentPath },
      );
      scheduleWatchedFileFlush(output);
    });
    fileWatcher.onDidDelete((uri) => {
      enqueueWatchedFileChange(
        pendingWatchedFileChanges,
        { fsPath: uri.fsPath, type: WATCH_CHANGE_DELETED },
        { isOpenFilePath: isOpenFileDocumentPath },
      );
      scheduleWatchedFileFlush(output);
    });

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
      initializationOptions: buildInitializationOptions(cfg),
      outputChannel: output,
      middleware: {
        provideDefinition: async (document, position, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, token);
          } catch (e) {
            output.appendLine(
              `Server definition failed; trying LSIF fallback: ${String(e)}`,
            );
          }
          if (hasProviderResult(serverResult))
            return normalizeNavResult(serverResult);
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifDefinitionFastPath(document, position);
          if (fallback && fallback.length > 0)
            return normalizeNavResult(fallback);
          return serverResult;
        },
        provideDeclaration: async (document, position, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, token);
          } catch (e) {
            output.appendLine(
              `Server declaration failed; trying LSIF fallback: ${String(e)}`,
            );
          }
          if (hasProviderResult(serverResult))
            return normalizeNavResult(serverResult);
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifDeclarationFastPath(document, position);
          if (fallback && fallback.length > 0)
            return normalizeNavResult(fallback);
          return serverResult;
        },
        provideTypeDefinition: async (document, position, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, token);
          } catch (e) {
            output.appendLine(
              `Server typeDefinition failed; trying LSIF fallback: ${String(e)}`,
            );
          }
          if (hasProviderResult(serverResult))
            return normalizeNavResult(serverResult);
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifTypeDefinitionFastPath(document, position);
          if (fallback && fallback.length > 0)
            return normalizeNavResult(fallback);
          return serverResult;
        },
        provideImplementation: async (document, position, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, token);
          } catch (e) {
            output.appendLine(
              `Server implementation failed; trying LSIF fallback: ${String(e)}`,
            );
          }
          if (hasProviderResult(serverResult))
            return normalizeNavResult(serverResult);
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifImplementationFastPath(document, position);
          if (fallback && fallback.length > 0)
            return normalizeNavResult(fallback);
          return serverResult;
        },
        provideReferences: async (document, position, context, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, context, token);
          } catch (e) {
            output.appendLine(
              `Server references failed; trying LSIF fallback: ${String(e)}`,
            );
          }
          if (hasProviderResult(serverResult))
            return normalizeNavResult(serverResult);
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifReferencesFastPath(
            document,
            position,
            context.includeDeclaration,
          );
          if (fallback && fallback.length > 0)
            return normalizeNavResult(fallback);
          return serverResult;
        },
        provideHover: async (document, position, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, token);
          } catch (e) {
            output.appendLine(
              `Server hover failed; trying LSIF fallback: ${String(e)}`,
            );
          }
          if (hasProviderResult(serverResult)) return serverResult;
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifHoverFastPath(document, position);
          if (fallback) return fallback;
          return serverResult;
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

    await client.start();
    clearStartupStatus();
    const startupDiagTargetMs = STARTUP_DIAG_TARGET_DEFAULT_MS;
    const startupNavTargetMs = STARTUP_NAV_TARGET_DEFAULT_MS;
    setStatus(
      status,
      "running",
      `Startup warming (diag/hover ${startupDiagTargetMs}ms, full nav ${startupNavTargetMs}ms)`,
    );
    bindStartupNotifications({
      client,
      output,
      status,
      startupDiagTargetMs,
      startupNavTargetMs,
    });
    applyTraceSetting(output);
    await flushWatchedFileChanges(output);
    scheduleLsifRefresh(output, "startup", firstPreferredLsifUri(), 200);
    autoRestartAttempts = [];
    output.appendLine("Jovial LSP client started.");
    setStatus(
      status,
      "running",
      `Startup warming (diag/hover ${startupDiagTargetMs}ms, full nav ${startupNavTargetMs}ms)`,
    );
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
  resetLsifState();
  if (fileWatcher) {
    fileWatcher.dispose();
    fileWatcher = undefined;
  }
  resetWatchedFileStreamingState();
  if (client) await client.stop();
}
