import * as vscode from "vscode";
import * as cp from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { Worker } from "worker_threads";

import {
  LanguageClient,
  LanguageClientOptions,
  CloseAction,
  ErrorAction,
  ExecuteCommandRequest,
  ExecuteCommandParams,
  StreamInfo,
  Trace,
} from "vscode-languageclient/node";
import {
  LsifLocationData,
  LsifReferenceData,
  LsifSymbolEntryData,
  ParsedLsifDelta,
  ParsedLsifIndex,
  parseLsifDeltaPayload,
  parseLsifIndexPayload,
} from "./lsif_codec";

let client: LanguageClient | undefined;
let fileWatcher: vscode.FileSystemWatcher | undefined;
type WatchedFileChangeType = 1 | 2 | 3;
type PendingWatchedFileChange = { fsPath: string; type: WatchedFileChangeType };

const WATCH_CHANGE_CREATED: WatchedFileChangeType = 1;
const WATCH_CHANGE_CHANGED: WatchedFileChangeType = 2;
const WATCH_CHANGE_DELETED: WatchedFileChangeType = 3;
const WATCH_FLUSH_DELAY_MS = 150;
const WATCH_CHUNK_SIZE = 256;
const WATCH_FORCE_FLUSH_SIZE = 2000;
const AUTO_RESTART_DELAY_MS = 1200;
const AUTO_RESTART_WINDOW_MS = 120000;
const AUTO_RESTART_MAX_ATTEMPTS = 5;
const LSIF_REFRESH_DEBOUNCE_MS = 800;
const LSIF_REFRESH_MIN_INTERVAL_MS = 1200;
const LSIF_MAX_FAST_RESULTS = 300;

let pendingWatchedFileChanges = new Map<string, PendingWatchedFileChange>();
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

function watchPathKey(fsPath: string): string {
  const norm = path.normalize(fsPath);
  return process.platform === "win32" ? norm.toLowerCase() : norm;
}

function pathWithinRoot(pathKey: string, rootKey: string): boolean {
  if (pathKey === rootKey) return true;
  if (!pathKey.startsWith(rootKey)) return false;
  const next = pathKey.charAt(rootKey.length);
  return next === path.sep || next === "/" || next === "\\";
}

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

function pickBestWorkspaceFolderForUri(uri: vscode.Uri): vscode.WorkspaceFolder | undefined {
  const folders = vscode.workspace.workspaceFolders ?? [];
  if (folders.length === 0) return undefined;
  const direct = vscode.workspace.getWorkspaceFolder(uri);
  if (direct) return direct;
  if (uri.scheme !== "file") return folders[0];
  const pathKey = watchPathKey(uri.fsPath);
  let best: vscode.WorkspaceFolder | undefined;
  let bestLen = -1;
  for (const folder of folders) {
    const rootKey = watchPathKey(folder.uri.fsPath);
    if (!pathWithinRoot(pathKey, rootKey)) continue;
    if (rootKey.length > bestLen) {
      best = folder;
      bestLen = rootKey.length;
    }
  }
  return best ?? folders[0];
}

function firstPreferredLsifUri(): vscode.Uri | undefined {
  const active = vscode.window.activeTextEditor?.document;
  if (active && active.languageId === "jovial" && active.uri.scheme === "file") {
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
  allowFallback = true
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
    return { rootKey: rootKeyForFolder(folder), contextUri: normalizeNavUri(uri) };
  }
  return { rootKey: rootKeyForFileUri(uri), contextUri: normalizeNavUri(uri) };
}

function shouldIgnoreWatchedPath(fsPath: string): boolean {
  const norm = watchPathKey(fsPath).replace(/\\/g, "/");
  return (
    norm.includes("/.git/") ||
    norm.includes("/_build/") ||
    norm.includes("/node_modules/") ||
    norm.includes("/.vscode/")
  );
}

function isOpenFileDocumentPath(fsPath: string): boolean {
  const target = watchPathKey(fsPath);
  return vscode.workspace.textDocuments.some(
    (doc) => doc.uri.scheme === "file" && watchPathKey(doc.uri.fsPath) === target
  );
}

function mergeWatchedChangeType(
  prev: WatchedFileChangeType,
  next: WatchedFileChangeType
): WatchedFileChangeType | null {
  if (prev === WATCH_CHANGE_CREATED && next === WATCH_CHANGE_DELETED) return null;
  if (prev === WATCH_CHANGE_DELETED && next === WATCH_CHANGE_CREATED) return WATCH_CHANGE_CHANGED;
  if (prev === WATCH_CHANGE_CREATED && next === WATCH_CHANGE_CHANGED) return WATCH_CHANGE_CREATED;
  if (prev === WATCH_CHANGE_CHANGED && next === WATCH_CHANGE_DELETED) return WATCH_CHANGE_DELETED;
  return next;
}

function queueWatchedFileChange(uri: vscode.Uri, type: WatchedFileChangeType): void {
  const fsPath = uri.fsPath;
  if (!fsPath || shouldIgnoreWatchedPath(fsPath)) return;
  // Open documents already stream content via textDocument/didChange.
  if (type === WATCH_CHANGE_CHANGED && isOpenFileDocumentPath(fsPath)) return;

  const key = watchPathKey(fsPath);
  const prev = pendingWatchedFileChanges.get(key);
  if (!prev) {
    pendingWatchedFileChanges.set(key, { fsPath, type });
    return;
  }

  const merged = mergeWatchedChangeType(prev.type, type);
  if (merged === null) pendingWatchedFileChanges.delete(key);
  else pendingWatchedFileChanges.set(key, { fsPath, type: merged });
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

function takeWatchedFileBatch(maxItems: number): PendingWatchedFileChange[] {
  const out: PendingWatchedFileChange[] = [];
  for (const [k, v] of pendingWatchedFileChanges) {
    out.push(v);
    pendingWatchedFileChanges.delete(k);
    if (out.length >= maxItems) break;
  }
  return out;
}

async function flushWatchedFileChanges(output: vscode.OutputChannel): Promise<void> {
  clearWatchedFileFlushTimer();
  if (watchedFileFlushInFlight || pendingWatchedFileChanges.size === 0 || !client) return;

  watchedFileFlushInFlight = true;
  let sentAny = false;
  let lastSentUri: vscode.Uri | undefined;
  try {
    while (client && pendingWatchedFileChanges.size > 0) {
      const batch = takeWatchedFileBatch(WATCH_CHUNK_SIZE);
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
  const cfg = vscode.workspace.getConfiguration("jovial");
  const workspaceDiagnosticsMode = cfg.get<"off" | "errors" | "all">(
    "workspaceDiagnostics.mode",
    "errors"
  );
  const backgroundIndexBudgetMs = Math.max(1, Math.trunc(cfg.get<number>("background.indexBudgetMs", 8)));
  const backgroundDiagBatchSize = Math.max(
    1,
    Math.trunc(cfg.get<number>("background.diagBatchSize", 64))
  );
  return {
    serverPath: cfg.get<string>("server.path", ""),
    preferBundled: cfg.get<boolean>("server.preferBundled", true),
    serverArgs: cfg.get<string[]>("server.args", []),
    autostart: cfg.get<boolean>("autostart", true),
    trace: cfg.get<"off" | "messages" | "verbose">("trace", "off"),
    lsifFastPath: cfg.get<boolean>("lsif.fastPath", false),
    workspaceDiagnosticsMode,
    backgroundIndexBudgetMs,
    backgroundDiagBatchSize,
  };
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
  return v !== null && typeof v === "object" ? (v as Record<string, unknown>) : undefined;
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
        "LSIF worker script is missing; falling back to extension-thread LSIF parsing."
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
        new Error(typeof errRaw === "string" ? errRaw : "Unknown LSIF worker response error.")
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
  fallback: () => T | undefined
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
      `LSIF worker task '${kind}' failed; falling back to extension-thread parse: ${String(e)}`
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
    symbolIdsByKey.set(entry.key, Array.from(new Set(ids)).sort((a, b) => a.localeCompare(b)));
  }
  for (const sym of symbolsById.values()) {
    const prev = symbolIdsByKey.get(sym.key) ?? [];
    if (!prev.includes(sym.id)) prev.push(sym.id);
    prev.sort((a, b) => a.localeCompare(b));
    symbolIdsByKey.set(sym.key, prev);
  }

  const occurrenceIndexByDoc = new Map<string, Map<number, LsifOccurrenceEntry[]>>();
  const appendOccurrence = (docUri: string, line: number, entry: LsifOccurrenceEntry): void => {
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
      const startCharacter = Math.max(0, Math.trunc(ref.location.startCharacter));
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

async function parseLsifIndex(output: vscode.OutputChannel, payload: unknown): Promise<LsifIndexCache | undefined> {
  const parsed = await runLsifWorkerTask<ParsedLsifIndex>(
    output,
    "parseIndex",
    payload,
    () => parseLsifIndexPayload(payload)
  );
  return parsed ? toLsifIndexCache(parsed) : undefined;
}

async function parseLsifDelta(output: vscode.OutputChannel, payload: unknown): Promise<LsifDeltaPayload | undefined> {
  return runLsifWorkerTask<LsifDeltaPayload>(
    output,
    "parseDelta",
    payload,
    () => parseLsifDeltaPayload(payload)
  );
}

function applyLsifDelta(cache: LsifIndexCache, delta: LsifDeltaPayload): LsifIndexCache {
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

function toVscodeLocations(locations: readonly LsifLocationData[]): vscode.Location[] {
  const out: vscode.Location[] = [];
  for (const loc of locations) {
    const parsed = toVscodeLocation(loc);
    if (parsed) out.push(parsed);
  }
  return out;
}

function normalizeSymbolKeyAtPosition(
  document: vscode.TextDocument,
  position: vscode.Position
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
  pos: vscode.Position
): boolean {
  if (pos.line < entry.startLine || pos.line > entry.endLine) return false;
  if (entry.startLine === entry.endLine) {
    return pos.character >= entry.startCharacter && pos.character <= entry.endCharacter;
  }
  if (pos.line === entry.startLine) return pos.character >= entry.startCharacter;
  if (pos.line === entry.endLine) return pos.character <= entry.endCharacter;
  return true;
}

function lsifOccurrenceSpan(entry: LsifOccurrenceEntry): number {
  const lineDelta = entry.endLine - entry.startLine;
  const charDelta = entry.endCharacter - entry.startCharacter;
  return lineDelta * 100000 + charDelta;
}

function lsifCacheForDocument(document: vscode.TextDocument): LsifIndexCache | undefined {
  if (!getConfig().lsifFastPath) return undefined;
  if (document.languageId !== "jovial") return undefined;
  const ctx = resolveLsifRootContext(document.uri, false);
  if (!ctx) return undefined;
  return lsifRootStates.get(ctx.rootKey)?.cache;
}

function lsifSymbolIdAtPosition(
  cache: LsifIndexCache,
  docUri: string,
  position: vscode.Position
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
    if (span < cur || (span === cur && entry.symbolId.localeCompare(best.symbolId) < 0)) {
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
  position: vscode.Position
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

function dedupeLsifLocations(locations: readonly LsifLocationData[]): LsifLocationData[] {
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
  select: (sym: LsifSymbolEntry) => readonly LsifLocationData[]
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
  position: vscode.Position
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
  position: vscode.Position
): vscode.Location[] | undefined {
  return lsifDefinitionFastPath(document, position);
}

function lsifImplementationFastPath(
  document: vscode.TextDocument,
  position: vscode.Position
): vscode.Location[] | undefined {
  const resolved = resolveLsifSymbolsAt(document, position);
  if (!resolved) return undefined;
  const impls = collectLsifLocations(resolved.symbols, (sym) =>
    sym.implementations.length > 0 ? sym.implementations : sym.definitions
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
  position: vscode.Position
): vscode.Location[] | undefined {
  const resolved = resolveLsifSymbolsAt(document, position);
  if (!resolved) return undefined;
  const typeSymbols = resolved.symbols.filter((sym) => isLsifTypeLikeKind(sym.kind));
  if (typeSymbols.length === 0) return undefined;
  const defs = collectLsifLocations(typeSymbols, (sym) => sym.definitions);
  if (defs.length === 0) return undefined;
  const locations = toVscodeLocations(defs);
  return locations.length > 0 ? locations : undefined;
}

function lsifReferencesFastPath(
  document: vscode.TextDocument,
  position: vscode.Position,
  includeDeclaration: boolean
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
      uri.scheme === "file"
        ? uri.fsPath.replace(/\\/g, "/")
        : uri.toString();
    return `${pathPart}:${loc.startLine + 1}:${loc.startCharacter + 1}`;
  } catch {
    return `${loc.uri}:${loc.startLine + 1}:${loc.startCharacter + 1}`;
  }
}

function lsifHoverFastPath(
  document: vscode.TextDocument,
  position: vscode.Position
): vscode.Hover | undefined {
  const resolved = resolveLsifSymbolsAt(document, position);
  if (!resolved || resolved.symbols.length === 0) return undefined;
  const sym = resolved.symbols[0];
  const defs = sym.definitions.length > 0 ? sym.definitions : sym.implementations;
  if (defs.length === 0) return undefined;

  const md = new vscode.MarkdownString();
  md.appendMarkdown(`**${sym.key}**\n\n`);
  md.appendMarkdown(`Cached definitions (${Math.min(defs.length, 3)} shown):\n`);
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
  if (!start || typeof start !== "object" || !end || typeof end !== "object") return undefined;
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
    new vscode.Position(Math.max(0, Math.trunc(sl)), Math.max(0, Math.trunc(sc))),
    new vscode.Position(Math.max(0, Math.trunc(el)), Math.max(0, Math.trunc(ec)))
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
  delayMs: number
): void {
  const state = lsifRootStateFor(context.rootKey);
  state.refreshPending = true;
  state.pendingReason = reason;
  state.lastContextUri = context.contextUri;
  clearLsifRootTimer(state);
  state.refreshTimer = setTimeout(() => {
    state.refreshTimer = undefined;
    void refreshLsifIndex(output, reason, context.contextUri);
  }, Math.max(0, Math.trunc(delayMs)));
}

async function refreshLsifIndex(
  output: vscode.OutputChannel,
  reason: string,
  preferredUri?: vscode.Uri
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
      LSIF_REFRESH_MIN_INTERVAL_MS - elapsed
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
            `${delta.upserts.length} upserts, ${delta.deletes.length} deletes.`
          );
          return;
        }
        if (delta?.reset) {
          output.appendLine(
            `LSIF delta requested full refresh (${reason}, ${context.rootKey}) at base rev ${delta.baseRevision}.`
          );
        }
      } catch (deltaErr) {
        output.appendLine(
          `LSIF delta refresh fallback (${reason}, ${context.rootKey}): ${String(deltaErr)}`
        );
      }
    }

    const result = await executeServerCommand("jovial.dumpLsifIndex", [context.contextUri.toString()]);
    const parsed = await parseLsifIndex(output, result);
    if (!parsed) {
      output.appendLine("LSIF cache refresh skipped: invalid server payload.");
      return;
    }
    state.cache = parsed;
    state.lastRefreshMs = Date.now();
    output.appendLine(
      `LSIF cache refreshed (${reason}, ${context.rootKey}): rev ${parsed.revision}, ` +
      `${parsed.symbolCount} symbols across ${parsed.docCount} docs.`
    );
  } catch (e) {
    output.appendLine(`LSIF cache refresh failed (${reason}, ${context.rootKey}): ${String(e)}`);
  } finally {
    state.refreshInFlight = false;
    if (state.refreshPending && client && getConfig().lsifFastPath) {
      const pendingReason = state.pendingReason ?? `${reason}-pending`;
      state.refreshPending = false;
      state.pendingReason = undefined;
      const followUri = state.lastContextUri ?? context.contextUri;
      const followContext = resolveLsifRootContext(followUri, true);
      if (followContext) {
        scheduleLsifRefreshForContext(output, pendingReason, followContext, LSIF_REFRESH_DEBOUNCE_MS);
      }
    }
  }
}

function scheduleLsifRefresh(
  output: vscode.OutputChannel,
  reason: string,
  preferredUri?: vscode.Uri,
  delayMs = LSIF_REFRESH_DEBOUNCE_MS
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
    (t) => now - t <= AUTO_RESTART_WINDOW_MS
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
    if ((a === "\"" && b === "\"") || (a === "'" && b === "'")) {
      return s.slice(1, -1);
    }
  }
  return s;
}

function expandEnvVars(s: string): string {
  const a = s.replace(/\$\{env:([^}]+)\}/g, (_m, name: string) => process.env[name] ?? "");
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
  maxParentDepth = 3
): void {
  let cur = normalizeServerPathForPlatform(baseDir);
  for (let i = 0; i <= maxParentDepth; i += 1) {
    pushCandidate(candidates, path.join(cur, relPath));
    const parent = path.dirname(cur);
    if (parent === cur) break;
    cur = parent;
  }
}

type ServerPathSource = "configured" | "bundled-exact" | "bundled-fallback" | "development";
type ServerPathResolution = { path: string | undefined; source: ServerPathSource; note?: string };

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
  relPaths: string[]
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
  const hasAppsExtension = fs.existsSync(path.join(dir, "apps", "vscode-extension"));
  const hasLegacyServer = fs.existsSync(path.join(dir, "server_proj"));
  const hasLegacyExtension = fs.existsSync(path.join(dir, "extension_proj"));
  return hasGit || (hasAppsServer && hasAppsExtension) || (hasLegacyServer && hasLegacyExtension);
}

function findRepoRoot(
  folders: readonly vscode.WorkspaceFolder[],
  extensionPath: string
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
  folders: readonly vscode.WorkspaceFolder[]
): string[] {
  const named = s.replace(/\$\{workspaceFolder:([^}]+)\}/g, (_m, name: string) => {
    const hit = folders.find((f) => f.name === name);
    return hit ? hit.uri.fsPath : "";
  });

  if (!named.includes("${workspaceFolder}")) {
    return [named];
  }

  if (folders.length === 0) {
    return [named.replace(/\$\{workspaceFolder\}/g, "")];
  }

  return folders.map((f) => named.replace(/\$\{workspaceFolder\}/g, f.uri.fsPath));
}

function resolveServerPath(
  context: vscode.ExtensionContext,
  configured: string,
  preferBundled: boolean
): ServerPathResolution {
  const cfgRaw = stripWrappingQuotes((configured ?? "").trim());
  const folders = vscode.workspace.workspaceFolders ?? [];
  const repoRoot = findRepoRoot(folders, context.extensionPath);

  const resolveAutoPath = (): ServerPathResolution => {
    const bundledRuntime = bundledRuntimeRelPaths();
    const bundledExactHit = findExistingCandidate(
      collectRelativeProbeCandidates(context, folders, repoRoot, bundledRuntime.exact)
    );
    if (bundledExactHit) {
      return {
        path: normalizeServerPathForPlatform(bundledExactHit),
        source: "bundled-exact",
      };
    }

    const bundledFallbackHit = findExistingCandidate(
      collectRelativeProbeCandidates(context, folders, repoRoot, bundledRuntime.fallback)
    );
    if (bundledFallbackHit) {
      return {
        path: normalizeServerPathForPlatform(bundledFallbackHit),
        source: "bundled-fallback",
        note: "Bundled win32-arm64 binary was not found; using bundled win32-x64 fallback.",
      };
    }

    const exes = process.platform === "win32" ? ["jovial-lsp.exe", "Main.exe"] : ["jovial-lsp", "Main"];
    const legacyBundledRelCandidates = exes.map((e) => path.join("server", e));
    const legacyBundledHit = findExistingCandidate(
      collectRelativeProbeCandidates(context, folders, repoRoot, legacyBundledRelCandidates)
    );
    if (legacyBundledHit) {
      return {
        path: normalizeServerPathForPlatform(legacyBundledHit),
        source: "bundled-fallback",
        note: "Using legacy bundled server path for compatibility; migrate to runtime/server/<platform-arch>/.",
      };
    }

    const devRelCandidates = [
      ...exes.map((e) => path.join("apps", "lsp-server", "_build", "default", "bin", e)),
      ...exes.map((e) => path.join("apps", "lsp-server", "_build", "install", "default", "bin", e)),
      ...exes.map((e) => path.join("lsp-server", "_build", "default", "bin", e)),
      ...exes.map((e) => path.join("lsp-server", "_build", "install", "default", "bin", e)),
      ...exes.map((e) => path.join("server_proj", "_build", "default", "bin", e)),
      ...exes.map((e) => path.join("server_proj", "_build", "install", "default", "bin", e)),
    ];
    const devHit = findExistingCandidate(
      collectRelativeProbeCandidates(context, folders, repoRoot, devRelCandidates)
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

  const expanded = expandWorkspaceVars(expandHomeDir(expandEnvVars(cfgRaw)), folders);
  const candidates: string[] = [];

  for (const item of expanded) {
    const rawItem = item.trim();
    if (!rawItem) continue;

    if (/^file:\/\//i.test(rawItem)) {
      try {
        candidates.push(normalizeServerPathForPlatform(vscode.Uri.parse(rawItem).fsPath));
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
        note:
          "Configured jovial.server.path was not found; using automatic server resolution instead.",
      };
    }
    return {
      path: ordered.length > 0 ? normalizeServerPathForPlatform(ordered[0]) : undefined,
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
      note:
        "Ignoring jovial.server.path because jovial.server.preferBundled is enabled and a bundled server is available.",
    };
  }

  return { path: normalizeServerPathForPlatform(configuredHit), source: "configured" };
}

function setStatus(
  status: vscode.StatusBarItem,
  kind: "starting" | "running" | "stopped" | "error",
  detail?: string
) {
  switch (kind) {
    case "starting":
      status.text = `$(sync~spin) Jovial LSP: starting...`;
      status.color = "#ffd24d";
      status.tooltip = detail ?? "Starting Jovial LSP (click to restart)";
      break;
    case "running":
      status.text = `$(check) Jovial LSP: running`;
      status.color = "#4dff88";
      status.tooltip = detail ?? "Jovial LSP running (click to restart)";
      break;
    case "stopped":
      status.text = `$(circle-slash) Jovial LSP: stopped`;
      status.color = "#cccccc";
      status.tooltip = detail ?? "Jovial LSP stopped (click to start)";
      break;
    case "error":
      status.text = `$(error) Jovial LSP: error`;
      status.color = "#ff4d4d";
      status.tooltip = detail ?? "Jovial LSP error (click to restart)";
      break;
  }
}

async function stopClient(status: vscode.StatusBarItem) {
  serverStopRequested = true;
  clearAutoRestartTimer();
  resetLsifState();

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
    setStatus(status, "stopped");
  }
}

function scheduleAutoRestart(
  context: vscode.ExtensionContext,
  output: vscode.OutputChannel,
  status: vscode.StatusBarItem,
  reason: string
): void {
  if (serverStopRequested) return;
  if (!getConfig().autostart) return;
  if (pendingAutoRestartTimer) return;
  if (!shouldAttemptAutoRestart()) {
    const msg =
      `Auto-restart limit reached (${AUTO_RESTART_MAX_ATTEMPTS} failures in ${Math.floor(
        AUTO_RESTART_WINDOW_MS / 1000
      )}s).`;
    output.appendLine(msg);
    setStatus(status, "error", msg);
    return;
  }

  output.appendLine(`Scheduling Jovial LSP restart (${reason}) in ${AUTO_RESTART_DELAY_MS}ms.`);
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
  source: "manual" | "auto-restart" | "config-change" = "manual"
) {
  if (startInProgress) {
    output.appendLine(`Start request (${source}) ignored: startup already in progress.`);
    return;
  }

  startInProgress = true;
  clearAutoRestartTimer();
  serverStopRequested = false;

  try {
    const cfg = getConfig();
    const resolvedServer = resolveServerPath(context, cfg.serverPath, cfg.preferBundled);
    const serverPath = resolvedServer.path;

    output.appendLine(`Resolved server path (${resolvedServer.source}): ${serverPath ?? "<none>"}`);
    if (resolvedServer.note) {
      output.appendLine(resolvedServer.note);
    }

    if (!serverPath || !fs.existsSync(serverPath)) {
      const msg =
        "Server executable not found. Bundle runtime binaries under runtime/server/<platform-arch>/ or set jovial.server.path.";
      setStatus(status, "error", msg);
      if (source !== "auto-restart") {
        vscode.window.showErrorMessage(
          "Jovial LSP: server executable not found. Bundle runtime binaries or set jovial.server.path in Settings (can be relative to workspace)."
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
    fileWatcher = vscode.workspace.createFileSystemWatcher("**/*.{jov,j73,jvl,j}");
    context.subscriptions.push(fileWatcher);
    fileWatcher.onDidCreate((uri) => {
      queueWatchedFileChange(uri, WATCH_CHANGE_CREATED);
      scheduleWatchedFileFlush(output);
    });
    fileWatcher.onDidChange((uri) => {
      queueWatchedFileChange(uri, WATCH_CHANGE_CHANGED);
      scheduleWatchedFileFlush(output);
    });
    fileWatcher.onDidDelete((uri) => {
      queueWatchedFileChange(uri, WATCH_CHANGE_DELETED);
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
          JOVIAL_WORKSPACE_DIAGS_MODE: cfg.workspaceDiagnosticsMode,
          JOVIAL_BG_TICK_BUDGET_MS: String(cfg.backgroundIndexBudgetMs),
          JOVIAL_BG_DIAG_BATCH_SIZE: String(cfg.backgroundDiagBatchSize),
        },
        windowsHide: true,
        stdio: stdioMode,
      });

      if (child.stderr) {
        child.stderr.setEncoding("utf8");
        child.stderr.on("data", (chunk: string) => output.appendLine(chunk.toString()));
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
      outputChannel: output,
      middleware: {
        provideDefinition: async (document, position, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, token);
          } catch (e) {
            output.appendLine(`Server definition failed; trying LSIF fallback: ${String(e)}`);
          }
          if (hasProviderResult(serverResult)) return normalizeNavResult(serverResult);
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifDefinitionFastPath(document, position);
          if (fallback && fallback.length > 0) return normalizeNavResult(fallback);
          return serverResult;
        },
        provideDeclaration: async (document, position, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, token);
          } catch (e) {
            output.appendLine(`Server declaration failed; trying LSIF fallback: ${String(e)}`);
          }
          if (hasProviderResult(serverResult)) return normalizeNavResult(serverResult);
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifDeclarationFastPath(document, position);
          if (fallback && fallback.length > 0) return normalizeNavResult(fallback);
          return serverResult;
        },
        provideTypeDefinition: async (document, position, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, token);
          } catch (e) {
            output.appendLine(`Server typeDefinition failed; trying LSIF fallback: ${String(e)}`);
          }
          if (hasProviderResult(serverResult)) return normalizeNavResult(serverResult);
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifTypeDefinitionFastPath(document, position);
          if (fallback && fallback.length > 0) return normalizeNavResult(fallback);
          return serverResult;
        },
        provideImplementation: async (document, position, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, token);
          } catch (e) {
            output.appendLine(`Server implementation failed; trying LSIF fallback: ${String(e)}`);
          }
          if (hasProviderResult(serverResult)) return normalizeNavResult(serverResult);
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifImplementationFastPath(document, position);
          if (fallback && fallback.length > 0) return normalizeNavResult(fallback);
          return serverResult;
        },
        provideReferences: async (document, position, context, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, context, token);
          } catch (e) {
            output.appendLine(`Server references failed; trying LSIF fallback: ${String(e)}`);
          }
          if (hasProviderResult(serverResult)) return normalizeNavResult(serverResult);
          if (token.isCancellationRequested) return serverResult;
          const fallback = lsifReferencesFastPath(document, position, context.includeDeclaration);
          if (fallback && fallback.length > 0) return normalizeNavResult(fallback);
          return serverResult;
        },
        provideHover: async (document, position, token, next) => {
          let serverResult: Awaited<ReturnType<typeof next>> | undefined;
          try {
            serverResult = await next(document, position, token);
          } catch (e) {
            output.appendLine(`Server hover failed; trying LSIF fallback: ${String(e)}`);
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
          output.appendLine(`Client error (${count ?? 0}): ${message ?? ""} ${String(error)}`);
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
    client = new LanguageClient("jovialLsp", "Jovial Language Server", serverOptions, clientOptions);

    await client.start();
    applyTraceSetting(output);
    await flushWatchedFileChanges(output);
    scheduleLsifRefresh(output, "startup", firstPreferredLsifUri(), 200);
    autoRestartAttempts = [];
    output.appendLine("Jovial LSP client started.");
    setStatus(status, "running", `Server: ${serverPath}`);
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

async function dumpAstUi(output: vscode.OutputChannel) {
  if (!client) {
    vscode.window.showWarningMessage("Jovial LSP is not running.");
    return;
  }
  const editor = vscode.window.activeTextEditor;
  if (!editor) return;

  const uri = editor.document.uri.toString();

  // Call the *server* command name here:
  const params: ExecuteCommandParams = {
    command: "jovial.dumpAst",
    arguments: [uri],
  };

  const res = await client.sendRequest(ExecuteCommandRequest.type, params);
  const text = typeof res === "string" ? res : JSON.stringify(res, null, 2);

  const doc = await vscode.workspace.openTextDocument({ content: text, language: "plaintext" });
  await vscode.window.showTextDocument(doc, { preview: true });
}

async function dumpCstUi(output: vscode.OutputChannel) {
  if (!client) {
    vscode.window.showWarningMessage("Jovial LSP is not running.");
    return;
  }
  const editor = vscode.window.activeTextEditor;
  if (!editor) return;

  const uri = editor.document.uri.toString();
  const params: ExecuteCommandParams = {
    command: "jovial.dumpCst",
    arguments: [uri],
  };

  const res = await client.sendRequest(ExecuteCommandRequest.type, params);
  const text = typeof res === "string" ? res : JSON.stringify(res, null, 2);

  const doc = await vscode.workspace.openTextDocument({ content: text, language: "plaintext" });
  await vscode.window.showTextDocument(doc, { preview: true });
}

async function executeServerCommand(command: string, args: unknown[]): Promise<unknown> {
  if (!client) {
    throw new Error("Jovial LSP client is not running.");
  }
  const params: ExecuteCommandParams = { command, arguments: args };
  return client.sendRequest(ExecuteCommandRequest.type, params);
}

function toDisplayText(res: unknown, fallback: string): string {
  if (typeof res === "string") return res;
  if (res === null || res === undefined) return fallback;
  try {
    return JSON.stringify(res, null, 2);
  } catch {
    return String(res);
  }
}

function escapeHtml(s: string): string {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

function syntaxTreeHtml(
  fileLabel: string,
  uri: string,
  astText: string,
  cstText: string,
  activeTab: "ast" | "cst"
): string {
  const astPayload = JSON.stringify(astText);
  const cstPayload = JSON.stringify(cstText);
  const astActive = activeTab === "ast";
  const shown = astActive ? astText : cstText;
  const shownTitle = astActive ? "AST" : "CST (Token Stream)";
  return `<!doctype html>
<html>
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <style>
    :root {
      --bg: #11151a;
      --panel: #1a2129;
      --text: #d6dee8;
      --muted: #98a7b8;
      --accent: #48b06a;
      --border: #2d3946;
    }
    body {
      margin: 0;
      padding: 0;
      background: radial-gradient(1200px 800px at 10% -20%, #213241 0%, var(--bg) 55%);
      color: var(--text);
      font-family: Consolas, "Cascadia Code", "Fira Code", monospace;
    }
    .wrap { padding: 14px; }
    .header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 10px;
      gap: 8px;
    }
    .title {
      font-weight: 700;
      font-size: 13px;
      letter-spacing: 0.2px;
    }
    .meta {
      color: var(--muted);
      font-size: 11px;
      margin-top: 2px;
      word-break: break-all;
    }
    .actions { display: flex; gap: 8px; }
    button {
      border: 1px solid var(--border);
      background: var(--panel);
      color: var(--text);
      font-size: 12px;
      padding: 6px 10px;
      border-radius: 7px;
      cursor: pointer;
    }
    button.active {
      border-color: var(--accent);
      box-shadow: 0 0 0 1px #2a4f36 inset;
      color: #c9f2d6;
    }
    .viewer {
      border: 1px solid var(--border);
      border-radius: 10px;
      overflow: hidden;
      background: #0f141a;
    }
    .bar {
      display: flex;
      justify-content: space-between;
      gap: 8px;
      align-items: center;
      padding: 8px 10px;
      background: #151c24;
      border-bottom: 1px solid var(--border);
      color: var(--muted);
      font-size: 11px;
    }
    .legend {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      align-items: center;
      padding: 8px 10px;
      border-bottom: 1px solid var(--border);
      background: #111922;
      color: #b8c7d8;
      font-size: 11px;
    }
    .legend.hidden { display: none; }
    .legendItem {
      display: inline-flex;
      align-items: center;
      gap: 6px;
    }
    .legendSwatch {
      width: 10px;
      height: 10px;
      border-radius: 3px;
      border: 1px solid transparent;
    }
    .legendSwatch.proc { background: #5a3e26; border-color: #e39f5b; }
    .legendSwatch.decl { background: #214e45; border-color: #6fd8bf; }
    .legendSwatch.stmt { background: #5b4e1f; border-color: #f4c26b; }
    .legendSwatch.expr { background: #233c61; border-color: #7fb0ee; }
    .legendSwatch.type { background: #4b3f24; border-color: #dfc06d; }
    .legendSwatch.struct { background: #313744; border-color: #8fa0b7; }
    .graphWrap {
      position: relative;
      min-height: 260px;
      max-height: calc(100vh - 220px);
      overflow: auto;
      background:
        radial-gradient(700px 400px at 2% -15%, rgba(72, 176, 106, 0.12), rgba(15, 20, 26, 0) 50%),
        linear-gradient(180deg, #0f141a 0%, #111821 100%);
    }
    .graphMsg {
      position: absolute;
      top: 10px;
      left: 10px;
      right: 10px;
      border: 1px solid #3d4b5b;
      border-radius: 8px;
      background: #1b2430;
      color: #c5d3e2;
      font-size: 12px;
      padding: 8px 10px;
      display: none;
      pointer-events: none;
    }
    svg {
      display: block;
      min-width: 100%;
    }
    .raw {
      margin-top: 10px;
      border: 1px solid var(--border);
      border-radius: 10px;
      overflow: hidden;
      background: #121922;
    }
    .raw summary {
      cursor: pointer;
      padding: 8px 10px;
      color: var(--muted);
      font-size: 12px;
      border-bottom: 1px solid var(--border);
      user-select: none;
    }
    pre {
      margin: 0;
      padding: 12px;
      min-height: 120px;
      max-height: 260px;
      overflow: auto;
      white-space: pre;
      line-height: 1.35;
      font-size: 12px;
      color: #d9e6f2;
    }
  </style>
</head>
<body>
  <div class="wrap">
    <div class="header">
      <div>
        <div class="title">Jovial Syntax Trees - ${escapeHtml(fileLabel)}</div>
        <div class="meta">${escapeHtml(uri)}</div>
      </div>
      <div class="actions">
        <button id="tab-ast" class="${astActive ? "active" : ""}">AST</button>
        <button id="tab-cst" class="${astActive ? "" : "active"}">CST</button>
        <button id="refresh">Refresh</button>
      </div>
    </div>
    <div class="viewer">
      <div class="bar">
        <span>${escapeHtml(shownTitle)}</span>
        <span>${astActive ? "Click a node to jump to source" : "Token flow order"}</span>
      </div>
      <div class="legend ${astActive ? "" : "hidden"}">
        <span class="legendItem"><span class="legendSwatch proc"></span>Procedure</span>
        <span class="legendItem"><span class="legendSwatch decl"></span>Declaration</span>
        <span class="legendItem"><span class="legendSwatch stmt"></span>Statement</span>
        <span class="legendItem"><span class="legendSwatch expr"></span>Expression</span>
        <span class="legendItem"><span class="legendSwatch type"></span>Type</span>
        <span class="legendItem"><span class="legendSwatch struct"></span>Structure</span>
      </div>
      <div class="graphWrap">
        <svg id="graphSvg" xmlns="http://www.w3.org/2000/svg"></svg>
        <div id="graphMsg" class="graphMsg"></div>
      </div>
    </div>
    <details class="raw">
      <summary>Raw ${escapeHtml(shownTitle)}</summary>
      <pre id="rawDump">${escapeHtml(shown)}</pre>
    </details>
  </div>
  <script>
    const vscode = acquireVsCodeApi();
    const payload = {
      tab: ${JSON.stringify(activeTab)},
      uri: ${JSON.stringify(uri)},
      ast: ${astPayload},
      cst: ${cstPayload},
    };

    const svg = document.getElementById("graphSvg");
    const msgBox = document.getElementById("graphMsg");

    function shorten(s, n) {
      if (s.length <= n) return s;
      return s.slice(0, Math.max(1, n - 3)) + "...";
    }

    function parseLocationPrefix(text, from) {
      const m = text.slice(from).match(/^(.+):(\\d+):(\\d+)-(\\d+):(\\d+)/);
      if (!m) return null;
      const sl = Number(m[2]);
      const sc = Number(m[3]);
      const el = Number(m[4]);
      const ec = Number(m[5]);
      if (![sl, sc, el, ec].every((n) => Number.isFinite(n))) return null;
      return {
        text: m[0],
        length: m[0].length,
        loc: {
          file: m[1],
          startLine: Math.trunc(sl),
          startCol: Math.trunc(sc),
          endLine: Math.trunc(el),
          endCol: Math.trunc(ec),
        },
      };
    }

    function parseLocationText(text) {
      if (!text) return null;
      const m = String(text).trim().match(/^(.+):(\\d+):(\\d+)-(\\d+):(\\d+)$/);
      if (!m) return null;
      const sl = Number(m[2]);
      const sc = Number(m[3]);
      const el = Number(m[4]);
      const ec = Number(m[5]);
      if (![sl, sc, el, ec].every((n) => Number.isFinite(n))) return null;
      return {
        file: m[1],
        startLine: Math.trunc(sl),
        startCol: Math.trunc(sc),
        endLine: Math.trunc(el),
        endCol: Math.trunc(ec),
      };
    }

    function astNodeKind(label) {
      const headMatch = String(label).match(/^[A-Za-z][A-Za-z0-9_]*/);
      const head = headMatch ? headMatch[0] : "";
      if (head === "Program" || head === "TopDecl" || head === "TopStmt" || head === "Param" || head === "Field") {
        return "struct";
      }
      if (head === "DProc") return "proc";
      if (/^D[A-Za-z]/.test(head)) return "decl";
      if (/^S[A-Za-z]/.test(head)) return "stmt";
      if (/^E[A-Za-z]/.test(head)) return "expr";
      if (/^T[A-Za-z]/.test(head)) return "type";
      return "other";
    }

    function nodeColor(node, isFlow) {
      if (node.id === 0) {
        return { fill: "#244f3f", stroke: "#53bf78", text: "#dcfee7" };
      }
      if (isFlow || node.kind === "token") {
        return { fill: "#23344a", stroke: "#5f86ad", text: "#dce8f5" };
      }
      switch (node.kind) {
        case "proc":
          return { fill: "#5a3e26", stroke: "#e39f5b", text: "#ffe8cb" };
        case "decl":
          return { fill: "#214e45", stroke: "#6fd8bf", text: "#d7fff4" };
        case "stmt":
          return { fill: "#5b4e1f", stroke: "#f4c26b", text: "#fff0c8" };
        case "expr":
          return { fill: "#233c61", stroke: "#7fb0ee", text: "#d9ebff" };
        case "type":
          return { fill: "#4b3f24", stroke: "#dfc06d", text: "#fff0c8" };
        case "struct":
          return { fill: "#313744", stroke: "#8fa0b7", text: "#e3e8ef" };
        default:
          return { fill: "#2a3140", stroke: "#67758a", text: "#dce8f5" };
      }
    }

    function showMessage(msg) {
      if (!msgBox) return;
      if (!msg) {
        msgBox.style.display = "none";
        msgBox.textContent = "";
        return;
      }
      msgBox.textContent = msg;
      msgBox.style.display = "block";
    }

    function parseAstGraph(text, maxNodes = 520) {
      const nodes = [{ id: 0, label: "AST", kind: "root", depth: 0 }];
      const edges = [];
      const stack = [0];
      let pending = "";
      let truncated = false;
      let recentClosed = null;

      const isWordChar = (ch) => /[A-Za-z0-9_'$<>.-]/.test(ch);
      const pushNode = (label) => {
        if (nodes.length >= maxNodes) {
          truncated = true;
          return null;
        }
        const id = nodes.length;
        nodes.push({ id, label, kind: astNodeKind(label), depth: stack.length });
        edges.push({ from: stack[stack.length - 1], to: id });
        return id;
      };

      let i = 0;
      while (i < text.length) {
        if (recentClosed !== null) {
          let j = i;
          while (j < text.length && /\\s/.test(text[j])) j += 1;
          const parsedLoc = parseLocationPrefix(text, j);
          if (parsedLoc) {
            const n = nodes[recentClosed];
            if (n && !n.loc) {
              n.meta = parsedLoc.text;
              n.loc = parsedLoc.loc;
            }
            i = j + parsedLoc.length;
            recentClosed = null;
            continue;
          }
          recentClosed = null;
          i = j;
          if (i >= text.length) break;
        }

        const ch = text[i];
        if (/\\s/.test(ch)) {
          i += 1;
          continue;
        }
        if (ch === '"') {
          i += 1;
          while (i < text.length) {
            const c = text[i];
            if (c === "\\\\") {
              i += 2;
              continue;
            }
            if (c === '"') {
              i += 1;
              break;
            }
            i += 1;
          }
          pending = "";
          continue;
        }
        if (isWordChar(ch)) {
          let j = i + 1;
          while (j < text.length && isWordChar(text[j])) j += 1;
          pending = text.slice(i, j);
          i = j;
          continue;
        }
        if (ch === "(" || ch === "[" || ch === "{") {
          const label = pending || (ch === "[" ? "List" : "Group");
          pending = "";
          const id = pushNode(label);
          if (id !== null) stack.push(id);
          i += 1;
          if (truncated) break;
          continue;
        }
        if (ch === ")" || ch === "]" || ch === "}") {
          pending = "";
          if (stack.length > 1) recentClosed = stack.pop();
          i += 1;
          continue;
        }
        pending = "";
        i += 1;
      }

      return { nodes, edges, truncated };
    }

    function parseCstGraph(text, maxTokens = 260) {
      const nodes = [{ id: 0, label: "CST", kind: "root", depth: 0 }];
      const edges = [];
      let truncated = false;
      let prev = 0;
      const lines = text.split(/\\r?\\n/);
      const tokenRe = /^\\s*(\\d+)\\s+(\\S+)\\s+(".*?")\\s+@\\s+(\\d+:\\d+-\\d+:\\d+)/;

      for (const line of lines) {
        if (line.startsWith("CST (token stream)")) continue;
        const m = line.match(tokenRe);
        if (!m) continue;
        if (nodes.length >= maxTokens + 1) {
          truncated = true;
          break;
        }
        const tok = m[2];
        const lex = m[3];
        const loc = m[4];
        const parsed = parseLocationText(loc);
        const id = nodes.length;
        nodes.push({
          id,
          label: tok + "\\n" + lex,
          kind: "token",
          meta: loc,
          loc: parsed || undefined,
          depth: 1,
        });
        edges.push({ from: prev, to: id });
        prev = id;
      }

      return { nodes, edges, truncated };
    }

    function layoutTree(graph) {
      const kids = new Map();
      for (const e of graph.edges) {
        const arr = kids.get(e.from) || [];
        arr.push(e.to);
        kids.set(e.from, arr);
      }
      const pos = new Map();
      let nextX = 0;
      let maxDepth = 0;

      const walk = (id, depth) => {
        maxDepth = Math.max(maxDepth, depth);
        const children = kids.get(id) || [];
        if (children.length === 0) {
          pos.set(id, { x: nextX, y: depth });
          nextX += 1;
          return;
        }
        for (const child of children) walk(child, depth + 1);
        const first = pos.get(children[0]);
        const last = pos.get(children[children.length - 1]);
        pos.set(id, { x: (first.x + last.x) / 2, y: depth });
      };

      walk(0, 0);
      return { pos, maxDepth, span: Math.max(nextX, 1) };
    }

    function layoutFlow(graph) {
      const cols = 8;
      const pos = new Map();
      pos.set(0, { x: (cols - 1) / 2, y: 0 });
      for (let i = 1; i < graph.nodes.length; i += 1) {
        const n = i - 1;
        const row = Math.floor(n / cols);
        let col = n % cols;
        if (row % 2 === 1) col = cols - 1 - col;
        pos.set(i, { x: col, y: row + 1 });
      }
      const rows = Math.floor((Math.max(0, graph.nodes.length - 2)) / cols) + 2;
      return { pos, maxDepth: rows, span: cols };
    }

    function drawGraph(graph, isFlow) {
      if (!svg) return;
      while (svg.firstChild) svg.removeChild(svg.firstChild);

      if (!graph || !graph.nodes || graph.nodes.length <= 1) {
        showMessage("Could not extract graph nodes from this dump. Use the raw output below.");
        return;
      }

      const layout = isFlow ? layoutFlow(graph) : layoutTree(graph);
      const xGap = isFlow ? 190 : 160;
      const yGap = isFlow ? 120 : 96;
      const pad = 48;
      const nodeW = 152;
      const nodeH = 54;

      const toPixel = (p) => ({
        x: pad + p.x * xGap,
        y: pad + p.y * yGap,
      });

      let maxX = 0;
      let maxY = 0;
      for (const n of graph.nodes) {
        const p = toPixel(layout.pos.get(n.id));
        maxX = Math.max(maxX, p.x);
        maxY = Math.max(maxY, p.y);
      }
      const width = maxX + pad + nodeW;
      const height = maxY + pad + nodeH;
      svg.setAttribute("width", String(width));
      svg.setAttribute("height", String(height));
      svg.setAttribute("viewBox", "0 0 " + width + " " + height);

      const make = (name) => document.createElementNS("http://www.w3.org/2000/svg", name);

      for (const e of graph.edges) {
        const a = toPixel(layout.pos.get(e.from));
        const b = toPixel(layout.pos.get(e.to));
        const path = make("path");
        const x1 = a.x + nodeW / 2;
        const y1 = a.y + nodeH;
        const x2 = b.x + nodeW / 2;
        const y2 = b.y;
        const cy = (y1 + y2) / 2;
        path.setAttribute("d", "M " + x1 + " " + y1 + " C " + x1 + " " + cy + " " + x2 + " " + cy + " " + x2 + " " + y2);
        path.setAttribute("fill", "none");
        path.setAttribute("stroke", isFlow ? "#6f8eab" : "#587993");
        path.setAttribute("stroke-width", "1.5");
        path.setAttribute("opacity", "0.75");
        svg.appendChild(path);
      }

      for (const n of graph.nodes) {
        const p = toPixel(layout.pos.get(n.id));
        const g = make("g");
        const rect = make("rect");
        rect.setAttribute("x", String(p.x));
        rect.setAttribute("y", String(p.y));
        rect.setAttribute("width", String(nodeW));
        rect.setAttribute("height", String(nodeH));
        rect.setAttribute("rx", "11");
        const colors = nodeColor(n, isFlow);
        rect.setAttribute("fill", colors.fill);
        rect.setAttribute("stroke", colors.stroke);
        rect.setAttribute("stroke-width", "1.2");
        g.appendChild(rect);

        const title = make("title");
        title.textContent = n.meta ? n.label + " @ " + n.meta : n.label;
        if (n.loc) {
          title.textContent += " (click to open source)";
        }
        g.appendChild(title);

        const textEl = make("text");
        textEl.setAttribute("x", String(p.x + nodeW / 2));
        textEl.setAttribute("y", String(p.y + 23));
        textEl.setAttribute("fill", colors.text);
        textEl.setAttribute("font-size", "11");
        textEl.setAttribute("font-family", "Consolas, 'Cascadia Code', monospace");
        textEl.setAttribute("text-anchor", "middle");

        const lines = String(n.label)
          .split("\\n")
          .slice(0, 2)
          .map((x) => shorten(x, 26));
        lines.forEach((line, i) => {
          const tspan = make("tspan");
          tspan.setAttribute("x", String(p.x + nodeW / 2));
          tspan.setAttribute("dy", i === 0 ? "0" : "13");
          tspan.textContent = line;
          textEl.appendChild(tspan);
        });
        g.appendChild(textEl);

        if (n.loc) {
          g.style.cursor = "pointer";
          g.addEventListener("mouseenter", () => rect.setAttribute("stroke-width", "2"));
          g.addEventListener("mouseleave", () => rect.setAttribute("stroke-width", "1.2"));
          g.addEventListener("click", () => {
            vscode.postMessage({
              type: "goto",
              uri: payload.uri,
              loc: n.loc,
              label: n.label,
            });
          });
        }
        svg.appendChild(g);
      }

      if (graph.truncated) {
        showMessage("Graph was truncated for performance. Raw output below still has full data.");
      } else {
        showMessage("");
      }
    }

    function renderGraphFromPayload() {
      const isAst = payload.tab === "ast";
      const text = isAst ? payload.ast : payload.cst;
      try {
        const graph = isAst ? parseAstGraph(text) : parseCstGraph(text);
        drawGraph(graph, !isAst);
      } catch (err) {
        showMessage("Graph render failed. Showing raw output below.");
      }
    }

    renderGraphFromPayload();

    document.getElementById("refresh")?.addEventListener("click", () => {
      vscode.postMessage({ type: "refresh" });
    });
    document.getElementById("tab-ast")?.addEventListener("click", () => {
      vscode.postMessage({ type: "tab", value: "ast" });
    });
    document.getElementById("tab-cst")?.addEventListener("click", () => {
      vscode.postMessage({ type: "tab", value: "cst" });
    });
  </script>
</body>
</html>`;
}

async function showSyntaxTreesUi(output: vscode.OutputChannel) {
  if (!client) {
    vscode.window.showWarningMessage("Jovial LSP is not running.");
    return;
  }
  const editor = vscode.window.activeTextEditor;
  if (!editor) return;

  const sourceUri = editor.document.uri;
  const docUri = editor.document.uri.toString();
  const fileLabel = path.basename(editor.document.fileName || editor.document.uri.path || "document");
  const panel = vscode.window.createWebviewPanel(
    "jovialSyntaxTrees",
    `Jovial Trees: ${fileLabel}`,
    vscode.ViewColumn.Beside,
    { enableScripts: true, retainContextWhenHidden: true }
  );

  let activeTab: "ast" | "cst" = "ast";
  let astText = "Loading AST...";
  let cstText = "Loading CST...";

  const render = () => {
    panel.webview.html = syntaxTreeHtml(fileLabel, docUri, astText, cstText, activeTab);
  };

  const refresh = async () => {
    try {
      const [astRes, cstRes] = await Promise.all([
        executeServerCommand("jovial.dumpAst", [docUri]),
        executeServerCommand("jovial.dumpCst", [docUri]),
      ]);
      astText = toDisplayText(astRes, "No AST available.");
      cstText = toDisplayText(cstRes, "No CST available.");
    } catch (e) {
      const msg = `Failed to fetch syntax trees: ${String(e)}`;
      output.appendLine(msg);
      astText = msg;
      cstText = msg;
    }
    render();
  };

  type GraphLoc = {
    file?: string;
    startLine: number;
    startCol: number;
    endLine: number;
    endCol: number;
  };

  const parseGraphLoc = (value: unknown): GraphLoc | null => {
    if (!value || typeof value !== "object") return null;
    const rec = value as Record<string, unknown>;
    const num = (key: string): number | null => {
      const raw = rec[key];
      if (typeof raw !== "number" || !Number.isFinite(raw)) return null;
      return Math.trunc(raw);
    };
    const startLine = num("startLine");
    const startCol = num("startCol");
    const endLine = num("endLine");
    const endCol = num("endCol");
    if (startLine === null || startCol === null || endLine === null || endCol === null) return null;
    const rawFile = rec["file"];
    return {
      file: typeof rawFile === "string" ? rawFile : undefined,
      startLine,
      startCol,
      endLine,
      endCol,
    };
  };

  const resolveLocUri = (loc: GraphLoc): vscode.Uri => {
    const file = (loc.file ?? "").trim();
    if (!file || file === "<nofile>") return sourceUri;

    if (/^[A-Za-z]:[\\/]/.test(file) || file.startsWith("\\\\")) {
      return vscode.Uri.file(file);
    }
    if (/^[a-zA-Z][a-zA-Z0-9+.-]*:/.test(file)) {
      try {
        return vscode.Uri.parse(file);
      } catch {
        // fall through to relative resolution
      }
    }

    const baseDir = sourceUri.fsPath ? path.dirname(sourceUri.fsPath) : "";
    if (baseDir) {
      return vscode.Uri.file(path.resolve(baseDir, file));
    }
    return sourceUri;
  };

  const clamp = (n: number, lo: number, hi: number): number => Math.min(hi, Math.max(lo, n));

  const docPos = (doc: vscode.TextDocument, line1: number, col0: number): vscode.Position => {
    const maxLine = Math.max(0, doc.lineCount - 1);
    const line = clamp(line1 - 1, 0, maxLine);
    const lineLen = doc.lineAt(line).text.length;
    const col = clamp(col0, 0, lineLen);
    return new vscode.Position(line, col);
  };

  const jumpToGraphLoc = async (value: unknown): Promise<void> => {
    const loc = parseGraphLoc(value);
    if (!loc) return;
    try {
      const targetUri = resolveLocUri(loc);
      const targetDoc = await vscode.workspace.openTextDocument(targetUri);
      const start = docPos(targetDoc, Math.max(1, loc.startLine), Math.max(0, loc.startCol));
      let end = docPos(targetDoc, Math.max(1, loc.endLine), Math.max(0, loc.endCol));
      if (end.isBefore(start)) end = start;
      const column = vscode.window.activeTextEditor?.viewColumn ?? vscode.ViewColumn.One;
      const targetEditor = await vscode.window.showTextDocument(targetDoc, { preview: false, viewColumn: column });
      const range = new vscode.Range(start, end);
      targetEditor.selection = new vscode.Selection(start, end);
      targetEditor.revealRange(range, vscode.TextEditorRevealType.InCenterIfOutsideViewport);
    } catch (e) {
      output.appendLine(`AST/CST goto failed: ${String(e)}`);
    }
  };

  panel.webview.onDidReceiveMessage(
    async (msg) => {
      if (!msg || typeof msg !== "object") return;
      const kind = (msg as { type?: string }).type;
      if (kind === "refresh") {
        await refresh();
        return;
      }
      if (kind === "goto") {
        await jumpToGraphLoc((msg as { loc?: unknown }).loc);
        return;
      }
      if (kind === "tab") {
        const value = (msg as { value?: string }).value;
        if (value === "ast" || value === "cst") {
          activeTab = value;
          render();
        }
      }
    },
    undefined
  );

  render();
  await refresh();
}

export async function activate(context: vscode.ExtensionContext) {
  const output = vscode.window.createOutputChannel("Jovial LSP");
  context.subscriptions.push(output);

  const status = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
  status.command = "jovial.restartServer";
  status.show();
  context.subscriptions.push(status);
  setStatus(status, "stopped", "Click to start / restart Jovial LSP");

  // UI command (renamed) — does NOT collide with languageclient’s auto registration
  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.dumpAstUi", async () => {
      try {
        await dumpAstUi(output);
      } catch (e) {
        output.appendLine(`dumpAstUi failed: ${String(e)}`);
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.dumpCstUi", async () => {
      try {
        await dumpCstUi(output);
      } catch (e) {
        output.appendLine(`dumpCstUi failed: ${String(e)}`);
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.showSyntaxTrees", async () => {
      try {
        await showSyntaxTreesUi(output);
      } catch (e) {
        output.appendLine(`showSyntaxTrees failed: ${String(e)}`);
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.restartServer", async () => {
      try {
        await startClient(context, output, status);
      } catch (e) {
        output.appendLine(`restart failed: ${String(e)}`);
        setStatus(status, "error", `restart failed: ${String(e)}`);
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.refreshLsifCache", async () => {
      try {
        if (!getConfig().lsifFastPath) {
          vscode.window.showInformationMessage(
            "Jovial: LSIF fast path is disabled. Enable jovial.lsif.fastPath to refresh LSIF cache."
          );
          return;
        }
        await refreshLsifIndex(output, "manual command", firstPreferredLsifUri());
        vscode.window.showInformationMessage("Jovial: LSIF cache refresh completed.");
      } catch (e) {
        output.appendLine(`refreshLsifCache failed: ${String(e)}`);
        vscode.window.showWarningMessage(
          "Jovial: LSIF cache refresh failed. Check the Jovial LSP output channel."
        );
      }
    })
  );

  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration(async (event) => {
      if (!event.affectsConfiguration("jovial")) return;
      const cfg = getConfig();

      if (event.affectsConfiguration("jovial.trace") && client) {
        applyTraceSetting(output);
      }
      if (event.affectsConfiguration("jovial.lsif.fastPath")) {
        if (cfg.lsifFastPath) {
          output.appendLine(
            "LSIF fast path enabled. Use command 'Jovial: Refresh LSIF Cache' to build the cache."
          );
        } else {
          resetLsifState();
        }
      }

      const serverConfigChanged =
        event.affectsConfiguration("jovial.server.path") ||
        event.affectsConfiguration("jovial.server.preferBundled") ||
        event.affectsConfiguration("jovial.server.args") ||
        event.affectsConfiguration("jovial.workspaceDiagnostics.mode") ||
        event.affectsConfiguration("jovial.background.indexBudgetMs") ||
        event.affectsConfiguration("jovial.background.diagBatchSize");

      if (serverConfigChanged) {
        output.appendLine("Jovial server configuration changed; restarting server.");
        if (client || cfg.autostart) {
          await startClient(context, output, status, "config-change");
        }
        return;
      }

      if (event.affectsConfiguration("jovial.autostart")) {
        if (cfg.autostart) {
          await startClient(context, output, status, "config-change");
        } else {
          await stopClient(status);
        }
      }
    })
  );

  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument((doc) => {
      if (doc.languageId !== "jovial") return;
      // Refresh on save only; avoid refresh storms during open/edit/navigation.
      scheduleLsifRefresh(output, "didSave", doc.uri, 1000);
    })
  );

  context.subscriptions.push({ dispose: () => { void stopClient(status); } });

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
