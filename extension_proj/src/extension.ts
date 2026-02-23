import * as vscode from "vscode";
import * as cp from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";

import {
  LanguageClient,
  LanguageClientOptions,
  CloseAction,
  ErrorAction,
  ExecuteCommandRequest,
  ExecuteCommandParams,
  StreamInfo,
} from "vscode-languageclient/node";

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

let pendingWatchedFileChanges = new Map<string, PendingWatchedFileChange>();
let pendingWatchedFileFlushTimer: NodeJS.Timeout | undefined;
let watchedFileFlushInFlight = false;

function watchPathKey(fsPath: string): string {
  const norm = path.normalize(fsPath);
  return process.platform === "win32" ? norm.toLowerCase() : norm;
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
  try {
    while (client && pendingWatchedFileChanges.size > 0) {
      const batch = takeWatchedFileBatch(WATCH_CHUNK_SIZE);
      if (batch.length === 0) break;
      const params = {
        changes: batch.map((c) => ({
          uri: vscode.Uri.file(c.fsPath).toString(),
          type: c.type,
        })),
      };
      await client.sendNotification("workspace/didChangeWatchedFiles", params);
    }
  } catch (e) {
    output.appendLine(`Watched-file flush failed: ${String(e)}`);
  } finally {
    watchedFileFlushInFlight = false;
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
  return {
    serverPath: cfg.get<string>("server.path", ""),
    serverArgs: cfg.get<string[]>("server.args", []),
    autostart: cfg.get<boolean>("autostart", true),
  };
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

function isRepoRootDir(dir: string): boolean {
  const hasGit = fs.existsSync(path.join(dir, ".git"));
  const hasServer = fs.existsSync(path.join(dir, "server_proj"));
  const hasExtension = fs.existsSync(path.join(dir, "extension_proj"));
  return hasGit || (hasServer && hasExtension);
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

function resolveServerPath(context: vscode.ExtensionContext, configured: string): string | undefined {
  const cfgRaw = stripWrappingQuotes((configured ?? "").trim());
  const folders = vscode.workspace.workspaceFolders ?? [];
  const repoRoot = findRepoRoot(folders, context.extensionPath);

  if (cfgRaw.length > 0) {
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
    const hit = ordered.find((p) => fs.existsSync(p));
    return normalizeServerPathForPlatform(hit ?? ordered[0]);
  }

  const exes = process.platform === "win32"
    ? ["Main.exe", "jovial-lsp.exe"]
    : ["Main", "jovial-lsp"];
  const relCandidates = [
    ...exes.map((e) => path.join("server", e)),
    ...exes.map((e) => path.join("server_proj", "_build", "default", "bin", e)),
    ...exes.map((e) => path.join("server_proj", "_build", "install", "default", "bin", e)),
  ];

  const probe: string[] = [];
  for (const rel of relCandidates) {
    if (repoRoot) {
      pushCandidate(probe, path.join(repoRoot, rel));
    }
    for (const f of folders) {
      addRelativeCandidates(probe, f.uri.fsPath, rel, 4);
    }
    addRelativeCandidates(probe, context.extensionPath, rel, 2);
    pushCandidate(probe, context.asAbsolutePath(rel));
  }
  const hit = uniquePaths(probe).find((p) => fs.existsSync(p));
  return hit ? normalizeServerPathForPlatform(hit) : undefined;
}

function setStatus(
  status: vscode.StatusBarItem,
  kind: "starting" | "running" | "stopped" | "error",
  detail?: string
) {
  switch (kind) {
    case "starting":
      status.text = `$(sync~spin) Jovial LSP: starting…`;
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

async function startClient(
  context: vscode.ExtensionContext,
  output: vscode.OutputChannel,
  status: vscode.StatusBarItem
) {
  const cfg = getConfig();
  const serverPath = resolveServerPath(context, cfg.serverPath);

  output.appendLine(`Resolved server path: ${serverPath ?? "<none>"}`);

  if (!serverPath || !fs.existsSync(serverPath)) {
    setStatus(status, "error", "Server executable not found. Set jovial.server.path.");
    vscode.window.showErrorMessage(
      "Jovial LSP: server executable not found. Set jovial.server.path in Settings (can be relative to workspace)."
    );
    return;
  }

  setStatus(status, "starting", `Starting: ${serverPath}`);

  if (client) {
    await stopClient(status);
  }

  output.appendLine(`Starting Jovial LSP: ${serverPath}`);

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
      env: { ...process.env },
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
    errorHandler: {
      error: (error, message, count) => {
        output.appendLine(`Client error (${count ?? 0}): ${message ?? ""} ${String(error)}`);
        return { action: ErrorAction.Continue };
      },
      closed: () => {
        output.appendLine("Client closed: not restarting automatically.");
        setStatus(status, "stopped", "Client closed (not restarting automatically).");
        return { action: CloseAction.DoNotRestart };
      },
    },
  };

  // IMPORTANT: keep the client id stable; VS Code uses it for tracing settings keys.
  client = new LanguageClient("jovialLsp", "Jovial Language Server", serverOptions, clientOptions);

  try {
    await client.start();
    await flushWatchedFileChanges(output);
    output.appendLine("Jovial LSP client started.");
    setStatus(status, "running", `Server: ${serverPath}`);
  } catch (e) {
    output.appendLine(`Client failed to start: ${String(e)}`);
    setStatus(status, "error", `Client failed to start: ${String(e)}`);
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

  context.subscriptions.push({ dispose: () => { void stopClient(status); } });

  if (getConfig().autostart) {
    await startClient(context, output, status);
  }
}

export async function deactivate() {
  if (client) await client.stop();
}
