import * as vscode from "vscode";
import * as cp from "child_process";
import * as fs from "fs";
import * as path from "path";

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

function getConfig() {
  const cfg = vscode.workspace.getConfiguration("jovial");
  return {
    serverPath: cfg.get<string>("server.path", ""),
    serverArgs: cfg.get<string[]>("server.args", []),
    autostart: cfg.get<boolean>("autostart", true),
  };
}

function resolveServerPath(context: vscode.ExtensionContext, configured: string): string | undefined {
  const cfg = (configured ?? "").trim();
  const wsRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;

  if (cfg.length > 0) {
    const expanded = wsRoot ? cfg.replace("${workspaceFolder}", wsRoot) : cfg;
    if (path.isAbsolute(expanded)) return expanded;
    if (wsRoot) return path.join(wsRoot, expanded);
    return context.asAbsolutePath(expanded);
  }

  const exe = process.platform === "win32" ? "jovial-lsp.exe" : "jovial-lsp";
  const bundled = context.asAbsolutePath(path.join("server", exe));
  if (fs.existsSync(bundled)) return bundled;

  return undefined;
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
  fileWatcher = vscode.workspace.createFileSystemWatcher("**/*.{jov,j73,jvl,j}");
  context.subscriptions.push(fileWatcher);

  const serverOptions = async (): Promise<StreamInfo> => {
    const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;

    const child = cp.spawn(serverPath, cfg.serverArgs, {
      cwd: workspaceRoot,
      env: { ...process.env },
      windowsHide: true,
      stdio: ["pipe", "pipe", "pipe"],
    });

    child.stderr.setEncoding("utf8");
    child.stderr.on("data", (chunk: string) => output.appendLine(chunk.toString()));

    child.on("exit", (code, signal) => {
      output.appendLine(`Jovial LSP exited: code=${code} signal=${signal}`);
      setStatus(status, "stopped", `Exited: code=${code} signal=${signal}`);
    });

    child.on("error", (err) => {
      output.appendLine(`Failed to start Jovial LSP: ${String(err)}`);
      setStatus(status, "error", `Failed to start: ${String(err)}`);
    });

    return { reader: child.stdout, writer: child.stdin };
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "jovial" },
      { scheme: "untitled", language: "jovial" },
    ],
    synchronize: {
      fileEvents: fileWatcher,
    },
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
        <span>Node Graph View</span>
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
      ast: ${astPayload},
      cst: ${cstPayload},
    };

    const svg = document.getElementById("graphSvg");
    const msgBox = document.getElementById("graphMsg");

    function shorten(s, n) {
      if (s.length <= n) return s;
      return s.slice(0, Math.max(1, n - 3)) + "...";
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
      const nodes = [{ id: 0, label: "AST", depth: 0 }];
      const edges = [];
      const stack = [0];
      let pending = "";
      let truncated = false;

      const isWordChar = (ch) => /[A-Za-z0-9_'$<>.-]/.test(ch);
      const pushNode = (label) => {
        if (nodes.length >= maxNodes) {
          truncated = true;
          return null;
        }
        const id = nodes.length;
        nodes.push({ id, label, depth: stack.length });
        edges.push({ from: stack[stack.length - 1], to: id });
        return id;
      };

      let i = 0;
      while (i < text.length) {
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
          if (stack.length > 1) stack.pop();
          i += 1;
          continue;
        }
        pending = "";
        i += 1;
      }

      return { nodes, edges, truncated };
    }

    function parseCstGraph(text, maxTokens = 260) {
      const nodes = [{ id: 0, label: "CST", depth: 0 }];
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
        const id = nodes.length;
        nodes.push({ id, label: tok + "\\n" + lex, meta: loc, depth: 1 });
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
        rect.setAttribute("fill", n.id === 0 ? "#244f3f" : (isFlow ? "#23344a" : "#1f2e40"));
        rect.setAttribute("stroke", n.id === 0 ? "#53bf78" : "#40607b");
        rect.setAttribute("stroke-width", "1.2");
        g.appendChild(rect);

        const title = make("title");
        title.textContent = n.meta ? n.label + " @ " + n.meta : n.label;
        g.appendChild(title);

        const textEl = make("text");
        textEl.setAttribute("x", String(p.x + nodeW / 2));
        textEl.setAttribute("y", String(p.y + 23));
        textEl.setAttribute("fill", "#dce8f5");
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

  panel.webview.onDidReceiveMessage(
    async (msg) => {
      if (!msg || typeof msg !== "object") return;
      const kind = (msg as { type?: string }).type;
      if (kind === "refresh") {
        await refresh();
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
