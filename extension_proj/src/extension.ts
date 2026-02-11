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
