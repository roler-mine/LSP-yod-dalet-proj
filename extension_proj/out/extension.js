"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const vscode = __importStar(require("vscode"));
const cp = __importStar(require("child_process"));
const fs = __importStar(require("fs"));
const path = __importStar(require("path"));
const node_1 = require("vscode-languageclient/node");
let client;
function getConfig() {
    const cfg = vscode.workspace.getConfiguration("jovial");
    return {
        serverPath: cfg.get("server.path", ""),
        serverArgs: cfg.get("server.args", []),
        autostart: cfg.get("autostart", true),
    };
}
function resolveServerPath(context, configured) {
    const cfg = (configured ?? "").trim();
    const wsRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
    if (cfg.length > 0) {
        const expanded = wsRoot ? cfg.replace("${workspaceFolder}", wsRoot) : cfg;
        if (path.isAbsolute(expanded))
            return expanded;
        if (wsRoot)
            return path.join(wsRoot, expanded);
        return context.asAbsolutePath(expanded);
    }
    const exe = process.platform === "win32" ? "jovial-lsp.exe" : "jovial-lsp";
    const bundled = context.asAbsolutePath(path.join("server", exe));
    if (fs.existsSync(bundled))
        return bundled;
    return undefined;
}
function setStatus(status, kind, detail) {
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
async function stopClient(status) {
    if (!client) {
        setStatus(status, "stopped");
        return;
    }
    const c = client;
    client = undefined;
    try {
        await c.stop();
    }
    finally {
        setStatus(status, "stopped");
    }
}
async function startClient(context, output, status) {
    const cfg = getConfig();
    const serverPath = resolveServerPath(context, cfg.serverPath);
    output.appendLine(`Resolved server path: ${serverPath ?? "<none>"}`);
    if (!serverPath || !fs.existsSync(serverPath)) {
        setStatus(status, "error", "Server executable not found. Set jovial.server.path.");
        vscode.window.showErrorMessage("Jovial LSP: server executable not found. Set jovial.server.path in Settings (can be relative to workspace).");
        return;
    }
    setStatus(status, "starting", `Starting: ${serverPath}`);
    if (client) {
        await stopClient(status);
    }
    output.appendLine(`Starting Jovial LSP: ${serverPath}`);
    const serverOptions = async () => {
        const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
        const child = cp.spawn(serverPath, cfg.serverArgs, {
            cwd: workspaceRoot,
            env: { ...process.env },
            windowsHide: true,
            stdio: ["pipe", "pipe", "pipe"],
        });
        child.stderr.setEncoding("utf8");
        child.stderr.on("data", (chunk) => output.appendLine(chunk.toString()));
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
    const clientOptions = {
        documentSelector: [
            { scheme: "file", language: "jovial" },
            { scheme: "untitled", language: "jovial" },
        ],
        outputChannel: output,
        errorHandler: {
            error: (error, message, count) => {
                output.appendLine(`Client error (${count ?? 0}): ${message ?? ""} ${String(error)}`);
                return { action: node_1.ErrorAction.Continue };
            },
            closed: () => {
                output.appendLine("Client closed: not restarting automatically.");
                setStatus(status, "stopped", "Client closed (not restarting automatically).");
                return { action: node_1.CloseAction.DoNotRestart };
            },
        },
    };
    // IMPORTANT: keep the client id stable; VS Code uses it for tracing settings keys.
    client = new node_1.LanguageClient("jovialLsp", "Jovial Language Server", serverOptions, clientOptions);
    try {
        await client.start();
        output.appendLine("Jovial LSP client started.");
        setStatus(status, "running", `Server: ${serverPath}`);
    }
    catch (e) {
        output.appendLine(`Client failed to start: ${String(e)}`);
        setStatus(status, "error", `Client failed to start: ${String(e)}`);
    }
}
async function dumpAstUi(output) {
    if (!client) {
        vscode.window.showWarningMessage("Jovial LSP is not running.");
        return;
    }
    const editor = vscode.window.activeTextEditor;
    if (!editor)
        return;
    const uri = editor.document.uri.toString();
    // Call the *server* command name here:
    const params = {
        command: "jovial.dumpAst",
        arguments: [uri],
    };
    const res = await client.sendRequest(node_1.ExecuteCommandRequest.type, params);
    const text = typeof res === "string" ? res : JSON.stringify(res, null, 2);
    const doc = await vscode.workspace.openTextDocument({ content: text, language: "plaintext" });
    await vscode.window.showTextDocument(doc, { preview: true });
}
async function activate(context) {
    const output = vscode.window.createOutputChannel("Jovial LSP");
    context.subscriptions.push(output);
    const status = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
    status.command = "jovial.restartServer";
    status.show();
    context.subscriptions.push(status);
    setStatus(status, "stopped", "Click to start / restart Jovial LSP");
    // UI command (renamed) — does NOT collide with languageclient’s auto registration
    context.subscriptions.push(vscode.commands.registerCommand("jovial.dumpAstUi", async () => {
        try {
            await dumpAstUi(output);
        }
        catch (e) {
            output.appendLine(`dumpAstUi failed: ${String(e)}`);
        }
    }));
    context.subscriptions.push(vscode.commands.registerCommand("jovial.restartServer", async () => {
        try {
            await startClient(context, output, status);
        }
        catch (e) {
            output.appendLine(`restart failed: ${String(e)}`);
            setStatus(status, "error", `restart failed: ${String(e)}`);
        }
    }));
    context.subscriptions.push({ dispose: () => { void stopClient(status); } });
    if (getConfig().autostart) {
        await startClient(context, output, status);
    }
}
async function deactivate() {
    if (client)
        await client.stop();
}
//# sourceMappingURL=extension.js.map