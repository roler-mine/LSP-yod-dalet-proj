"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const vscode = require("vscode");
const fs = require("fs");
const node_1 = require("vscode-languageclient/node");
let client;
function getServerPath() {
    // Read from settings: "jovial.serverPath"
    const cfg = vscode.workspace.getConfiguration("jovial");
    const configured = cfg.get("C:\\Users\\miran\\OneDrive\\מסמכים\\GitHub\\LSP-yod-dalet-proj\\server_proj\\_build\\default\\bin\\main.exe");
    if (configured && configured.trim().length > 0)
        return configured;
    return undefined;
}
function startClient(context) {
    return __awaiter(this, void 0, void 0, function* () {
        const output = vscode.window.createOutputChannel("Jovial LSP");
        const serverPath = "C:\\Users\\miran\\OneDrive\\מסמכים\\GitHub\\LSP-yod-dalet-proj\\server_proj\\_build\\default\\bin\\main.exe";
        if (!serverPath) {
            vscode.window.showErrorMessage("Jovial LSP: Missing server path. Set jovial.serverPath in settings.");
            output.appendLine("Missing jovial.serverPath setting.");
            output.show(true);
            return;
        }
        if (!fs.existsSync(serverPath)) {
            vscode.window.showErrorMessage(`Jovial LSP: Server not found at: ${serverPath}`);
            output.appendLine(`Server not found: ${serverPath}`);
            output.show(true);
            return;
        }
        const serverOptions = {
            run: { command: serverPath, args: [] },
            debug: { command: serverPath, args: [] },
        };
        const clientOptions = {
            documentSelector: [{ scheme: "file", language: "jovial" }],
            outputChannel: output,
            traceOutputChannel: output,
            // You can also enable protocol trace via VS Code setting:
            // "jovial.trace.server": "verbose" :contentReference[oaicite:2]{index=2}
        };
        client = new node_1.LanguageClient("jovialLsp", "Jovial LSP", serverOptions, clientOptions);
        // vscode-languageclient start() is Disposable in older versions, Promise in newer versions. :contentReference[oaicite:3]{index=3}
        const started = client.start();
        if (started && typeof started.then === "function") {
            yield started;
        }
        else if (started && typeof started.dispose === "function") {
            context.subscriptions.push(started);
        }
        // Always stop on deactivate
        context.subscriptions.push({ dispose: () => client === null || client === void 0 ? void 0 : client.stop() });
        output.appendLine("Client started.");
    });
}
function activate(context) {
    return __awaiter(this, void 0, void 0, function* () {
        yield startClient(context);
        // Optional: on-demand parse/validate command (does NOT replace standard LSP diagnostics)
        context.subscriptions.push(vscode.commands.registerCommand("jovial.debugParse", () => __awaiter(this, void 0, void 0, function* () {
            var _a;
            if (!client) {
                vscode.window.showErrorMessage("Jovial LSP: client not running.");
                return;
            }
            const editor = vscode.window.activeTextEditor;
            if (!editor || editor.document.languageId !== "jovial") {
                vscode.window.showInformationMessage("Open a Jovial file first.");
                return;
            }
            const uri = editor.document.uri.toString();
            const text = editor.document.getText();
            try {
                const res = yield client.sendRequest("jovial/parse", { uri, text });
                // Expect: { status: "ok", diagnostics: [...] }
                const diags = Array.isArray(res === null || res === void 0 ? void 0 : res.diagnostics) ? res.diagnostics : [];
                if (diags.length === 0) {
                    vscode.window.showInformationMessage("jovial/parse: no diagnostics.");
                    return;
                }
                // Show a compact summary
                const lines = diags.slice(0, 50).map((d) => {
                    var _a, _b;
                    const msg = (_a = d === null || d === void 0 ? void 0 : d.message) !== null && _a !== void 0 ? _a : "(no message)";
                    const s = (_b = d === null || d === void 0 ? void 0 : d.range) === null || _b === void 0 ? void 0 : _b.start;
                    const line = typeof (s === null || s === void 0 ? void 0 : s.line) === "number" ? s.line + 1 : "?";
                    const col = typeof (s === null || s === void 0 ? void 0 : s.character) === "number" ? s.character + 1 : "?";
                    return `(${line}:${col}) ${msg}`;
                });
                const doc = yield vscode.workspace.openTextDocument({
                    content: lines.join("\n"),
                    language: "text",
                });
                yield vscode.window.showTextDocument(doc, { preview: true });
            }
            catch (e) {
                vscode.window.showErrorMessage(`jovial/parse failed: ${String((_a = e === null || e === void 0 ? void 0 : e.message) !== null && _a !== void 0 ? _a : e)}`);
            }
        })));
    });
}
exports.activate = activate;
function deactivate() {
    return client ? client.stop() : undefined;
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map