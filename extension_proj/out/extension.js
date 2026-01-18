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
const diagnosticCollection = vscode.languages.createDiagnosticCollection("jovial");
function activate(context) {
    console.log("!!! JOVIAL EXTENSION ACTIVATING !!!");
    // FIX: Update this path to your specific main.exe location
    const serverPath = "C:\\Users\\miran\\OneDrive\\מסמכים\\GitHub\\LSP-yod-dalet-proj\\server_proj\\_build\\default\\bin\\main.exe";
    if (!fs.existsSync(serverPath)) {
        vscode.window.showErrorMessage(`CRITICAL: Cannot find server at ${serverPath}`);
        return;
    }
    const serverOptions = {
        run: { command: serverPath, transport: node_1.TransportKind.stdio },
        debug: { command: serverPath, transport: node_1.TransportKind.stdio }
    };
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'jovial' }],
    };
    client = new node_1.LanguageClient('jovialLsp', 'Jovial LSP', serverOptions, clientOptions);
    client.start().then(() => {
        console.log("CLIENT STARTED.");
        // Validate open documents
        vscode.workspace.textDocuments.forEach(validateTextDocument);
        // Validate on change and open
        context.subscriptions.push(vscode.workspace.onDidChangeTextDocument(e => validateTextDocument(e.document)));
        context.subscriptions.push(vscode.workspace.onDidOpenTextDocument(validateTextDocument));
    });
    context.subscriptions.push(diagnosticCollection);
}
exports.activate = activate;
function deactivate() {
    return client ? client.stop() : undefined;
}
exports.deactivate = deactivate;
function validateTextDocument(textDocument) {
    return __awaiter(this, void 0, void 0, function* () {
        if (textDocument.languageId !== 'jovial') {
            return;
        }
        try {
            const response = yield client.sendRequest("parse", {
                text: textDocument.getText()
            });
            const diagnostics = [];
            // Main.ml sends { status: "error", errors: [...] }
            if (response.status === "error" && response.errors && Array.isArray(response.errors)) {
                for (const err of response.errors) {
                    // Convert 1-based (OCaml) to 0-based (VS Code)
                    const line = Math.max(0, err.line - 1);
                    const col = Math.max(0, err.col);
                    // Highlight 5 characters or the rest of the word
                    const range = new vscode.Range(line, col, line, col + 5);
                    const diagnostic = new vscode.Diagnostic(range, err.message, vscode.DiagnosticSeverity.Error);
                    diagnostics.push(diagnostic);
                }
            }
            // If status is "success", we set an empty array to clear previous errors
            diagnosticCollection.set(textDocument.uri, diagnostics);
        }
        catch (e) {
            console.error("Validation failed:", e);
        }
    });
}
//# sourceMappingURL=extension.js.map