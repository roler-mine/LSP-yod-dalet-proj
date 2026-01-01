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
const node_1 = require("vscode-languageclient/node");
let client;
let diagnosticCollection;
function activate(context) {
    console.log("!!! JOVIAL EXTENSION ACTIVATING !!!");
    const serverPath = "C:\\Users\\miran\\OneDrive\\מסמכים\\GitHub\\LSP-yod-dalet-proj\\_build\\default\\server_proj\\main.exe";
    const serverOptions = {
        run: {
            command: serverPath,
            transport: node_1.TransportKind.stdio
        },
        debug: {
            command: serverPath,
            transport: node_1.TransportKind.stdio
        }
    };
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'jovial' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
        }
    };
    // 2. Start the LSP Client
    client = new node_1.LanguageClient('jovialLsp', 'Jovial LSP', serverOptions, clientOptions);
    // 3. Create a collection for red squiggles (diagnostics)
    diagnosticCollection = vscode.languages.createDiagnosticCollection('jovial');
    context.subscriptions.push(diagnosticCollection);
    // 4. Function to send document text to server and process result
    function validateTextDocument(textDocument) {
        return __awaiter(this, void 0, void 0, function* () {
            if (textDocument.languageId !== 'jovial') {
                return;
            }
            try {
                // Send our custom "parse" request
                const response = yield client.sendRequest("parse", {
                    text: textDocument.getText()
                });
                console.log("Server Response:", JSON.stringify(response, null, 2));
                const diagnostics = [];
                if (response.status === "error") {
                    // Parse the error string "Syntax error at line X, column Y"
                    // This regex matches the format generated in Main.ml
                    const match = response.message.match(/line (\d+), column (\d+)/);
                    if (match) {
                        const line = parseInt(match[1]) - 1; // VSCode is 0-indexed
                        const col = parseInt(match[2]);
                        const range = new vscode.Range(line, col, line, col + 1);
                        const diagnostic = new vscode.Diagnostic(range, response.message, vscode.DiagnosticSeverity.Error);
                        diagnostics.push(diagnostic);
                    }
                }
                // Set the diagnostics (clears old ones if empty)
                diagnosticCollection.set(textDocument.uri, diagnostics);
            }
            catch (e) {
                console.error(e);
            }
        });
    }
    console.log("Attempting to start client with path:", serverPath);
    client.start().then(() => {
        console.log("CLIENT STARTED SUCCESSFULLY!");
        // Validate open documents
        vscode.workspace.textDocuments.forEach(validateTextDocument);
        // Validate on change
        vscode.workspace.onDidChangeTextDocument(event => {
            validateTextDocument(event.document);
        });
    }).catch((error) => {
        // THIS IS THE IMPORTANT PART
        console.error("CLIENT FAILED TO START:", error);
    });
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map