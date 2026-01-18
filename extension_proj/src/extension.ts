import * as path from 'path';
import * as vscode from 'vscode';
import * as fs from 'fs';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient;
const diagnosticCollection = vscode.languages.createDiagnosticCollection("jovial");

export function activate(context: vscode.ExtensionContext) {
    console.log("!!! JOVIAL EXTENSION ACTIVATING !!!");

    // FIX: Update this path to your specific main.exe location
    const serverPath = "C:\\Users\\miran\\OneDrive\\מסמכים\\GitHub\\LSP-yod-dalet-proj\\server_proj\\_build\\default\\bin\\main.exe"

    if (!fs.existsSync(serverPath)) {
        vscode.window.showErrorMessage(`CRITICAL: Cannot find server at ${serverPath}`);
        return;
    }

    const serverOptions: ServerOptions = {
        run: { command: serverPath, transport: TransportKind.stdio },
        debug: { command: serverPath, transport: TransportKind.stdio }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'jovial' }],
    };

    client = new LanguageClient('jovialLsp', 'Jovial LSP', serverOptions, clientOptions);

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

export function deactivate(): Thenable<void> | undefined {
    return client ? client.stop() : undefined;
}

async function validateTextDocument(textDocument: vscode.TextDocument) {
    if (textDocument.languageId !== 'jovial') { return; }

    try {
        const response: any = await client.sendRequest("parse", {
            text: textDocument.getText()
        });

        const diagnostics: vscode.Diagnostic[] = [];

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

    } catch (e) {
        console.error("Validation failed:", e);
    }
}