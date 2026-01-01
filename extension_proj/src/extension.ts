import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;
let diagnosticCollection: vscode.DiagnosticCollection;

export function activate(context: vscode.ExtensionContext) {
    console.log("!!! JOVIAL EXTENSION ACTIVATING !!!");
    
    const serverPath = "C:\\Users\\miran\\OneDrive\\מסמכים\\GitHub\\LSP-yod-dalet-proj\\_build\\default\\server_proj\\main.exe";
    
    const serverOptions: ServerOptions = {
        run: { 
            command: serverPath, 
            transport: TransportKind.stdio 
        },
        debug: { 
            command: serverPath, 
            transport: TransportKind.stdio 
        }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'jovial' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
        }
    };

    // 2. Start the LSP Client
    client = new LanguageClient(
        'jovialLsp',
        'Jovial LSP',
        serverOptions,
        clientOptions
    );

    // 3. Create a collection for red squiggles (diagnostics)
    diagnosticCollection = vscode.languages.createDiagnosticCollection('jovial');
    context.subscriptions.push(diagnosticCollection);

    // 4. Function to send document text to server and process result
    async function validateTextDocument(textDocument: vscode.TextDocument) {
        if (textDocument.languageId !== 'jovial') {
            return;
        }

        try {
            // Send our custom "parse" request
            const response: any = await client.sendRequest("parse", {
                text: textDocument.getText()
            });

                console.log("Server Response:", JSON.stringify(response, null, 2));

            const diagnostics: vscode.Diagnostic[] = [];

            if (response.status === "error") {
                // Parse the error string "Syntax error at line X, column Y"
                // This regex matches the format generated in Main.ml
                const match = response.message.match(/line (\d+), column (\d+)/);
                
                if (match) {
                    const line = parseInt(match[1]) - 1; // VSCode is 0-indexed
                    const col = parseInt(match[2]);
                    
                    const range = new vscode.Range(line, col, line, col + 1);
                    const diagnostic = new vscode.Diagnostic(
                        range, 
                        response.message, 
                        vscode.DiagnosticSeverity.Error
                    );
                    diagnostics.push(diagnostic);
                }
            }

            // Set the diagnostics (clears old ones if empty)
            diagnosticCollection.set(textDocument.uri, diagnostics);

        } catch (e) {
            console.error(e);
        }
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

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}