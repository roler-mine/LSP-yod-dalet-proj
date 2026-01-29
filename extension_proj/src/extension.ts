import * as vscode from "vscode";
import * as fs from "fs";
import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient | undefined;

function getServerPath(): string | undefined {
  // Read from settings: "jovial.serverPath"
  const cfg = vscode.workspace.getConfiguration("jovial");
  const configured = cfg.get<string>("C:\\Users\\miran\\OneDrive\\מסמכים\\GitHub\\LSP-yod-dalet-proj\\server_proj\\_build\\default\\bin\\main.exe");
  if (configured && configured.trim().length > 0) return configured;

  return undefined;
}

async function startClient(context: vscode.ExtensionContext): Promise<void> {
  const output = vscode.window.createOutputChannel("Jovial LSP");
  const serverPath = "C:\\Users\\miran\\OneDrive\\מסמכים\\GitHub\\LSP-yod-dalet-proj\\server_proj\\_build\\default\\bin\\Main.exe";

  if (!serverPath) {
    vscode.window.showErrorMessage(
      "Jovial LSP: Missing server path. Set jovial.serverPath in settings."
    );
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

  const serverOptions: ServerOptions = {
    run: { command: serverPath, args: [] },
    debug: { command: serverPath, args: [] },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "jovial" }],
    outputChannel: output,
    traceOutputChannel: output,
    // You can also enable protocol trace via VS Code setting:
    // "jovial.trace.server": "verbose" :contentReference[oaicite:2]{index=2}
  };

  client = new LanguageClient("jovialLsp", "Jovial LSP", serverOptions, clientOptions);

  // vscode-languageclient start() is Disposable in older versions, Promise in newer versions. :contentReference[oaicite:3]{index=3}
  const started: any = client.start();
  if (started && typeof started.then === "function") {
    await started;
  } else if (started && typeof started.dispose === "function") {
    context.subscriptions.push(started);
  }

  // Always stop on deactivate
  context.subscriptions.push({ dispose: () => client?.stop() });

  output.appendLine("Client started.");
}

export async function activate(context: vscode.ExtensionContext) {
  await startClient(context);


  // Optional: on-demand parse/validate command (does NOT replace standard LSP diagnostics)
  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.debugParse", async () => {
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
        const res: any = await client.sendRequest("jovial/parse", { uri, text });

        // Expect: { status: "ok", diagnostics: [...] }
        const diags = Array.isArray(res?.diagnostics) ? res.diagnostics : [];
        if (diags.length === 0) {
          vscode.window.showInformationMessage("jovial/parse: no diagnostics.");
          return;
        }

        // Show a compact summary
        const lines = diags.slice(0, 50).map((d: any) => {
          const msg = d?.message ?? "(no message)";
          const s = d?.range?.start;
          const line = typeof s?.line === "number" ? s.line + 1 : "?";
          const col = typeof s?.character === "number" ? s.character + 1 : "?";
          return `(${line}:${col}) ${msg}`;
        });

        const doc = await vscode.workspace.openTextDocument({
          content: lines.join("\n"),
          language: "text",
        });
        await vscode.window.showTextDocument(doc, { preview: true });
      } catch (e: any) {
        vscode.window.showErrorMessage(`jovial/parse failed: ${String(e?.message ?? e)}`);
      }
    })
  );
}

export function deactivate(): Thenable<void> | undefined {
  return client ? client.stop() : undefined;
}
