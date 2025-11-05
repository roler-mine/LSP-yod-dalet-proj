// src/extension.ts
import * as vscode from 'vscode';
import * as net from 'net';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  StreamInfo,
  TransportKind,
  type DocumentSelector,
} from 'vscode-languageclient/node';
import { Trace } from 'vscode-jsonrpc';

let client: LanguageClient | undefined;
let output: vscode.OutputChannel;

export async function activate(ctx: vscode.ExtensionContext) {
  output = vscode.window.createOutputChannel('MyLang Language Server');

  const startClient = async () => {
    if (client) {
      await client.stop();
      client.dispose();
      client = undefined;
    }

    const config = vscode.workspace.getConfiguration();
    const languageId = config.get<string>('lspLang.languageId', 'mylang');
    const mode = config.get<'stdio' | 'tcp'>('lspLang.server.mode', 'stdio');
    const tracePref = config.get<'off' | 'messages' | 'verbose'>('lspLang.trace.server', 'off');

    const serverOptions: ServerOptions =
      mode === 'tcp' ? tcpServerOptions(config) : stdioServerOptions(config);

    const documentSelector: DocumentSelector = [
      { scheme: 'file', language: languageId },
      { scheme: 'untitled', language: languageId },
    ];

    const clientOptions: LanguageClientOptions = {
      documentSelector,
      synchronize: {
        fileEvents: vscode.workspace.createFileSystemWatcher('**/*'),
      },
      outputChannel: output,
    };

    client = new LanguageClient(
      'mylang',
      'MyLang Language Server',
      serverOptions,
      clientOptions
    );

    // ensure client is disposed with the extension
    ctx.subscriptions.push(client);

    // start the client then set trace
    await client.start();
    const traceMap = { off: Trace.Off, messages: Trace.Messages, verbose: Trace.Verbose } as const;
    client.setTrace(traceMap[tracePref]);
  };

  ctx.subscriptions.push(
    vscode.commands.registerCommand('lspLang.restart', () => startClient()),
    vscode.commands.registerCommand('lspLang.showServerLog', () => output.show(true))
  );

  await startClient();
}

export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
    client.dispose();
    client = undefined;
  }
}

function stdioServerOptions(config: vscode.WorkspaceConfiguration): ServerOptions {
  const command = config.get<string>('lspLang.server.command');
  const args = config.get<string[]>('lspLang.server.args', []);
  if (!command || command.trim() === '') {
    void vscode.window.showErrorMessage(
      'MyLang LSP: Set "lspLang.server.command" to your server executable (or switch to tcp mode).'
    );
  }
  return {
    run: { command: command ?? '', args, transport: TransportKind.stdio },
    debug: { command: command ?? '', args, transport: TransportKind.stdio },
  };
}

function tcpServerOptions(config: vscode.WorkspaceConfiguration): ServerOptions {
  const host = config.get<string>('lspLang.server.tcp.host', '127.0.0.1');
  const port = config.get<number>('lspLang.server.tcp.port', 2087);
  return async (): Promise<StreamInfo> =>
    new Promise((resolve, reject) => {
      const socket = new net.Socket();
      socket.connect(port, host, () => resolve({ reader: socket, writer: socket }));
      socket.on('error', (err) => {
        void vscode.window.showErrorMessage(`MyLang LSP TCP connection failed: ${err.message}`);
        reject(err);
      });
    });
}
