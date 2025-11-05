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
// src/extension.ts
const vscode = __importStar(require("vscode"));
const net = __importStar(require("net"));
const node_1 = require("vscode-languageclient/node");
const vscode_jsonrpc_1 = require("vscode-jsonrpc");
let client;
let output;
async function activate(ctx) {
    output = vscode.window.createOutputChannel('MyLang Language Server');
    const startClient = async () => {
        if (client) {
            await client.stop();
            client.dispose();
            client = undefined;
        }
        const config = vscode.workspace.getConfiguration();
        const languageId = config.get('lspLang.languageId', 'mylang');
        const mode = config.get('lspLang.server.mode', 'stdio');
        const tracePref = config.get('lspLang.trace.server', 'off');
        const serverOptions = mode === 'tcp' ? tcpServerOptions(config) : stdioServerOptions(config);
        const documentSelector = [
            { scheme: 'file', language: languageId },
            { scheme: 'untitled', language: languageId },
        ];
        const clientOptions = {
            documentSelector,
            synchronize: {
                fileEvents: vscode.workspace.createFileSystemWatcher('**/*'),
            },
            outputChannel: output,
        };
        client = new node_1.LanguageClient('mylang', 'MyLang Language Server', serverOptions, clientOptions);
        // ensure client is disposed with the extension
        ctx.subscriptions.push(client);
        // start the client then set trace
        await client.start();
        const traceMap = { off: vscode_jsonrpc_1.Trace.Off, messages: vscode_jsonrpc_1.Trace.Messages, verbose: vscode_jsonrpc_1.Trace.Verbose };
        client.setTrace(traceMap[tracePref]);
    };
    ctx.subscriptions.push(vscode.commands.registerCommand('lspLang.restart', () => startClient()), vscode.commands.registerCommand('lspLang.showServerLog', () => output.show(true)));
    await startClient();
}
async function deactivate() {
    if (client) {
        await client.stop();
        client.dispose();
        client = undefined;
    }
}
function stdioServerOptions(config) {
    const command = config.get('lspLang.server.command');
    const args = config.get('lspLang.server.args', []);
    if (!command || command.trim() === '') {
        void vscode.window.showErrorMessage('MyLang LSP: Set "lspLang.server.command" to your server executable (or switch to tcp mode).');
    }
    return {
        run: { command: command ?? '', args, transport: node_1.TransportKind.stdio },
        debug: { command: command ?? '', args, transport: node_1.TransportKind.stdio },
    };
}
function tcpServerOptions(config) {
    const host = config.get('lspLang.server.tcp.host', '127.0.0.1');
    const port = config.get('lspLang.server.tcp.port', 2087);
    return async () => new Promise((resolve, reject) => {
        const socket = new net.Socket();
        socket.connect(port, host, () => resolve({ reader: socket, writer: socket }));
        socket.on('error', (err) => {
            void vscode.window.showErrorMessage(`MyLang LSP TCP connection failed: ${err.message}`);
            reject(err);
        });
    });
}
//# sourceMappingURL=extension.js.map