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
const vscode = __importStar(require("vscode"));
const node_1 = require("vscode-languageclient/node");
let client;
let output;
async function activate(ctx) {
    output = vscode.window.createOutputChannel('MyLang Language Server');
    const startClient = async () => {
        if (client) {
            await client.stop();
            client = undefined;
        }
        const config = vscode.workspace.getConfiguration();
        const languageId = config.get('lspLang.languageId', 'mylang');
        const mode = config.get('lspLang.server.mode', 'stdio');
        const trace = config.get('lspLang.trace.server', 'off');
        const serverOptions = mode === 'tcp' ? tcpServerOptions(config) : stdioServerOptions(config);
        const docSelector = [
            { scheme: 'file', language: languageId },
            { scheme: 'untitled', language: languageId }
        ];
        const clientOptions = {
            documentSelector: docSelector,
            synchronize: {
                // Watch relevant files so the server can react to file changes
                fileEvents: vscode.workspace.createFileSystemWatcher('**/*')
            },
            // Send workspace and settings as initializationOptions if you want
            initializationOptions: {
            // You can add custom knobs here to pass into your server
            },
            outputChannel: output,
            markdown: { isTrusted: true }
        };
        client = new node_1.LanguageClient('mylang', 'MyLang Language Server', serverOptions, clientOptions);
        // Apply trace level
        switch (trace) {
            case 'messages':
                client.trace = 1; // Trace.Messages
                break;
            case 'verbose':
                client.trace = 2; // Trace.Verbose
                break;
            default:
                client.trace = 0; // Trace.Off
        }
        ctx.subscriptions.push(client.start());
    };
    // Commands
    ctx.subscriptions.push(vscode.commands.registerCommand('lspLang.restart', startClient), vscode.commands.registerCommand('lspLang.showServerLog', () => output.show(true)));
}
//# sourceMappingURL=extension.js.map