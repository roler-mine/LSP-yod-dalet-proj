"use strict";

const fs = require("fs");
const path = require("path");

function parseArgs(argv) {
  const out = {};
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--log" && i + 1 < argv.length) {
      out.logPath = argv[i + 1];
      i += 1;
    }
  }
  return out;
}

const { logPath } = parseArgs(process.argv.slice(2));

const semanticTokenTypes = [
  "namespace",
  "type",
  "class",
  "enum",
  "interface",
  "struct",
  "typeParameter",
  "parameter",
  "variable",
  "property",
  "enumMember",
  "event",
  "function",
  "method",
  "macro",
  "keyword",
  "modifier",
  "comment",
  "string",
  "number",
  "regexp",
  "operator",
];
const semanticTokenModifiers = ["declaration", "readonly"];
const defaultFeatures = {
  definition: true,
  declaration: true,
  typeDefinition: true,
  implementation: true,
  references: true,
  documentSymbols: true,
  workspaceSymbols: true,
  hover: true,
  signatureHelp: true,
  rename: true,
  completion: true,
  codeActions: true,
  codeLens: true,
  inlayHints: true,
  formatting: true,
  semanticTokens: true,
};
let enabledFeatures = { ...defaultFeatures };

function featureEnabled(features, key) {
  return features?.[key] !== false;
}

function readEnabledFeatures(features) {
  const out = {};
  for (const key of Object.keys(defaultFeatures)) {
    out[key] = featureEnabled(features, key);
  }
  return out;
}

function isEnabled(key) {
  return enabledFeatures[key] !== false;
}

function appendEvent(event) {
  if (!logPath) return;
  const rec = {
    ...event,
    pid: process.pid,
    timestamp: Date.now(),
  };
  fs.mkdirSync(path.dirname(logPath), { recursive: true });
  fs.appendFileSync(logPath, `${JSON.stringify(rec)}\n`);
}

function sampleUri() {
  return process.env.JOVIAL_INTEGRATION_SAMPLE_URI ?? "file:///sample.jov";
}

function makeRange() {
  return {
    start: { line: 0, character: 0 },
    end: { line: 0, character: 3 },
  };
}

function makeLocation(uri) {
  return {
    uri,
    range: makeRange(),
  };
}

function makeLsifIndex(uri) {
  return {
    symbols: [
      {
        id: "sym-foo",
        key: "FOO",
        kind: 12,
        definitions: [makeLocation(uri)],
        implementations: [],
        references: [
          { location: makeLocation(uri), declaration: true },
          { location: makeLocation(uri), declaration: false },
        ],
      },
    ],
    keyIndex: [{ key: "FOO", symbolIds: ["sym-foo"] }],
    symbolCount: 1,
    docCount: 1,
    revision: 1,
  };
}

function makeInlayHint() {
  return {
    position: { line: 0, character: 0 },
    label: "FAKE_PARAM:",
    kind: 2,
    paddingRight: true,
  };
}

function makeCodeLens() {
  return {
    range: makeRange(),
    command: {
      title: "1 reference",
      command: "jovial.refreshLsifCache",
      arguments: [sampleUri()],
    },
  };
}

function makeDocumentSymbol() {
  return {
    name: "FOO",
    detail: "fake JOVIAL symbol",
    kind: 12,
    range: makeRange(),
    selectionRange: makeRange(),
  };
}

function makeWorkspaceSymbol() {
  return {
    name: "FOO",
    kind: 12,
    location: makeLocation(sampleUri()),
  };
}

function makeCodeAction() {
  return {
    title: "Fake Jovial quick fix",
    kind: "quickfix",
    diagnostics: [],
  };
}

function makeCompletionList() {
  return {
    isIncomplete: false,
    items: [
      {
        label: "FOO",
        kind: 3,
        detail: "fake JOVIAL completion",
      },
    ],
  };
}

function send(message) {
  const body = JSON.stringify(message);
  process.stdout.write(
    `Content-Length: ${Buffer.byteLength(body, "utf8")}\r\n\r\n${body}`,
  );
}

function respond(id, result) {
  send({ jsonrpc: "2.0", id, result });
}

function parseContentLength(header) {
  const match = /Content-Length:\s*(\d+)/i.exec(header);
  return match ? Number.parseInt(match[1], 10) : undefined;
}

function handleRequest(message) {
  const method = message.method;
  const params = message.params ?? {};
  appendEvent({
    type: message.id === undefined ? "notification" : "request",
    method,
    params,
  });

  if (message.id === undefined) {
    if (method === "exit") {
      process.exit(0);
    }
    return;
  }

  switch (method) {
    case "initialize":
      const features = params?.initializationOptions?.jovial?.features ?? {};
      enabledFeatures = readEnabledFeatures(features);
      respond(message.id, {
        capabilities: {
          textDocumentSync: { openClose: true, change: 2 },
          ...(isEnabled("definition") ? { definitionProvider: true } : {}),
          ...(isEnabled("declaration") ? { declarationProvider: true } : {}),
          ...(isEnabled("typeDefinition")
            ? { typeDefinitionProvider: true }
            : {}),
          ...(isEnabled("implementation")
            ? { implementationProvider: true }
            : {}),
          ...(isEnabled("references") ? { referencesProvider: true } : {}),
          ...(isEnabled("documentSymbols")
            ? { documentSymbolProvider: true }
            : {}),
          ...(isEnabled("workspaceSymbols")
            ? { workspaceSymbolProvider: true }
            : {}),
          ...(isEnabled("hover") ? { hoverProvider: true } : {}),
          ...(isEnabled("signatureHelp")
            ? {
                signatureHelpProvider: {
                  triggerCharacters: ["(", ","],
                  retriggerCharacters: [","],
                },
              }
            : {}),
          ...(isEnabled("rename")
            ? { renameProvider: { prepareProvider: true } }
            : {}),
          ...(isEnabled("completion")
            ? {
                completionProvider: {
                  triggerCharacters: [".", "!", "'", '"'],
                  resolveProvider: true,
                },
              }
            : {}),
          ...(isEnabled("codeActions") ? { codeActionProvider: true } : {}),
          ...(isEnabled("codeLens")
            ? { codeLensProvider: { resolveProvider: true } }
            : {}),
          ...(isEnabled("formatting")
            ? {
                documentFormattingProvider: true,
                documentRangeFormattingProvider: true,
              }
            : {}),
          ...(isEnabled("inlayHints")
            ? { inlayHintProvider: { resolveProvider: false } }
            : {}),
          ...(isEnabled("semanticTokens")
            ? {
                semanticTokensProvider: {
                  legend: {
                    tokenTypes: semanticTokenTypes,
                    tokenModifiers: semanticTokenModifiers,
                  },
                  full: { delta: true },
                  range: true,
                },
              }
            : {}),
          executeCommandProvider: {
            commands: [
              "jovial.dumpLsifIndex",
              "jovial.dumpLsifDelta",
              "jovial.dumpAst",
              "jovial.dumpCst",
              "jovial.dumpAstGraph",
              "jovial.debugReport",
              "jovial.debugPerfStats",
              "jovial.debugScheduler",
              "jovial.debugMemory",
              "jovial.rescanWorkspace",
            ],
          },
          workspace: {
            didChangeWatchedFiles: { dynamicRegistration: false },
          },
        },
      });
      return;
    case "shutdown":
      respond(message.id, null);
      return;
    case "workspace/executeCommand": {
      const command = typeof params.command === "string" ? params.command : "";
      const args = Array.isArray(params.arguments) ? params.arguments : [];
      appendEvent({
        type: "executeCommand",
        command,
        args,
      });
      if (command === "jovial.dumpLsifIndex") {
        const targetUri =
          typeof args[0] === "string" && args[0].startsWith("file:")
            ? args[0]
            : process.env.JOVIAL_INTEGRATION_SAMPLE_URI;
        respond(message.id, makeLsifIndex(targetUri ?? "file:///sample.jov"));
        return;
      }
      if (command === "jovial.dumpLsifDelta") {
        const baseRevision =
          typeof args[1] === "number" ? Math.trunc(args[1]) : 0;
        respond(message.id, {
          baseRevision,
          revision: baseRevision + 1,
          reset: true,
          deletes: [],
          upserts: [],
        });
        return;
      }
      if (
        command === "jovial.dumpAst" ||
        command === "jovial.dumpCst" ||
        command === "jovial.dumpAstGraph"
      ) {
        respond(message.id, { command, ok: true });
        return;
      }
      respond(message.id, null);
      return;
    }
    case "textDocument/definition":
    case "textDocument/declaration":
    case "textDocument/typeDefinition":
    case "textDocument/implementation":
    case "textDocument/references":
    case "textDocument/hover":
      respond(message.id, null);
      return;
    case "textDocument/documentSymbol":
      respond(
        message.id,
        isEnabled("documentSymbols") ? [makeDocumentSymbol()] : [],
      );
      return;
    case "workspace/symbol":
      respond(
        message.id,
        isEnabled("workspaceSymbols") ? [makeWorkspaceSymbol()] : [],
      );
      return;
    case "textDocument/completion":
      respond(
        message.id,
        isEnabled("completion") ? makeCompletionList() : null,
      );
      return;
    case "completionItem/resolve":
      respond(message.id, params);
      return;
    case "textDocument/codeAction":
      respond(message.id, isEnabled("codeActions") ? [makeCodeAction()] : []);
      return;
    case "textDocument/codeLens":
      respond(message.id, isEnabled("codeLens") ? [makeCodeLens()] : []);
      return;
    case "codeLens/resolve":
      respond(message.id, params);
      return;
    case "textDocument/inlayHint":
      respond(message.id, isEnabled("inlayHints") ? [makeInlayHint()] : []);
      return;
    case "textDocument/formatting":
    case "textDocument/rangeFormatting":
      respond(message.id, []);
      return;
    case "textDocument/semanticTokens/full":
    case "textDocument/semanticTokens/range":
      respond(message.id, { data: [] });
      return;
    case "textDocument/semanticTokens/full/delta":
      respond(message.id, { edits: [] });
      return;
    default:
      respond(message.id, null);
  }
}

let buffer = Buffer.alloc(0);
process.stdin.on("data", (chunk) => {
  buffer = Buffer.concat([buffer, chunk]);
  while (true) {
    const headerEnd = buffer.indexOf("\r\n\r\n");
    if (headerEnd < 0) return;
    const header = buffer.slice(0, headerEnd).toString("utf8");
    const contentLength = parseContentLength(header);
    if (contentLength === undefined) {
      appendEvent({ type: "protocolError", header });
      buffer = Buffer.alloc(0);
      return;
    }
    const bodyStart = headerEnd + 4;
    const bodyEnd = bodyStart + contentLength;
    if (buffer.length < bodyEnd) return;
    const rawBody = buffer.slice(bodyStart, bodyEnd).toString("utf8");
    buffer = buffer.slice(bodyEnd);
    try {
      handleRequest(JSON.parse(rawBody));
    } catch (error) {
      appendEvent({
        type: "parseError",
        body: rawBody,
        error: String(error),
      });
    }
  }
});

process.on("SIGINT", () => process.exit(0));
process.on("SIGTERM", () => process.exit(0));

appendEvent({
  type: "processStart",
  argv: process.argv.slice(2),
});
