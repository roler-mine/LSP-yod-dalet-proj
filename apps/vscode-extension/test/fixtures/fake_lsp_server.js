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

function makeLocation(uri) {
  return {
    uri,
    range: {
      start: { line: 0, character: 0 },
      end: { line: 0, character: 3 },
    },
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
      respond(message.id, {
        capabilities: {
          textDocumentSync: 2,
          definitionProvider: true,
          declarationProvider: true,
          typeDefinitionProvider: true,
          implementationProvider: true,
          referencesProvider: true,
          hoverProvider: true,
          executeCommandProvider: {
            commands: [
              "jovial.dumpLsifIndex",
              "jovial.dumpLsifDelta",
              "jovial.dumpAst",
              "jovial.dumpCst",
              "jovial.dumpAstGraph",
            ],
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
