// Module overview: End-to-end performance scenarios for extension startup and server readiness.

import { strict as assert } from "assert";
import * as fs from "fs";
import * as path from "path";
import { performance } from "perf_hooks";
import * as vscode from "vscode";

const EXTENSION_ID = "roler-mine.jovial-lsp-client";
const SAMPLE_PATH = process.env.JOVIAL_E2E_SAMPLE;
const WORKSPACE_PATH = process.env.JOVIAL_E2E_WORKSPACE;
const REPORT_PATH = process.env.JOVIAL_E2E_REPORT;
const SERVER_PATH = process.env.JOVIAL_E2E_SERVER_PATH;
const DIAGNOSTIC_SAMPLE_PATH = process.env.JOVIAL_E2E_DIAGNOSTIC_SAMPLE;
const E2E_PROFILE = process.env.JOVIAL_E2E_PROFILE ?? "small";
const DEFAULT_WAIT_TIMEOUT_MS =
  Number.parseInt(process.env.JOVIAL_E2E_WAIT_TIMEOUT_MS ?? "", 10) ||
  (E2E_PROFILE === "huge" || process.env.CI === "true" ? 60000 : 20000);
const DIAGNOSTIC_QUIET_MS = process.env.CI === "true" ? 1200 : 500;
const COMMAND_SAMPLES = Math.max(
  1,
  Number.parseInt(process.env.JOVIAL_E2E_PERF_SAMPLES ?? "5", 10) || 5,
);
const HOVER_SAMPLES = Math.max(
  1,
  Number.parseInt(
    process.env.JOVIAL_E2E_HOVER_SAMPLES ??
      process.env.JOVIAL_E2E_PERF_SAMPLES ??
      "5",
    10,
  ) || 5,
);
const NAV_SAMPLES = Math.max(
  1,
  Number.parseInt(
    process.env.JOVIAL_E2E_NAV_SAMPLES ??
      process.env.JOVIAL_E2E_PERF_SAMPLES ??
      "5",
    10,
  ) || 5,
);
const SAMPLE_BYTES =
  Number.parseInt(process.env.JOVIAL_E2E_SAMPLE_BYTES ?? "0", 10) || 0;
const HUGE_THRESHOLD_BYTES =
  Number.parseInt(process.env.JOVIAL_E2E_HUGE_THRESHOLD_BYTES ?? "0", 10) || 0;
const FULL_PARSE_MAX_BYTES =
  Number.parseInt(process.env.JOVIAL_E2E_FULL_PARSE_MAX_BYTES ?? "0", 10) || 0;
const VIEWPORT_LINE_COUNT = Math.max(
  0,
  Number.parseInt(process.env.JOVIAL_E2E_VIEWPORT_LINE_COUNT ?? "0", 10) || 0,
);
const COMMAND_TIMEOUT_MS =
  Number.parseInt(process.env.JOVIAL_E2E_COMMAND_TIMEOUT_MS ?? "", 10) ||
  DEFAULT_WAIT_TIMEOUT_MS;

type TimedValue<T> = {
  durationMs: number;
  value: T;
};

type CommandMeasurement = {
  name: string;
  iterations: number;
  durationsMs: number[];
  stats: LatencyStats;
  resultSummary: unknown;
  errors: string[];
};

type DiagnosticSnapshot = {
  count: number;
  fingerprint: string;
  messages: string[];
};

type DiagnosticMeasurement = {
  name: string;
  durationMs: number;
  count: number;
  fingerprint: string;
  messages: string[];
  error?: string;
};

type DocumentEditState = {
  version: number;
  lineCount: number;
  firstLine: string;
  startsWithBrokenLine: boolean;
  textBytes: number;
};

type PulledDiagnosticSummary = {
  kind: string;
  count: number;
  messages: string[];
  error?: string;
};

type SeededDiagnosticMeasurement = {
  samplePath: string;
  languageId: string;
  textLength: number;
  pulled: {
    durationMs: number;
    kind: string;
    count: number;
    messages: string[];
    error?: string;
  };
  expectedSubstrings: string[];
  matchedSubstrings: string[];
  missingSubstrings: string[];
  baseline: DiagnosticMeasurement;
  errors: string[];
};

type WorkspaceReadinessMeasurement = {
  name: string;
  durationMs: number;
  ready: boolean;
  elapsedMs?: number;
  targetMs?: number;
  startup?: unknown;
  errors: string[];
};

type WorkspaceFolderReport = {
  name: string;
  uri: string;
  fsPath: string;
};

type WorkspaceInventory = {
  sourceFileCount: number;
  sourceBytes: number;
  files: Array<{ name: string; bytes: number }>;
  featureCounts: {
    compoolImports: number;
    compoolDeclarations: number;
    icopyIncludes: number;
    defines: number;
    defProcedures: number;
    refProcedures: number;
    types: number;
    tables: number;
  };
  featureScanBytesPerFile: number;
};

type LatencyStats = {
  minMs: number;
  p50Ms: number;
  p95Ms: number;
  p99Ms: number;
  maxMs: number;
  avgMs: number;
};

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function requireEnv(name: string, value: string | undefined): string {
  assert.ok(value, `Missing required environment variable: ${name}`);
  return value;
}

function errorText(error: unknown): string {
  if (error instanceof Error) return error.stack ?? error.message;
  return String(error);
}

function objectRecord(value: unknown): Record<string, unknown> | undefined {
  if (!value || typeof value !== "object" || Array.isArray(value)) {
    return undefined;
  }
  return value as Record<string, unknown>;
}

function finiteNumber(value: unknown): number | undefined {
  if (typeof value !== "number" || !Number.isFinite(value)) return undefined;
  return value;
}

function parseExpectedDiagnosticSubstrings(): string[] {
  const raw = process.env.JOVIAL_E2E_DIAGNOSTIC_EXPECT;
  if (!raw) return [];
  try {
    const parsed = JSON.parse(raw);
    return Array.isArray(parsed)
      ? parsed.filter((value): value is string => typeof value === "string")
      : [];
  } catch {
    return [];
  }
}

async function timed<T>(fn: () => T | PromiseLike<T>): Promise<TimedValue<T>> {
  const started = performance.now();
  const value = await fn();
  return {
    durationMs: performance.now() - started,
    value,
  };
}

async function withTimeout<T>(
  promise: PromiseLike<T>,
  timeoutMs: number,
  label: string,
): Promise<T> {
  let timer: ReturnType<typeof setTimeout> | undefined;
  try {
    return await Promise.race([
      Promise.resolve(promise),
      new Promise<T>((_, reject) => {
        timer = setTimeout(
          () => reject(new Error(`${label} timed out after ${timeoutMs}ms`)),
          timeoutMs,
        );
      }),
    ]);
  } finally {
    if (timer) clearTimeout(timer);
  }
}

function roundMs(value: number): number {
  return Math.round(value * 1000) / 1000;
}

function percentile(
  sortedValues: readonly number[],
  percentileValue: number,
): number {
  if (sortedValues.length === 0) return 0;
  const index = Math.min(
    sortedValues.length - 1,
    Math.ceil((percentileValue / 100) * sortedValues.length) - 1,
  );
  return sortedValues[Math.max(0, index)];
}

function latencyStats(values: readonly number[]): LatencyStats {
  if (values.length === 0) {
    return {
      minMs: 0,
      p50Ms: 0,
      p95Ms: 0,
      p99Ms: 0,
      maxMs: 0,
      avgMs: 0,
    };
  }

  const sorted = [...values].sort((a, b) => a - b);
  const total = values.reduce((acc, value) => acc + value, 0);
  return {
    minMs: roundMs(sorted[0]),
    p50Ms: roundMs(percentile(sorted, 50)),
    p95Ms: roundMs(percentile(sorted, 95)),
    p99Ms: roundMs(percentile(sorted, 99)),
    maxMs: roundMs(sorted[sorted.length - 1]),
    avgMs: roundMs(total / values.length),
  };
}

function resultSummary(value: unknown): unknown {
  if (Array.isArray(value)) {
    return { kind: "array", length: value.length };
  }

  if (value instanceof vscode.SemanticTokens) {
    return { kind: "semanticTokens", dataLength: value.data.length };
  }

  if (value && typeof value === "object") {
    const record = value as Record<string, unknown>;
    const items = record["items"];
    if (Array.isArray(items)) {
      return { kind: "objectWithItems", length: items.length };
    }
    return { kind: "object", keys: Object.keys(record).sort() };
  }

  return { kind: typeof value };
}

function summaryLength(summary: unknown): number | undefined {
  if (!summary || typeof summary !== "object") return undefined;
  const record = summary as Record<string, unknown>;
  const length = record["length"];
  if (typeof length === "number" && Number.isFinite(length)) return length;
  const dataLength = record["dataLength"];
  if (typeof dataLength === "number" && Number.isFinite(dataLength)) {
    return dataLength;
  }
  return undefined;
}

function emptyMixedProviderResults(commands: readonly CommandMeasurement[]): string[] {
  if (E2E_PROFILE !== "mixed") return [];
  const required = new Set([
    "documentSymbols",
    "hover",
    "definition",
    "references",
    "semanticTokens.range",
    "completion",
  ]);
  return commands
    .filter((command) => required.has(command.name))
    .filter((command) => (summaryLength(command.resultSummary) ?? 0) <= 0)
    .map((command) => command.name);
}

function countMatches(text: string, pattern: RegExp): number {
  return text.match(pattern)?.length ?? 0;
}

function workspaceFoldersReport(): WorkspaceFolderReport[] {
  return (vscode.workspace.workspaceFolders ?? []).map((folder) => ({
    name: folder.name,
    uri: folder.uri.toString(),
    fsPath: folder.uri.fsPath,
  }));
}

function workspaceInventory(workspacePath: string): WorkspaceInventory {
  const featureScanBytesPerFile = 256 * 1024;
  const files = fs
    .readdirSync(workspacePath, { withFileTypes: true })
    .filter((entry) => entry.isFile() && /\.j(ov|73|vl)$/i.test(entry.name))
    .map((entry) => {
      const filePath = path.join(workspacePath, entry.name);
      const stat = fs.statSync(filePath);
      return { name: entry.name, bytes: stat.size, filePath };
    })
    .sort((a, b) => a.name.localeCompare(b.name));

  const featureCounts = {
    compoolImports: 0,
    compoolDeclarations: 0,
    icopyIncludes: 0,
    defines: 0,
    defProcedures: 0,
    refProcedures: 0,
    types: 0,
    tables: 0,
  };

  for (const file of files) {
    const fd = fs.openSync(file.filePath, "r");
    let text: string;
    try {
      const buffer = Buffer.allocUnsafe(
        Math.min(file.bytes, featureScanBytesPerFile),
      );
      const bytesRead = fs.readSync(fd, buffer, 0, buffer.length, 0);
      text = buffer.subarray(0, bytesRead).toString("utf8");
    } finally {
      fs.closeSync(fd);
    }
    featureCounts.compoolImports += countMatches(text, /!COMPOOL\b/gi);
    featureCounts.compoolDeclarations += countMatches(
      text,
      /^\s*(?:START\s+)?COMPOOL\b/gim,
    );
    featureCounts.icopyIncludes += countMatches(text, /\bICOPY\b/gi);
    featureCounts.defines += countMatches(text, /\bDEFINE\b/gi);
    featureCounts.defProcedures += countMatches(text, /\bDEF\s+PROC\b/gi);
    featureCounts.refProcedures += countMatches(text, /\bREF\s+PROC\b/gi);
    featureCounts.types += countMatches(text, /\bTYPE\b/gi);
    featureCounts.tables += countMatches(text, /\bTABLE\b/gi);
  }

  return {
    sourceFileCount: files.length,
    sourceBytes: files.reduce((total, file) => total + file.bytes, 0),
    files: files.map((file) => ({ name: file.name, bytes: file.bytes })),
    featureCounts,
    featureScanBytesPerFile,
  };
}

async function measureCommand<T>(
  name: string,
  command: string,
  iterations: number,
  ...args: unknown[]
): Promise<CommandMeasurement> {
  console.log(`[jovial-e2e] command ${name}: start (${iterations} iterations)`);
  const durationsMs: number[] = [];
  const errors: string[] = [];
  let summary: unknown = { kind: "not-run" };

  for (let i = 0; i < iterations; i += 1) {
    try {
      const run = await timed(() =>
        withTimeout(
          vscode.commands.executeCommand<T>(command, ...args),
          COMMAND_TIMEOUT_MS,
          `${name} iteration ${i + 1}`,
        ),
      );
      durationsMs.push(run.durationMs);
      summary = resultSummary(run.value);
    } catch (error) {
      errors.push(errorText(error));
      console.log(
        `[jovial-e2e] command ${name}: iteration ${i + 1} failed: ${errorText(error)}`,
      );
    }
  }
  console.log(
    `[jovial-e2e] command ${name}: done (${durationsMs.length}/${iterations} successful)`,
  );

  return {
    name,
    iterations,
    durationsMs: durationsMs.map(roundMs),
    stats: latencyStats(durationsMs),
    resultSummary: summary,
    errors,
  };
}

function positionOf(
  document: vscode.TextDocument,
  needle: string,
): vscode.Position {
  const maxLines = Math.min(document.lineCount, 200);
  for (let line = 0; line < maxLines; line += 1) {
    const text = document.lineAt(line).text;
    const character = text.indexOf(needle);
    if (character >= 0) {
      return new vscode.Position(
        line,
        character + Math.floor(needle.length / 2),
      );
    }
  }
  assert.fail(`Could not find '${needle}' in ${document.uri.fsPath}`);
}

function positionOfAny(
  document: vscode.TextDocument,
  needles: readonly string[],
): vscode.Position {
  for (const needle of needles) {
    try {
      return positionOf(document, needle);
    } catch {
      // Try the next fixture-specific anchor.
    }
  }

  const keywordOnly = new Set(["START", "BEGIN", "END", "TERM"]);
  const maxLines = Math.min(document.lineCount, 200);
  for (let line = 0; line < maxLines; line += 1) {
    const text = document.lineAt(line).text;
    const match = /\b[A-Za-z][A-Za-z0-9_']*\b/.exec(text);
    if (!match || keywordOnly.has(match[0].toUpperCase())) continue;
    return new vscode.Position(
      line,
      match.index + Math.floor(match[0].length / 2),
    );
  }

  assert.fail(`Could not find a measurable symbol in ${document.uri.fsPath}`);
}

function measuredRange(document: vscode.TextDocument): vscode.Range {
  if (VIEWPORT_LINE_COUNT > 0) {
    const endLine = Math.min(
      document.lineCount - 1,
      Math.max(0, VIEWPORT_LINE_COUNT - 1),
    );
    return new vscode.Range(
      new vscode.Position(0, 0),
      new vscode.Position(endLine, document.lineAt(endLine).text.length),
    );
  }
  return new vscode.Range(
    document.positionAt(0),
    document.positionAt(document.getText().length),
  );
}

function diagnosticSnapshot(uri: vscode.Uri): DiagnosticSnapshot {
  const diagnostics = vscode.languages.getDiagnostics(uri);
  const parts = diagnostics.map((diag) => {
    const range = diag.range;
    return [
      diag.severity,
      diag.source ?? "",
      typeof diag.code === "object"
        ? JSON.stringify(diag.code)
        : String(diag.code ?? ""),
      diag.message,
      range.start.line,
      range.start.character,
      range.end.line,
      range.end.character,
    ].join("|");
  });
  return {
    count: diagnostics.length,
    fingerprint: parts.join("\n"),
    messages: diagnostics.slice(0, 8).map((diag) => diag.message),
  };
}

function documentEditState(document: vscode.TextDocument): DocumentEditState {
  const text = document.getText();
  return {
    version: document.version,
    lineCount: document.lineCount,
    firstLine: document.lineAt(0).text,
    startsWithBrokenLine: /^BROKEN LIVE EDIT\r?\n/.test(text),
    textBytes: Buffer.byteLength(text, "utf8"),
  };
}

function logDocumentEditState(
  label: string,
  document: vscode.TextDocument,
): void {
  const state = documentEditState(document);
  console.log(
    `[jovial-e2e] ${label}: version=${state.version} lines=${state.lineCount} bytes=${state.textBytes} startsBroken=${state.startsWithBrokenLine} firstLine=${JSON.stringify(state.firstLine)}`,
  );
}

async function pullDiagnosticSummary(
  uri: vscode.Uri,
): Promise<PulledDiagnosticSummary> {
  try {
    const result = await withTimeout(
      vscode.commands.executeCommand<{
        kind?: unknown;
        count?: unknown;
        messages?: unknown;
      }>("jovial.pullDiagnostics", uri),
      Math.min(DEFAULT_WAIT_TIMEOUT_MS, 5000),
      "jovial.pullDiagnostics",
    );
    const messages = Array.isArray(result?.messages)
      ? result.messages.filter(
          (message): message is string => typeof message === "string",
        )
      : [];
    return {
      kind: typeof result?.kind === "string" ? result.kind : "unknown",
      count:
        typeof result?.count === "number" && Number.isFinite(result.count)
          ? result.count
          : messages.length,
      messages,
    };
  } catch (error) {
    return {
      kind: "error",
      count: 0,
      messages: [],
      error: errorText(error),
    };
  }
}

async function waitForDiagnosticsStable(
  uri: vscode.Uri,
  quietMs = DIAGNOSTIC_QUIET_MS,
  timeoutMs = DEFAULT_WAIT_TIMEOUT_MS,
): Promise<TimedValue<DiagnosticSnapshot>> {
  const started = performance.now();
  const deadline = started + timeoutMs;
  let last = diagnosticSnapshot(uri);
  let stableSince = performance.now();

  while (performance.now() < deadline) {
    await sleep(100);
    const next = diagnosticSnapshot(uri);
    if (next.fingerprint !== last.fingerprint) {
      last = next;
      stableSince = performance.now();
      continue;
    }
    if (performance.now() - stableSince >= quietMs) {
      return {
        durationMs: performance.now() - started,
        value: next,
      };
    }
  }

  throw new Error(
    `Timed out waiting for stable diagnostics for ${uri.toString()}`,
  );
}

async function waitForDiagnosticsChange(
  uri: vscode.Uri,
  previousFingerprint: string,
  quietMs = DIAGNOSTIC_QUIET_MS,
  timeoutMs = DEFAULT_WAIT_TIMEOUT_MS,
): Promise<TimedValue<DiagnosticSnapshot>> {
  const started = performance.now();
  const deadline = started + timeoutMs;
  let last = diagnosticSnapshot(uri);
  let stableSince = performance.now();

  while (performance.now() < deadline) {
    await sleep(100);
    const next = diagnosticSnapshot(uri);
    if (next.fingerprint !== last.fingerprint) {
      last = next;
      stableSince = performance.now();
      continue;
    }
    if (
      next.fingerprint !== previousFingerprint &&
      performance.now() - stableSince >= quietMs
    ) {
      return {
        durationMs: performance.now() - started,
        value: next,
      };
    }
  }

  throw new Error(
    `Timed out waiting for diagnostics to change for ${uri.toString()}`,
  );
}

function diagnosticHaystack(snapshot: DiagnosticSnapshot): string {
  return `${snapshot.fingerprint}\n${snapshot.messages.join("\n")}`;
}

function seededDiagnosticHaystack(
  snapshot: DiagnosticSnapshot,
  pulledMessages: readonly string[],
): string {
  return `${diagnosticHaystack(snapshot)}\n${pulledMessages.join("\n")}`;
}

function missingDiagnosticSubstrings(
  snapshot: DiagnosticSnapshot,
  expectedSubstrings: readonly string[],
): string[] {
  return missingDiagnosticSubstringsFromHaystack(
    diagnosticHaystack(snapshot),
    expectedSubstrings,
  );
}

function missingDiagnosticSubstringsFromHaystack(
  haystack: string,
  expectedSubstrings: readonly string[],
): string[] {
  return expectedSubstrings.filter((needle) => !haystack.includes(needle));
}

async function waitForDiagnosticsMatching(
  uri: vscode.Uri,
  expectedSubstrings: readonly string[],
  quietMs = DIAGNOSTIC_QUIET_MS,
  timeoutMs = DEFAULT_WAIT_TIMEOUT_MS,
): Promise<TimedValue<DiagnosticSnapshot>> {
  const started = performance.now();
  const deadline = started + timeoutMs;
  let last = diagnosticSnapshot(uri);
  let stableSince = performance.now();

  while (performance.now() < deadline) {
    await sleep(100);
    const next = diagnosticSnapshot(uri);
    if (next.fingerprint !== last.fingerprint) {
      last = next;
      stableSince = performance.now();
      continue;
    }

    const missing = missingDiagnosticSubstrings(next, expectedSubstrings);
    const hasExpected =
      expectedSubstrings.length > 0 ? missing.length === 0 : next.count > 0;
    if (hasExpected && performance.now() - stableSince >= quietMs) {
      return {
        durationMs: performance.now() - started,
        value: next,
      };
    }
  }

  const missing = missingDiagnosticSubstrings(last, expectedSubstrings);
  throw new Error(
    `Timed out waiting for seeded diagnostics for ${uri.toString()}. Missing: ${missing.join("; ")}`,
  );
}

async function insertBrokenLine(document: vscode.TextDocument): Promise<void> {
  const edit = new vscode.WorkspaceEdit();
  edit.insert(document.uri, new vscode.Position(0, 0), "BROKEN LIVE EDIT\n");
  assert.equal(await vscode.workspace.applyEdit(edit), true);
  logDocumentEditState("after broken edit document", document);
}

async function removeFirstLine(document: vscode.TextDocument): Promise<void> {
  const edit = new vscode.WorkspaceEdit();
  edit.delete(document.uri, document.lineAt(0).rangeIncludingLineBreak);
  assert.equal(await vscode.workspace.applyEdit(edit), true);
  logDocumentEditState("after restore edit document", document);
}

async function activateWithDocument(
  samplePath: string,
): Promise<TimedValue<vscode.TextDocument>> {
  return timed(async () => {
    const document = await openJovialDocument(samplePath);
    await vscode.window.showTextDocument(document);

    const extension = vscode.extensions.getExtension(EXTENSION_ID);
    assert.ok(extension, `Extension '${EXTENSION_ID}' was not found`);
    if (!extension.isActive) {
      await extension.activate();
    }
    return document;
  });
}

async function openJovialDocument(
  samplePath: string,
): Promise<vscode.TextDocument> {
  let document = await vscode.workspace.openTextDocument(
    vscode.Uri.file(samplePath),
  );
  if (document.languageId !== "jovial") {
    document = await vscode.languages.setTextDocumentLanguage(
      document,
      "jovial",
    );
  }
  return document;
}

function diagnosticMeasurement(
  name: string,
  durationMs: number,
  snapshot: DiagnosticSnapshot,
  error?: unknown,
): DiagnosticMeasurement {
  return {
    name,
    durationMs: roundMs(durationMs),
    ...snapshot,
    ...(error === undefined ? {} : { error: errorText(error) }),
  };
}

function startupRecordFromReport(
  report: unknown,
): Record<string, unknown> | undefined {
  return objectRecord(objectRecord(report)?.["startup"]);
}

function startupComponentsRecord(
  startup: Record<string, unknown> | undefined,
): Record<string, unknown> | undefined {
  return objectRecord(startup?.["components"]);
}

function startupSemanticReady(
  startup: Record<string, unknown> | undefined,
): boolean {
  const components = startupComponentsRecord(startup);
  return (
    startup?.["isReady"] === true ||
    components?.["deepSemanticComplete"] === true ||
    components?.["fullyNavigable"] === true
  );
}

async function fetchDebugReport(
  document: vscode.TextDocument,
): Promise<unknown> {
  return withTimeout(
    vscode.commands.executeCommand(
      "jovial.debugReport",
      document.uri.toString(),
      12,
    ),
    Math.min(DEFAULT_WAIT_TIMEOUT_MS, 5000),
    "jovial.debugReport",
  );
}

async function measureWorkspaceSemanticReady(
  document: vscode.TextDocument,
  strict: boolean,
): Promise<WorkspaceReadinessMeasurement> {
  const started = performance.now();
  const deadline = started + DEFAULT_WAIT_TIMEOUT_MS;
  const errors: string[] = [];
  let lastStartup: Record<string, unknown> | undefined;

  while (performance.now() < deadline) {
    try {
      const report = await fetchDebugReport(document);
      lastStartup = startupRecordFromReport(report);
      if (startupSemanticReady(lastStartup)) {
        return {
          name: "workspace.deepSemanticReady",
          durationMs: roundMs(performance.now() - started),
          ready: true,
          elapsedMs: finiteNumber(lastStartup?.["elapsedMs"]),
          targetMs: finiteNumber(lastStartup?.["targetMs"]),
          startup: lastStartup,
          errors: [],
        };
      }
    } catch (error) {
      const text = errorText(error);
      if (!errors.includes(text)) errors.push(text);
    }
    await sleep(250);
  }

  if (strict) {
    errors.push(
      `Timed out waiting for full workspace navigation readiness after ${DEFAULT_WAIT_TIMEOUT_MS}ms.`,
    );
  }
  return {
    name: "workspace.deepSemanticReady",
    durationMs: roundMs(performance.now() - started),
    ready: false,
    elapsedMs: finiteNumber(lastStartup?.["elapsedMs"]),
    targetMs: finiteNumber(lastStartup?.["targetMs"]),
    startup: lastStartup,
    errors,
  };
}

async function captureDiagnosticStep(
  name: string,
  uri: vscode.Uri,
  waitForSnapshot: () => Promise<TimedValue<DiagnosticSnapshot>>,
): Promise<DiagnosticMeasurement> {
  const started = performance.now();
  try {
    const result = await waitForSnapshot();
    return diagnosticMeasurement(name, result.durationMs, result.value);
  } catch (error) {
    return diagnosticMeasurement(
      name,
      performance.now() - started,
      diagnosticSnapshot(uri),
      error,
    );
  }
}

async function measureDiagnostics(document: vscode.TextDocument): Promise<{
  baseline: DiagnosticMeasurement;
  brokenEdit: DiagnosticMeasurement;
  restoredEdit: DiagnosticMeasurement;
  restoredPull?: PulledDiagnosticSummary;
  editStates: {
    baseline: DocumentEditState;
    broken: DocumentEditState;
    restored: DocumentEditState;
  };
  restoredMatchesBaseline: boolean;
  errors: string[];
}> {
  console.log("[jovial-e2e] diagnostics baseline: start");
  logDocumentEditState("baseline document", document);
  const baselineEditState = documentEditState(document);
  const baseline = await captureDiagnosticStep(
    "diagnostics.baselineStable",
    document.uri,
    () => waitForDiagnosticsStable(document.uri),
  );
  console.log(
    `[jovial-e2e] diagnostics baseline: count=${baseline.count} error=${baseline.error ?? ""}`,
  );

  console.log("[jovial-e2e] diagnostics broken edit: apply");
  await insertBrokenLine(document);
  const brokenEditState = documentEditState(document);
  console.log("[jovial-e2e] diagnostics broken edit: wait");
  const broken = await captureDiagnosticStep(
    "diagnostics.afterBrokenEdit",
    document.uri,
    () => waitForDiagnosticsChange(document.uri, baseline.fingerprint),
  );
  console.log(
    `[jovial-e2e] diagnostics broken edit: count=${broken.count} error=${broken.error ?? ""}`,
  );

  console.log("[jovial-e2e] diagnostics restore edit: apply");
  await removeFirstLine(document);
  const restoredEditState = documentEditState(document);
  console.log("[jovial-e2e] diagnostics restore edit: wait");
  const restored = await captureDiagnosticStep(
    "diagnostics.afterRestoreEdit",
    document.uri,
    () => waitForDiagnosticsChange(document.uri, broken.fingerprint),
  );
  console.log(
    `[jovial-e2e] diagnostics restore edit: count=${restored.count} error=${restored.error ?? ""}`,
  );
  const restoredPull = restored.error
    ? await pullDiagnosticSummary(document.uri)
    : undefined;
  if (restoredPull) {
    console.log(
      `[jovial-e2e] diagnostics restore pull: kind=${restoredPull.kind} count=${restoredPull.count} error=${restoredPull.error ?? ""}`,
    );
  }

  const errors = [baseline, broken, restored].flatMap((step) =>
    step.error ? [`${step.name}: ${step.error}`] : [],
  );

  return {
    baseline,
    brokenEdit: broken,
    restoredEdit: restored,
    restoredPull,
    editStates: {
      baseline: baselineEditState,
      broken: brokenEditState,
      restored: restoredEditState,
    },
    restoredMatchesBaseline: restored.fingerprint === baseline.fingerprint,
    errors,
  };
}

function skippedDiagnosticsMeasurement(): {
  baseline: DiagnosticMeasurement;
  brokenEdit: DiagnosticMeasurement;
  restoredEdit: DiagnosticMeasurement;
  restoredPull?: PulledDiagnosticSummary;
  editStates: {
    baseline: DocumentEditState;
    broken: DocumentEditState;
    restored: DocumentEditState;
  };
  restoredMatchesBaseline: boolean;
  errors: string[];
} {
  const snapshot: DiagnosticSnapshot = {
    count: 0,
    fingerprint: "",
    messages: [],
  };
  const step = diagnosticMeasurement("diagnostics.skippedNavigationProfile", 0, snapshot);
  return {
    baseline: step,
    brokenEdit: step,
    restoredEdit: step,
    editStates: {
      baseline: {
        version: 0,
        lineCount: 0,
        firstLine: "",
        startsWithBrokenLine: false,
        textBytes: 0,
      },
      broken: {
        version: 0,
        lineCount: 0,
        firstLine: "",
        startsWithBrokenLine: false,
        textBytes: 0,
      },
      restored: {
        version: 0,
        lineCount: 0,
        firstLine: "",
        startsWithBrokenLine: false,
        textBytes: 0,
      },
    },
    restoredMatchesBaseline: true,
    errors: [],
  };
}

async function measureSeededDiagnostics(
  samplePath: string | undefined,
): Promise<SeededDiagnosticMeasurement | undefined> {
  if (!samplePath) return undefined;

  const expectedSubstrings = parseExpectedDiagnosticSubstrings();
  const uri = vscode.Uri.file(samplePath);
  const started = performance.now();
  try {
    const document = await openJovialDocument(samplePath);
    await vscode.window.showTextDocument(document, { preview: false });
    const refreshErrors: string[] = [];
    try {
      await withTimeout(
        vscode.commands.executeCommand(
          "jovial.refreshDiagnostics",
          document.uri,
        ),
        Math.min(DEFAULT_WAIT_TIMEOUT_MS, 2000),
        "jovial.refreshDiagnostics",
      );
    } catch (error) {
      refreshErrors.push(`diagnostics.seededRefresh: ${errorText(error)}`);
    }
    const pulled = await timed(async () => {
      try {
        const result = await withTimeout(
          vscode.commands.executeCommand<{
            kind?: unknown;
            count?: unknown;
            messages?: unknown;
          }>("jovial.pullDiagnostics", document.uri),
          DEFAULT_WAIT_TIMEOUT_MS,
          "jovial.pullDiagnostics",
        );
        const messages = Array.isArray(result?.messages)
          ? result.messages.filter(
              (message): message is string => typeof message === "string",
            )
          : [];
        return {
          kind: typeof result?.kind === "string" ? result.kind : "unknown",
          count:
            typeof result?.count === "number" && Number.isFinite(result.count)
              ? result.count
              : messages.length,
          messages,
        };
      } catch (error) {
        return {
          kind: "error",
          count: 0,
          messages: [] as string[],
          error: errorText(error),
        };
      }
    });
    let baseline = await captureDiagnosticStep(
      "diagnostics.seededVisibleStable",
      document.uri,
      () => waitForDiagnosticsStable(document.uri, DIAGNOSTIC_QUIET_MS, 2000),
    );
    let haystack = seededDiagnosticHaystack(
      baseline,
      pulled.value.messages,
    );
    if (missingDiagnosticSubstringsFromHaystack(haystack, expectedSubstrings).length > 0) {
      baseline = await captureDiagnosticStep(
        "diagnostics.seededVisibleMatching",
        document.uri,
        () => waitForDiagnosticsMatching(document.uri, expectedSubstrings),
      );
      haystack = seededDiagnosticHaystack(baseline, pulled.value.messages);
    }
    const matchedSubstrings = expectedSubstrings.filter((needle) =>
      haystack.includes(needle),
    );
    const missingSubstrings = expectedSubstrings.filter(
      (needle) => !matchedSubstrings.includes(needle),
    );
    const errors = [
      ...refreshErrors,
      ...(baseline.error ? [`${baseline.name}: ${baseline.error}`] : []),
      ...(pulled.value.error && missingSubstrings.length > 0
        ? [`diagnostics.seededPull: ${pulled.value.error}`]
        : []),
      ...(missingSubstrings.length > 0
        ? [`Missing seeded diagnostics: ${missingSubstrings.join("; ")}`]
        : []),
    ];
    return {
      samplePath,
      languageId: document.languageId,
      textLength: document.getText().length,
      pulled: {
        durationMs: roundMs(pulled.durationMs),
        ...pulled.value,
      },
      expectedSubstrings,
      matchedSubstrings,
      missingSubstrings,
      baseline,
      errors,
    };
  } catch (error) {
    return {
      samplePath,
      languageId: "",
      textLength: 0,
      pulled: {
        durationMs: 0,
        kind: "not-run",
        count: 0,
        messages: [],
      },
      expectedSubstrings,
      matchedSubstrings: [],
      missingSubstrings: expectedSubstrings,
      baseline: diagnosticMeasurement(
        "diagnostics.seededStable",
        performance.now() - started,
        diagnosticSnapshot(uri),
        error,
      ),
      errors: [errorText(error)],
    };
  }
}

async function runBenchmark(): Promise<void> {
  console.log("[jovial-e2e] benchmark: start");
  const samplePath = requireEnv("JOVIAL_E2E_SAMPLE", SAMPLE_PATH);
  const workspacePath = requireEnv("JOVIAL_E2E_WORKSPACE", WORKSPACE_PATH);
  const reportPath = requireEnv("JOVIAL_E2E_REPORT", REPORT_PATH);
  const serverPath = requireEnv("JOVIAL_E2E_SERVER_PATH", SERVER_PATH);

  const activationPath = DIAGNOSTIC_SAMPLE_PATH ?? samplePath;
  console.log(`[jovial-e2e] activation document: ${activationPath}`);
  const activation = await activateWithDocument(activationPath);
  console.log(`[jovial-e2e] activation done: ${roundMs(activation.durationMs)}ms`);
  const seededDiagnostics = await measureSeededDiagnostics(
    DIAGNOSTIC_SAMPLE_PATH,
  );
  const sampleOpen =
    activationPath === samplePath
      ? activation
      : await timed(async () => {
          const sampleDocument = await openJovialDocument(samplePath);
          await vscode.window.showTextDocument(sampleDocument, {
            preview: false,
          });
          return sampleDocument;
        });
  const document = sampleOpen.value;
  console.log("[jovial-e2e] workspace readiness: start");
  const strictWorkspaceReadiness = E2E_PROFILE === "mixed";
  const workspaceReadiness = await measureWorkspaceSemanticReady(
    document,
    strictWorkspaceReadiness,
  );
  console.log(
    `[jovial-e2e] workspace readiness: ready=${workspaceReadiness.ready} elapsed=${workspaceReadiness.elapsedMs ?? ""}`,
  );
  const symbolPosition = positionOfAny(document, [
    "SPEED",
    "LOCAL",
    "TEN",
    "LIMIT",
    "VALUE",
  ]);
  const callPosition = positionOfAny(document, [
    "FIND",
    "SCALE",
    "MAIN",
    "LOCAL",
    "TEN",
  ]);
  const providerRange = measuredRange(document);

  // These commands are invoked through VS Code, so their timings include the
  // extension host, LanguageClient middleware, JSON-RPC transport, and server.
  const commands = [
    await measureCommand<readonly vscode.DocumentSymbol[]>(
      "documentSymbols",
      "vscode.executeDocumentSymbolProvider",
      COMMAND_SAMPLES,
      document.uri,
    ),
    await measureCommand<readonly vscode.Hover[]>(
      "hover",
      "vscode.executeHoverProvider",
      HOVER_SAMPLES,
      document.uri,
      symbolPosition,
    ),
    await measureCommand<
      | vscode.Location
      | vscode.LocationLink
      | readonly vscode.Location[]
      | readonly vscode.LocationLink[]
      | undefined
    >(
      "definition",
      "vscode.executeDefinitionProvider",
      NAV_SAMPLES,
      document.uri,
      symbolPosition,
    ),
    await measureCommand<readonly vscode.Location[]>(
      "references",
      "vscode.executeReferenceProvider",
      NAV_SAMPLES,
      document.uri,
      symbolPosition,
    ),
    await measureCommand<readonly vscode.InlayHint[]>(
      "inlayHints",
      "vscode.executeInlayHintProvider",
      COMMAND_SAMPLES,
      document.uri,
      providerRange,
    ),
    await measureCommand<vscode.SemanticTokens>(
      "semanticTokens.range",
      "vscode.provideDocumentRangeSemanticTokens",
      COMMAND_SAMPLES,
      document.uri,
      providerRange,
    ),
    await measureCommand<readonly vscode.CompletionItem[]>(
      "completion",
      "vscode.executeCompletionItemProvider",
      COMMAND_SAMPLES,
      document.uri,
      callPosition,
    ),
  ];

  const diagnostics =
    E2E_PROFILE === "mixed"
      ? skippedDiagnosticsMeasurement()
      : await measureDiagnostics(document);
  console.log("[jovial-e2e] diagnostics phase: done");
  const failedCommands = commands.filter(
    (command) => command.errors.length > 0,
  );
  const emptyProviderResults = emptyMixedProviderResults(commands);
  const strictDiagnostics =
    process.env.JOVIAL_E2E_STRICT_DIAGNOSTICS === "1" ||
    (E2E_PROFILE !== "huge" && E2E_PROFILE !== "mixed");

  const report = {
    tool: "jovial-vscode-e2e-perf",
    generatedAt: new Date().toISOString(),
    note: "Measures VS Code extension-host UX path: extension activation, LanguageClient processing, stdio JSON-RPC, real OCaml LSP server processing, provider command results, and unsaved edit-to-diagnostics latency.",
    workspacePath,
    workspaceFolders: workspaceFoldersReport(),
    workspaceInventory: workspaceInventory(workspacePath),
    samplePath,
    serverPath,
    profile: E2E_PROFILE,
    sampleBytes: SAMPLE_BYTES,
    hugeThresholdBytes: HUGE_THRESHOLD_BYTES,
    fullParseMaxBytes: FULL_PARSE_MAX_BYTES,
    commandSamples: COMMAND_SAMPLES,
    hoverSamples: HOVER_SAMPLES,
    navigationSamples: NAV_SAMPLES,
    providerRange: {
      start: {
        line: providerRange.start.line,
        character: providerRange.start.character,
      },
      end: {
        line: providerRange.end.line,
        character: providerRange.end.character,
      },
    },
    activation: {
      name: "openDocument.extensionActivate.serverInitialize",
      durationMs: roundMs(activation.durationMs),
      samplePath: activationPath,
    },
    sampleOpen: {
      name:
        activationPath === samplePath
          ? "sample.openedDuringActivation"
          : "sample.openDocument.afterSeededDiagnostics",
      durationMs:
        activationPath === samplePath ? 0 : roundMs(sampleOpen.durationMs),
      samplePath,
    },
    commands,
    diagnostics,
    seededDiagnostics,
    workspaceReadiness,
    strictWorkspaceReadiness,
    summary: {
      commandErrors: failedCommands.map((command) => ({
        name: command.name,
        errors: command.errors,
      })),
      emptyProviderResults,
      diagnosticsRestoredToBaseline: diagnostics.restoredMatchesBaseline,
      diagnosticErrors: diagnostics.errors,
      seededDiagnosticErrors: seededDiagnostics?.errors ?? [],
      workspaceReadinessErrors: strictWorkspaceReadiness
        ? workspaceReadiness.errors
        : [],
    },
  };

  fs.writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`, "utf8");
  console.log(`[jovial-e2e] report written: ${reportPath}`);

  assert.equal(
    failedCommands.length,
    0,
    `Provider commands failed: ${failedCommands.map((command) => command.name).join(", ")}`,
  );
  assert.deepEqual(
    emptyProviderResults,
    [],
    `Mixed workspace provider commands returned empty results: ${emptyProviderResults.join(", ")}`,
  );
  if (E2E_PROFILE === "mixed") {
    assert.equal(
      workspaceReadiness.ready,
      true,
      `Workspace did not become fully navigable: ${workspaceReadiness.errors.join("; ")}`,
    );
    const readinessElapsed =
      workspaceReadiness.elapsedMs ?? workspaceReadiness.durationMs;
    assert.ok(
      readinessElapsed <= DEFAULT_WAIT_TIMEOUT_MS,
      `Workspace full navigation readiness took ${readinessElapsed}ms, expected <= ${DEFAULT_WAIT_TIMEOUT_MS}ms.`,
    );
  }
  if (strictDiagnostics) {
    assert.deepEqual(diagnostics.errors, [], "Diagnostics timing failed.");
    assert.ok(
      diagnostics.brokenEdit.fingerprint !== diagnostics.baseline.fingerprint,
      "Broken live edit did not change diagnostics.",
    );
    assert.ok(
      diagnostics.restoredMatchesBaseline,
      "Diagnostics did not return to the baseline snapshot after restoring the edit.",
    );
    if (seededDiagnostics) {
      assert.deepEqual(
        seededDiagnostics.errors,
        [],
        "Seeded diagnostics did not match expected messages.",
      );
      assert.ok(
        seededDiagnostics.baseline.count + seededDiagnostics.pulled.count > 0,
        "Seeded diagnostic fixture did not produce any diagnostics.",
      );
    }
  }
}

export async function run(): Promise<void> {
  await runBenchmark();
}
