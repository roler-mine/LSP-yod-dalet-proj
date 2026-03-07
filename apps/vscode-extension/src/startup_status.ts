import * as path from "path";
import * as vscode from "vscode";

import type { LanguageClient } from "vscode-languageclient/node";

export type StatusKind = "starting" | "running" | "stopped" | "error";

type StartupStage = "diagHoverReady" | "fullyNavigable";
type RootStartupStatus = {
  phase?: string;
  phaseElapsedMs?: number;
  phaseTargetMs?: number;
  diagReadyElapsedMs?: number;
  diagReadyTargetMs?: number;
  navReadyElapsedMs?: number;
  navReadyTargetMs?: number;
  diagMissElapsedMs?: number;
  diagMissTargetMs?: number;
  navMissElapsedMs?: number;
  navMissTargetMs?: number;
};

export const STARTUP_DIAG_TARGET_DEFAULT_MS = 15000;
export const STARTUP_NAV_TARGET_DEFAULT_MS = 30000;

const startupStatusByRoot = new Map<string, RootStartupStatus>();

function asRecord(v: unknown): Record<string, unknown> | undefined {
  return v !== null && typeof v === "object"
    ? (v as Record<string, unknown>)
    : undefined;
}

function asFiniteInt(value: unknown): number | undefined {
  if (typeof value !== "number" || !Number.isFinite(value)) return undefined;
  return Math.max(0, Math.trunc(value));
}

function normalizeRootUri(value: unknown): string | undefined {
  if (typeof value !== "string" || value.trim() === "") return undefined;
  try {
    return vscode.Uri.parse(value).toString();
  } catch {
    return undefined;
  }
}

function activeRootUriKey(): string | undefined {
  const activeDoc = vscode.window.activeTextEditor?.document;
  if (activeDoc?.uri.scheme === "file") {
    const folder = vscode.workspace.getWorkspaceFolder(activeDoc.uri);
    if (folder) return folder.uri.toString();
    return activeDoc.uri.toString();
  }
  const firstFolder = vscode.workspace.workspaceFolders?.[0];
  if (firstFolder) return firstFolder.uri.toString();
  return undefined;
}

function rootLabel(rootUri: string): string {
  try {
    const uri = vscode.Uri.parse(rootUri);
    if (uri.scheme === "file") {
      const base = path.basename(uri.fsPath);
      if (base) return base;
    }
  } catch {
    // Keep fallback below.
  }
  return rootUri;
}

function rootStartupState(rootUri: string): RootStartupStatus {
  const prev = startupStatusByRoot.get(rootUri);
  if (prev) return prev;
  const next: RootStartupStatus = {};
  startupStatusByRoot.set(rootUri, next);
  return next;
}

function startupPhaseLabel(phase: string | undefined): string {
  if (!phase) return "warming";
  if (phase === "aggressiveCatchUp") return "aggressive catch-up";
  return phase.toLowerCase();
}

export function setStatus(
  status: vscode.StatusBarItem,
  kind: StatusKind,
  detail?: string,
): void {
  switch (kind) {
    case "starting":
      status.text = `$(sync~spin) Jovial LSP: starting...`;
      status.color = "#ffd24d";
      status.tooltip = detail ?? "Starting Jovial LSP (click to restart)";
      break;
    case "running":
      status.text = `$(check) Jovial LSP: running`;
      status.color = "#4dff88";
      status.tooltip = detail ?? "Jovial LSP running (click to restart)";
      break;
    case "stopped":
      status.text = `$(circle-slash) Jovial LSP: stopped`;
      status.color = "#cccccc";
      status.tooltip = detail ?? "Jovial LSP stopped (click to start)";
      break;
    case "error":
      status.text = `$(error) Jovial LSP: error`;
      status.color = "#ff4d4d";
      status.tooltip = detail ?? "Jovial LSP error (click to restart)";
      break;
  }
}

export function clearStartupStatus(): void {
  startupStatusByRoot.clear();
}

export function refreshStartupStatusBar(status: vscode.StatusBarItem): void {
  const rootUri = activeRootUriKey();
  if (!rootUri) return;
  const state = startupStatusByRoot.get(rootUri);
  if (!state) return;
  const label = rootLabel(rootUri);

  if (state.navReadyElapsedMs !== undefined) {
    const target = state.navReadyTargetMs ?? STARTUP_NAV_TARGET_DEFAULT_MS;
    setStatus(
      status,
      "running",
      `Fully navigable in ${state.navReadyElapsedMs}ms (${label}, target ${target}ms)`,
    );
    return;
  }
  if (
    state.navMissElapsedMs !== undefined ||
    state.navMissTargetMs !== undefined
  ) {
    const elapsed = state.navMissElapsedMs ?? 0;
    const target = state.navMissTargetMs ?? STARTUP_NAV_TARGET_DEFAULT_MS;
    setStatus(
      status,
      "error",
      `Startup miss (fully navigable): ${elapsed}ms > ${target}ms (${label})`,
    );
    return;
  }
  if (state.diagReadyElapsedMs !== undefined) {
    const target = state.diagReadyTargetMs ?? STARTUP_DIAG_TARGET_DEFAULT_MS;
    setStatus(
      status,
      "running",
      `Diag/Hover ready in ${state.diagReadyElapsedMs}ms (${label}, target ${target}ms); warming full nav`,
    );
    return;
  }
  if (
    state.diagMissElapsedMs !== undefined ||
    state.diagMissTargetMs !== undefined
  ) {
    const elapsed = state.diagMissElapsedMs ?? 0;
    const target = state.diagMissTargetMs ?? STARTUP_DIAG_TARGET_DEFAULT_MS;
    setStatus(
      status,
      "error",
      `Startup miss (diag/hover): ${elapsed}ms > ${target}ms (${label})`,
    );
    return;
  }

  const phaseLabel = startupPhaseLabel(state.phase);
  const elapsed = state.phaseElapsedMs;
  const target = state.phaseTargetMs ?? STARTUP_NAV_TARGET_DEFAULT_MS;
  if (elapsed === undefined) {
    setStatus(status, "running", `Startup ${phaseLabel} (${label})`);
  } else {
    setStatus(
      status,
      "running",
      `Startup ${phaseLabel} ${elapsed}ms / ${target}ms (${label})`,
    );
  }
}

export function bindStartupNotifications({
  client,
  output,
  status,
  startupDiagTargetMs,
  startupNavTargetMs,
}: {
  client: LanguageClient;
  output: vscode.OutputChannel;
  status: vscode.StatusBarItem;
  startupDiagTargetMs: number;
  startupNavTargetMs: number;
}): void {
  client.onNotification("jovial/workspaceStartupPhase", (params: unknown) => {
    const obj = asRecord(params);
    const rootUri = normalizeRootUri(obj?.["rootUri"]);
    if (!rootUri) return;
    const state = rootStartupState(rootUri);
    const phaseRaw =
      typeof obj?.["phase"] === "string" ? obj["phase"] : "warming";
    const elapsedMs = asFiniteInt(obj?.["elapsedMs"]);
    const targetMs = asFiniteInt(obj?.["targetMs"]) ?? startupNavTargetMs;
    state.phase = phaseRaw;
    state.phaseElapsedMs = elapsedMs;
    state.phaseTargetMs = targetMs;
    const phaseLabel = startupPhaseLabel(phaseRaw);
    const detail =
      elapsedMs === undefined
        ? `Startup ${phaseLabel} (${rootLabel(rootUri)})`
        : `Startup ${phaseLabel} ${elapsedMs}ms / ${targetMs}ms (${rootLabel(rootUri)})`;
    output.appendLine(`workspaceStartupPhase: ${detail}`);
    refreshStartupStatusBar(status);
  });

  client.onNotification("jovial/workspaceStartupMiss", (params: unknown) => {
    const obj = asRecord(params);
    const rootUri = normalizeRootUri(obj?.["rootUri"]);
    if (!rootUri) return;
    const state = rootStartupState(rootUri);
    const stage =
      obj?.["stage"] === "diagHoverReady" || obj?.["stage"] === "fullyNavigable"
        ? (obj["stage"] as StartupStage)
        : "fullyNavigable";
    const elapsedMs = asFiniteInt(obj?.["elapsedMs"]);
    const targetMs =
      asFiniteInt(obj?.["targetMs"]) ??
      (stage === "diagHoverReady" ? startupDiagTargetMs : startupNavTargetMs);
    if (stage === "diagHoverReady") {
      state.diagMissElapsedMs = elapsedMs;
      state.diagMissTargetMs = targetMs;
    } else {
      state.navMissElapsedMs = elapsedMs;
      state.navMissTargetMs = targetMs;
    }
    const detail =
      elapsedMs === undefined
        ? `Startup missed ${stage} target (${targetMs}ms, ${rootLabel(rootUri)})`
        : `Startup missed ${stage} target: ${elapsedMs}ms > ${targetMs}ms (${rootLabel(rootUri)})`;
    output.appendLine(`workspaceStartupMiss: ${detail}`);
    refreshStartupStatusBar(status);
  });

  client.onNotification("jovial/workspaceReady", (params: unknown) => {
    const obj = asRecord(params);
    const rootUri = normalizeRootUri(obj?.["rootUri"]);
    if (!rootUri) return;
    const state = rootStartupState(rootUri);
    const stage =
      obj?.["stage"] === "diagHoverReady" || obj?.["stage"] === "fullyNavigable"
        ? (obj["stage"] as StartupStage)
        : "fullyNavigable";
    const readiness = asRecord(obj?.["readiness"]);
    const stages = asRecord(readiness?.["stages"]);
    const stagePayload = asRecord(stages?.[stage]);
    const elapsedMs = asFiniteInt(
      stagePayload?.["elapsedMs"] ?? readiness?.["elapsedMs"],
    );
    const targetMs =
      asFiniteInt(stagePayload?.["targetMs"] ?? readiness?.["targetMs"]) ??
      (stage === "diagHoverReady" ? startupDiagTargetMs : startupNavTargetMs);
    const readyWithinTarget = stagePayload?.["readyWithinTarget"] === true;
    if (stage === "diagHoverReady") {
      state.diagReadyElapsedMs = elapsedMs;
      state.diagReadyTargetMs = targetMs;
      state.diagMissElapsedMs = undefined;
      state.diagMissTargetMs = undefined;
    } else {
      state.navReadyElapsedMs = elapsedMs;
      state.navReadyTargetMs = targetMs;
      state.navMissElapsedMs = undefined;
      state.navMissTargetMs = undefined;
    }
    const detail =
      elapsedMs === undefined
        ? `Startup ready (${stage}, ${rootLabel(rootUri)})`
        : `${stage} ready in ${elapsedMs}ms (${readyWithinTarget ? "within" : "over"} ${targetMs}ms, ${rootLabel(rootUri)})`;
    output.appendLine(`workspaceReady: ${detail}`);
    refreshStartupStatusBar(status);
  });
}
