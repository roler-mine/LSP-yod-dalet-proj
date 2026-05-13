import * as vscode from "vscode";

import type { JovialConfig } from "./jovial_config";
import type { StatusKind } from "./startup_status";

type CommandDeps = {
  context: vscode.ExtensionContext;
  output: vscode.OutputChannel;
  status: vscode.StatusBarItem;
  getConfig: () => JovialConfig;
  setStatus: (
    status: vscode.StatusBarItem,
    kind: StatusKind,
    detail?: string,
  ) => void;
  startClient: (
    context: vscode.ExtensionContext,
    output: vscode.OutputChannel,
    status: vscode.StatusBarItem,
    source?: "manual" | "auto-restart" | "config-change",
  ) => Promise<void>;
  stopClient: (status: vscode.StatusBarItem) => Promise<void>;
  refreshLsifIndex: (
    output: vscode.OutputChannel,
    reason: string,
    preferredUri?: vscode.Uri,
  ) => Promise<void>;
  firstPreferredLsifUri: () => vscode.Uri | undefined;
  scheduleLsifRefresh: (
    output: vscode.OutputChannel,
    reason: string,
    preferredUri?: vscode.Uri,
    delayMs?: number,
  ) => void;
  refreshStartupStatusBar: (status: vscode.StatusBarItem) => void;
  resetLsifState: () => void;
  applyTraceSetting: (output: vscode.OutputChannel) => void;
  dumpAstUi: () => Promise<void>;
  dumpCstUi: () => Promise<void>;
  showSyntaxTreesUi: () => Promise<void>;
};

const serverRestartConfigKeys = [
  "jovial.server",
  "jovial.server.path",
  "jovial.server.preferBundled",
  "jovial.server.args",
  "jovial.server.inlayHints",
  "jovial.server.inlayhints",
  "jovial.server.InlayHint",
  "jovial.server.parseMaxFileBytes",
  "jovial.server.pressureSoftMb",
  "jovial.server.pressureCriticalMb",
  "jovial.workspaceDiagnostics",
  "jovial.workspaceDiagnostics.mode",
  "jovial.workspace",
  "jovial.workspace.profileMode",
  "jovial.workspace.rootModel",
  "jovial.workspace.manualRootFiles",
  "jovial.workspace.maxStartupFiles",
  "jovial.workspace.extraSourceFileExtensions",
  "jovial.background",
  "jovial.background.indexBudgetMs",
  "jovial.background.diagBatchSize",
  "jovial.features.profile",
  "jovial.features.custom",
  "jovial.features.custom.enabledFeatures",
  "jovial.features.overrides.documentSymbols",
  "jovial.features.overrides.workspaceSymbols",
  "jovial.features.overrides.hover",
  "jovial.features.overrides.signatureHelp",
  "jovial.features.overrides.completion",
  "jovial.features.overrides.codeActions",
  "jovial.features.overrides.codeLens",
  "jovial.features.overrides.inlayHints",
  "jovial.features.overrides.formatting",
  "jovial.features.overrides.semanticTokens",
  "jovial.features.definition",
  "jovial.features.declaration",
  "jovial.features.typeDefinition",
  "jovial.features.implementation",
  "jovial.features.references",
  "jovial.features.documentSymbols",
  "jovial.features.workspaceSymbols",
  "jovial.features.hover",
  "jovial.features.signatureHelp",
  "jovial.features.rename",
  "jovial.features.completion",
  "jovial.features.codeActions",
  "jovial.features.codeLens",
  "jovial.features.inlayHints",
  "jovial.features.formatting",
  "jovial.features.semanticTokens",
  "jovial.startup",
  "jovial.startup.priorityMode",
  "jovial.performance",
  "jovial.performance.priorityMode",
  "jovial.performance.largeFileThresholdBytes",
  "jovial.performance.hugeFileThresholdBytes",
  "jovial.performance.fullSemanticTokensMaxBytes",
  "jovial.performance.fullParseMaxBytes",
  "jovial.performance.enableHugeFileFullParse",
  "jovial.performance.backgroundParseWorkerCount",
] as const;

const inlayHintConfigKeys = [
  "jovial.features.profile",
  "jovial.features.custom",
  "jovial.features.custom.enabledFeatures",
  "jovial.features.overrides.inlayHints",
  "jovial.features.inlayHints",
  "jovial.server.inlayHints",
  "jovial.server.inlayhints",
  "jovial.server.InlayHint",
] as const;

const liveConfigKeys = [
  "jovial",
  "jovial.trace",
  "jovial.lsif.fastPath",
  "jovial.autostart",
  ...serverRestartConfigKeys,
] as const;

function configurationAffectsAny(
  event: vscode.ConfigurationChangeEvent,
  sections: readonly string[],
): boolean {
  return sections.some((section) => event.affectsConfiguration(section));
}

async function refreshEditorInlayHints(
  output: vscode.OutputChannel,
): Promise<void> {
  try {
    await vscode.commands.executeCommand("editor.action.inlayHints.refresh");
  } catch (e) {
    output.appendLine(`refresh inlay hints failed: ${String(e)}`);
  }
}

export function registerExtensionHooks({
  context,
  output,
  status,
  getConfig,
  setStatus,
  startClient,
  stopClient,
  refreshLsifIndex,
  firstPreferredLsifUri,
  scheduleLsifRefresh,
  refreshStartupStatusBar,
  resetLsifState,
  applyTraceSetting,
  dumpAstUi,
  dumpCstUi,
  showSyntaxTreesUi,
}: CommandDeps): void {
  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.dumpAstUi", async () => {
      try {
        await dumpAstUi();
      } catch (e) {
        output.appendLine(`dumpAstUi failed: ${String(e)}`);
      }
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.dumpCstUi", async () => {
      try {
        await dumpCstUi();
      } catch (e) {
        output.appendLine(`dumpCstUi failed: ${String(e)}`);
      }
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.showSyntaxTrees", async () => {
      try {
        await showSyntaxTreesUi();
      } catch (e) {
        output.appendLine(`showSyntaxTrees failed: ${String(e)}`);
      }
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.restartServer", async () => {
      try {
        await startClient(context, output, status);
      } catch (e) {
        output.appendLine(`restart failed: ${String(e)}`);
        setStatus(status, "error", `restart failed: ${String(e)}`);
      }
    }),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("jovial.refreshLsifCache", async () => {
      try {
        if (!getConfig().lsifFastPath) {
          vscode.window.showInformationMessage(
            "Jovial: LSIF fast path is disabled. Enable jovial.lsif.fastPath to refresh LSIF cache.",
          );
          return;
        }
        await refreshLsifIndex(
          output,
          "manual command",
          firstPreferredLsifUri(),
        );
        vscode.window.showInformationMessage(
          "Jovial: LSIF cache refresh completed.",
        );
      } catch (e) {
        output.appendLine(`refreshLsifCache failed: ${String(e)}`);
        vscode.window.showWarningMessage(
          "Jovial: LSIF cache refresh failed. Check the Jovial LSP output channel.",
        );
      }
    }),
  );

  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration(async (event) => {
      if (!configurationAffectsAny(event, liveConfigKeys)) return;
      const cfg = getConfig();

      if (event.affectsConfiguration("jovial.trace")) {
        applyTraceSetting(output);
      }
      if (event.affectsConfiguration("jovial.lsif.fastPath")) {
        if (cfg.lsifFastPath) {
          output.appendLine(
            "LSIF fast path enabled. Use command 'Jovial: Refresh LSIF Cache' to build the cache.",
          );
        } else {
          resetLsifState();
        }
      }

      const serverConfigChanged = configurationAffectsAny(
        event,
        serverRestartConfigKeys,
      );
      const inlayHintConfigChanged = configurationAffectsAny(
        event,
        inlayHintConfigKeys,
      );

      if (serverConfigChanged) {
        output.appendLine(
          "Jovial server configuration changed; stopping client/server before restart.",
        );
        if (inlayHintConfigChanged) {
          await refreshEditorInlayHints(output);
        }
        await stopClient(status);
        if (cfg.autostart) {
          await startClient(context, output, status, "config-change");
        }
        if (inlayHintConfigChanged) {
          await refreshEditorInlayHints(output);
        }
        return;
      }

      if (event.affectsConfiguration("jovial.autostart")) {
        if (cfg.autostart) {
          await startClient(context, output, status, "config-change");
        } else {
          await stopClient(status);
        }
      }
    }),
  );

  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument((doc) => {
      if (doc.languageId !== "jovial") return;
      scheduleLsifRefresh(output, "didSave", doc.uri, 1000);
    }),
  );

  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor(() => {
      refreshStartupStatusBar(status);
    }),
  );

  context.subscriptions.push({
    dispose: () => {
      void stopClient(status);
    },
  });
}
