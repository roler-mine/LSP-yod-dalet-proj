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
      if (!event.affectsConfiguration("jovial")) return;
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

      const serverConfigChanged =
        event.affectsConfiguration("jovial.server.path") ||
        event.affectsConfiguration("jovial.server.preferBundled") ||
        event.affectsConfiguration("jovial.server.args") ||
        event.affectsConfiguration("jovial.server.parseMaxFileBytes") ||
        event.affectsConfiguration("jovial.server.pressureSoftMb") ||
        event.affectsConfiguration("jovial.server.pressureCriticalMb") ||
        event.affectsConfiguration("jovial.workspaceDiagnostics.mode") ||
        event.affectsConfiguration("jovial.workspace.profileMode") ||
        event.affectsConfiguration("jovial.workspace.rootModel") ||
        event.affectsConfiguration("jovial.workspace.manualRootFiles") ||
        event.affectsConfiguration("jovial.background.indexBudgetMs") ||
        event.affectsConfiguration("jovial.background.diagBatchSize");

      if (serverConfigChanged) {
        output.appendLine(
          "Jovial server configuration changed; restarting server.",
        );
        if (cfg.autostart) {
          await startClient(context, output, status, "config-change");
        } else {
          await stopClient(status);
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
