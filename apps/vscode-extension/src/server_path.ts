// Module overview: Resolves the Jovial language-server executable from bundled runtime paths or user settings.

import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import * as vscode from "vscode";

import { watchPathKey } from "./workspace_paths";

export type ServerPathSource =
  | "configured"
  | "bundled-exact"
  | "bundled-fallback"
  | "development";

export type ServerPathResolution = {
  path: string | undefined;
  source: ServerPathSource;
  note?: string;
};

function uniquePaths(xs: string[]): string[] {
  const seen = new Set<string>();
  const out: string[] = [];
  for (const x of xs) {
    const norm = path.normalize(x);
    if (!seen.has(norm)) {
      seen.add(norm);
      out.push(norm);
    }
  }
  return out;
}

function stripWrappingQuotes(s: string): string {
  if (s.length >= 2) {
    const a = s[0];
    const b = s[s.length - 1];
    if ((a === '"' && b === '"') || (a === "'" && b === "'")) {
      return s.slice(1, -1);
    }
  }
  return s;
}

function expandEnvVars(s: string): string {
  const a = s.replace(
    /\$\{env:([^}]+)\}/g,
    (_m, name: string) => process.env[name] ?? "",
  );
  return a.replace(/%([^%]+)%/g, (_m, name: string) => process.env[name] ?? "");
}

function expandHomeDir(s: string): string {
  if (s === "~") return os.homedir();
  if (s.startsWith("~/") || s.startsWith("~\\")) {
    return path.join(os.homedir(), s.slice(2));
  }
  return s;
}

function normalizeServerPathForPlatform(s: string): string {
  const norm = path.normalize(s);
  if (process.platform === "win32") {
    return norm.replace(/\//g, "\\");
  }
  return norm;
}

function pushCandidate(candidates: string[], p: string): void {
  if (!p) return;
  candidates.push(normalizeServerPathForPlatform(p));
}

function addRelativeCandidates(
  candidates: string[],
  baseDir: string,
  relPath: string,
  maxParentDepth = 3,
): void {
  let cur = normalizeServerPathForPlatform(baseDir);
  for (let i = 0; i <= maxParentDepth; i += 1) {
    pushCandidate(candidates, path.join(cur, relPath));
    const parent = path.dirname(cur);
    if (parent === cur) break;
    cur = parent;
  }
}

function bundledRuntimeRelPaths(): { exact: string[]; fallback: string[] } {
  if (process.platform === "win32" && process.arch === "arm64") {
    return {
      exact: [path.join("runtime", "server", "win32-arm64", "jovial-lsp.exe")],
      fallback: [path.join("runtime", "server", "win32-x64", "jovial-lsp.exe")],
    };
  }
  if (process.platform === "win32" && process.arch === "x64") {
    return {
      exact: [path.join("runtime", "server", "win32-x64", "jovial-lsp.exe")],
      fallback: [],
    };
  }
  if (process.platform === "linux" && process.arch === "arm64") {
    return {
      exact: [path.join("runtime", "server", "linux-arm64", "jovial-lsp")],
      fallback: [],
    };
  }
  if (process.platform === "linux" && process.arch === "x64") {
    return {
      exact: [path.join("runtime", "server", "linux-x64", "jovial-lsp")],
      fallback: [],
    };
  }
  return { exact: [], fallback: [] };
}

function findExistingCandidate(candidates: string[]): string | undefined {
  return uniquePaths(candidates).find((p) => fs.existsSync(p));
}

function collectRelativeProbeCandidates(
  context: vscode.ExtensionContext,
  folders: readonly vscode.WorkspaceFolder[],
  repoRoot: string | undefined,
  relPaths: string[],
): string[] {
  const out: string[] = [];
  for (const rel of relPaths) {
    pushCandidate(out, context.asAbsolutePath(rel));
    addRelativeCandidates(out, context.extensionPath, rel, 2);
    if (repoRoot) {
      pushCandidate(out, path.join(repoRoot, rel));
    }
    for (const f of folders) {
      addRelativeCandidates(out, f.uri.fsPath, rel, 4);
    }
  }
  return uniquePaths(out);
}

function isRepoRootDir(dir: string): boolean {
  const hasGit = fs.existsSync(path.join(dir, ".git"));
  const hasAppsServer = fs.existsSync(path.join(dir, "apps", "lsp-server"));
  const hasAppsExtension = fs.existsSync(
    path.join(dir, "apps", "vscode-extension"),
  );
  const hasLegacyServer = fs.existsSync(path.join(dir, "server_proj"));
  const hasLegacyExtension = fs.existsSync(path.join(dir, "extension_proj"));
  return (
    hasGit ||
    (hasAppsServer && hasAppsExtension) ||
    (hasLegacyServer && hasLegacyExtension)
  );
}

function findRepoRoot(
  folders: readonly vscode.WorkspaceFolder[],
  extensionPath: string,
): string | undefined {
  const seeds = [
    ...folders.map((f) => f.uri.fsPath),
    extensionPath,
    path.dirname(extensionPath),
  ];

  for (const seed of seeds) {
    let cur = normalizeServerPathForPlatform(seed);
    for (let i = 0; i < 12; i += 1) {
      if (isRepoRootDir(cur)) {
        return normalizeServerPathForPlatform(cur);
      }
      const parent = path.dirname(cur);
      if (parent === cur) break;
      cur = parent;
    }
  }
  return undefined;
}

function expandWorkspaceVars(
  s: string,
  folders: readonly vscode.WorkspaceFolder[],
): string[] {
  const named = s.replace(
    /\$\{workspaceFolder:([^}]+)\}/g,
    (_m, name: string) => {
      const hit = folders.find((f) => f.name === name);
      return hit ? hit.uri.fsPath : "";
    },
  );

  if (!named.includes("${workspaceFolder}")) {
    return [named];
  }

  if (folders.length === 0) {
    return [named.replace(/\$\{workspaceFolder\}/g, "")];
  }

  return folders.map((f) =>
    named.replace(/\$\{workspaceFolder\}/g, f.uri.fsPath),
  );
}

export function resolveServerPath(
  context: vscode.ExtensionContext,
  configured: string,
  preferBundled: boolean,
  preferDevelopment = false,
): ServerPathResolution {
  const cfgRaw = stripWrappingQuotes((configured ?? "").trim());
  const folders = vscode.workspace.workspaceFolders ?? [];
  const repoRoot = findRepoRoot(folders, context.extensionPath);

  const resolveAutoPath = (): ServerPathResolution => {
    const exes =
      process.platform === "win32"
        ? ["jovial-lsp.exe", "Main.exe"]
        : ["jovial-lsp", "Main"];

    const resolveDevelopmentPath = (): ServerPathResolution | undefined => {
      const devRelCandidates = [
        ...exes.map((e) =>
          path.join("apps", "lsp-server", "_build", "default", "bin", e),
        ),
        ...exes.map((e) =>
          path.join(
            "apps",
            "lsp-server",
            "_build",
            "install",
            "default",
            "bin",
            e,
          ),
        ),
        ...exes.map((e) =>
          path.join("lsp-server", "_build", "default", "bin", e),
        ),
        ...exes.map((e) =>
          path.join("lsp-server", "_build", "install", "default", "bin", e),
        ),
        ...exes.map((e) =>
          path.join("server_proj", "_build", "default", "bin", e),
        ),
        ...exes.map((e) =>
          path.join("server_proj", "_build", "install", "default", "bin", e),
        ),
      ];
      const devHit = findExistingCandidate(
        collectRelativeProbeCandidates(
          context,
          folders,
          repoRoot,
          devRelCandidates,
        ),
      );
      if (!devHit) return undefined;
      return {
        path: normalizeServerPathForPlatform(devHit),
        source: "development",
      };
    };

    if (preferDevelopment) {
      const dev = resolveDevelopmentPath();
      if (dev) return dev;
    }

    const bundledRuntime = bundledRuntimeRelPaths();
    const bundledExactHit = findExistingCandidate(
      collectRelativeProbeCandidates(
        context,
        folders,
        repoRoot,
        bundledRuntime.exact,
      ),
    );
    if (bundledExactHit) {
      return {
        path: normalizeServerPathForPlatform(bundledExactHit),
        source: "bundled-exact",
      };
    }

    const bundledFallbackHit = findExistingCandidate(
      collectRelativeProbeCandidates(
        context,
        folders,
        repoRoot,
        bundledRuntime.fallback,
      ),
    );
    if (bundledFallbackHit) {
      return {
        path: normalizeServerPathForPlatform(bundledFallbackHit),
        source: "bundled-fallback",
        note: "Bundled win32-arm64 binary was not found; using bundled win32-x64 fallback.",
      };
    }

    const legacyBundledRelCandidates = exes.map((e) => path.join("server", e));
    const legacyBundledHit = findExistingCandidate(
      collectRelativeProbeCandidates(
        context,
        folders,
        repoRoot,
        legacyBundledRelCandidates,
      ),
    );
    if (legacyBundledHit) {
      return {
        path: normalizeServerPathForPlatform(legacyBundledHit),
        source: "bundled-fallback",
        note: "Using legacy bundled server path for compatibility; migrate to runtime/server/<platform-arch>/.",
      };
    }

    const dev = resolveDevelopmentPath();
    if (dev) return dev;
    return { path: undefined, source: "development" };
  };

  const auto = resolveAutoPath();
  if (cfgRaw.length === 0) {
    return auto;
  }

  const expanded = expandWorkspaceVars(
    expandHomeDir(expandEnvVars(cfgRaw)),
    folders,
  );
  const candidates: string[] = [];

  for (const item of expanded) {
    const rawItem = item.trim();
    if (!rawItem) continue;

    if (/^file:\/\//i.test(rawItem)) {
      try {
        candidates.push(
          normalizeServerPathForPlatform(vscode.Uri.parse(rawItem).fsPath),
        );
      } catch {
        // ignore malformed URI
      }
      continue;
    }

    const c = normalizeServerPathForPlatform(rawItem);
    if (!c) continue;

    if (path.isAbsolute(c)) {
      pushCandidate(candidates, c);
      continue;
    }

    if (repoRoot) {
      pushCandidate(candidates, path.join(repoRoot, c));
    }
    for (const f of folders) {
      addRelativeCandidates(candidates, f.uri.fsPath, c, 4);
    }
    addRelativeCandidates(candidates, context.extensionPath, c, 2);
    pushCandidate(candidates, context.asAbsolutePath(c));
    pushCandidate(candidates, path.resolve(c));
  }

  const ordered = uniquePaths(candidates);
  const configuredHit = ordered.find((p) => fs.existsSync(p));
  if (!configuredHit) {
    if (auto.path) {
      return {
        path: auto.path,
        source: auto.source,
        note: "Configured jovial.server.path was not found; using automatic server resolution instead.",
      };
    }
    return {
      path:
        ordered.length > 0
          ? normalizeServerPathForPlatform(ordered[0])
          : undefined,
      source: "configured",
      note: "Configured jovial.server.path was not found and no bundled/development fallback is available.",
    };
  }

  if (
    preferBundled &&
    auto.path &&
    (auto.source === "bundled-exact" || auto.source === "bundled-fallback") &&
    watchPathKey(auto.path) !== watchPathKey(configuredHit)
  ) {
    return {
      path: auto.path,
      source: auto.source,
      note: "Ignoring jovial.server.path because jovial.server.preferBundled is enabled and a bundled server is available.",
    };
  }

  return {
    path: normalizeServerPathForPlatform(configuredHit),
    source: "configured",
  };
}
