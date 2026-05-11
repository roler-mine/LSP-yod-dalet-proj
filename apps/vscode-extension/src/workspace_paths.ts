import * as path from "path";

function pathLibForPlatform(platform: NodeJS.Platform): typeof path.win32 {
  return platform === "win32" ? path.win32 : path.posix;
}

export function watchPathKey(
  fsPath: string,
  platform: NodeJS.Platform = process.platform,
): string {
  const pathLib = pathLibForPlatform(platform);
  const norm = pathLib.normalize(fsPath);
  return platform === "win32" ? norm.toLowerCase() : norm;
}

export function pathWithinRoot(pathKey: string, rootKey: string): boolean {
  if (pathKey === rootKey) return true;
  if (!pathKey.startsWith(rootKey)) return false;
  if (rootKey.endsWith("/") || rootKey.endsWith("\\")) return true;
  const next = pathKey.charAt(rootKey.length);
  return next === "/" || next === "\\";
}

export function pickBestWorkspaceRoot(
  targetPath: string,
  rootPaths: readonly string[],
  platform: NodeJS.Platform = process.platform,
): string | undefined {
  const pathKey = watchPathKey(targetPath, platform);
  let best: string | undefined;
  let bestLen = -1;
  for (const rootPath of rootPaths) {
    const rootKey = watchPathKey(rootPath, platform);
    if (!pathWithinRoot(pathKey, rootKey)) continue;
    if (rootKey.length > bestLen) {
      best = rootPath;
      bestLen = rootKey.length;
    }
  }
  return best;
}

type PathSegments = {
  root: string;
  rootKey: string;
  segments: string[];
  segmentKeys: string[];
};

function splitPathSegments(
  fsPath: string,
  platform: NodeJS.Platform,
): PathSegments {
  const pathLib = pathLibForPlatform(platform);
  const normalized = pathLib.normalize(fsPath);
  const parsed = pathLib.parse(normalized);
  const root = parsed.root;
  const rest = normalized.slice(root.length);
  const segments = rest.split(/[\\/]+/).filter((part) => part.length > 0);
  return {
    root,
    rootKey: platform === "win32" ? root.toLowerCase() : root,
    segments,
    segmentKeys:
      platform === "win32"
        ? segments.map((segment) => segment.toLowerCase())
        : segments,
  };
}

export function dirnamePath(
  fsPath: string,
  platform: NodeJS.Platform = process.platform,
): string {
  return pathLibForPlatform(platform).dirname(fsPath);
}

export function lowestCommonAncestorPath(
  fsPaths: readonly string[],
  platform: NodeJS.Platform = process.platform,
): string {
  if (fsPaths.length === 0) {
    throw new Error("Cannot compute LCA of empty path list");
  }

  const first = splitPathSegments(fsPaths[0], platform);
  let commonLen = first.segments.length;

  for (let i = 1; i < fsPaths.length; i += 1) {
    const current = splitPathSegments(fsPaths[i], platform);
    if (current.rootKey !== first.rootKey) {
      return first.root || "";
    }

    commonLen = Math.min(commonLen, current.segments.length);
    let j = 0;
    while (j < commonLen && first.segmentKeys[j] === current.segmentKeys[j]) {
      j += 1;
    }
    commonLen = j;
  }

  if (commonLen <= 0) return first.root || ".";

  const sep = platform === "win32" ? "\\" : "/";
  const suffix = first.segments.slice(0, commonLen).join(sep);
  if (!first.root) return suffix || ".";
  if (first.root.endsWith("/") || first.root.endsWith("\\")) {
    return `${first.root}${suffix}`;
  }
  return `${first.root}${sep}${suffix}`;
}
