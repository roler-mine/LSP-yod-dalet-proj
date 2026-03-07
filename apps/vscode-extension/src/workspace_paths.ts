import * as path from "path";

export function watchPathKey(
  fsPath: string,
  platform: NodeJS.Platform = process.platform,
): string {
  const pathLib = platform === "win32" ? path.win32 : path.posix;
  const norm = pathLib.normalize(fsPath);
  return platform === "win32" ? norm.toLowerCase() : norm;
}

export function pathWithinRoot(pathKey: string, rootKey: string): boolean {
  if (pathKey === rootKey) return true;
  if (!pathKey.startsWith(rootKey)) return false;
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
