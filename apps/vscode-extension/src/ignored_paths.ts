// Module overview: Centralizes workspace path ignore rules used by discovery, watching, and indexing.

import {
  DEFAULT_JOVIAL_SOURCE_EXTENSIONS,
  hasJovialSourceExtension,
} from "./source_extensions";
import { watchPathKey } from "./workspace_paths";

export const IGNORED_DIR_SEGMENTS = [
  ".git",
  "build",
  "dist",
  "out",
  "node_modules",
  ".vscode-test",
  ".vscode",
  ".jovial-lsp",
] as const;

export function sourceDiscoveryExcludeGlob(): string {
  return "{**/.git/**,**/_build*/**,**/build/**,**/dist/**,**/out/**,**/node_modules/**,**/.vscode-test/**,**/.vscode/**,**/.jovial-lsp/**}";
}

export function shouldIgnoreSourcePath(
  fsPath: string,
  platform: NodeJS.Platform = process.platform,
  sourceExtensions: readonly string[] = DEFAULT_JOVIAL_SOURCE_EXTENSIONS,
): boolean {
  const norm = watchPathKey(fsPath, platform).replace(/\\/g, "/");
  if (!hasJovialSourceExtension(norm, sourceExtensions)) return true;
  const segments = norm.split("/").filter((segment) => segment.length > 0);
  return segments.some(
    (segment) =>
      IGNORED_DIR_SEGMENTS.includes(
        segment as (typeof IGNORED_DIR_SEGMENTS)[number],
      ) || segment.startsWith("_build"),
  );
}
