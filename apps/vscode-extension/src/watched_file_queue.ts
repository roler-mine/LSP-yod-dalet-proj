// Module overview: Coalesces file watcher events before they are sent to the language server.

import { watchPathKey } from "./workspace_paths";
import { DEFAULT_JOVIAL_SOURCE_EXTENSIONS } from "./source_extensions";
import { shouldIgnoreSourcePath } from "./ignored_paths";

export type WatchedFileChangeType = 1 | 2 | 3;
export type PendingWatchedFileChange = {
  fsPath: string;
  type: WatchedFileChangeType;
};

export const WATCH_CHANGE_CREATED: WatchedFileChangeType = 1;
export const WATCH_CHANGE_CHANGED: WatchedFileChangeType = 2;
export const WATCH_CHANGE_DELETED: WatchedFileChangeType = 3;

export function shouldIgnoreWatchedPath(
  fsPath: string,
  platform: NodeJS.Platform = process.platform,
  sourceExtensions: readonly string[] = DEFAULT_JOVIAL_SOURCE_EXTENSIONS,
): boolean {
  return shouldIgnoreSourcePath(fsPath, platform, sourceExtensions);
}

export function mergeWatchedChangeType(
  prev: WatchedFileChangeType,
  next: WatchedFileChangeType,
): WatchedFileChangeType | null {
  if (prev === WATCH_CHANGE_CREATED && next === WATCH_CHANGE_DELETED)
    return null;
  if (prev === WATCH_CHANGE_DELETED && next === WATCH_CHANGE_CREATED)
    return WATCH_CHANGE_CHANGED;
  if (prev === WATCH_CHANGE_CREATED && next === WATCH_CHANGE_CHANGED)
    return WATCH_CHANGE_CREATED;
  if (prev === WATCH_CHANGE_CHANGED && next === WATCH_CHANGE_DELETED)
    return WATCH_CHANGE_DELETED;
  return next;
}

type QueueOptions = {
  isOpenFilePath?: (fsPath: string) => boolean;
  platform?: NodeJS.Platform;
  sourceExtensions?: readonly string[];
};

export function queueWatchedFileChange(
  state: Map<string, PendingWatchedFileChange>,
  change: PendingWatchedFileChange,
  options: QueueOptions = {},
): void {
  const {
    isOpenFilePath,
    platform = process.platform,
    sourceExtensions = DEFAULT_JOVIAL_SOURCE_EXTENSIONS,
  } = options;
  const fsPath = change.fsPath;
  if (!fsPath || shouldIgnoreWatchedPath(fsPath, platform, sourceExtensions))
    return;
  if (change.type === WATCH_CHANGE_CHANGED && isOpenFilePath?.(fsPath)) return;

  const key = watchPathKey(fsPath, platform);
  const prev = state.get(key);
  if (!prev) {
    state.set(key, change);
    return;
  }

  const merged = mergeWatchedChangeType(prev.type, change.type);
  if (merged === null) state.delete(key);
  else state.set(key, { fsPath, type: merged });
}

export function takeWatchedFileBatch(
  state: Map<string, PendingWatchedFileChange>,
  maxItems: number,
): PendingWatchedFileChange[] {
  const out: PendingWatchedFileChange[] = [];
  for (const [key, value] of state) {
    out.push(value);
    state.delete(key);
    if (out.length >= maxItems) break;
  }
  return out;
}
