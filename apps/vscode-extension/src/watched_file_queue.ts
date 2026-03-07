import { watchPathKey } from "./workspace_paths";

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
): boolean {
  const norm = watchPathKey(fsPath, platform).replace(/\\/g, "/");
  return (
    norm.includes("/.git/") ||
    norm.includes("/_build/") ||
    norm.includes("/node_modules/") ||
    norm.includes("/.vscode/")
  );
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
};

export function queueWatchedFileChange(
  state: Map<string, PendingWatchedFileChange>,
  change: PendingWatchedFileChange,
  options: QueueOptions = {},
): void {
  const { isOpenFilePath, platform = process.platform } = options;
  const fsPath = change.fsPath;
  if (!fsPath || shouldIgnoreWatchedPath(fsPath, platform)) return;
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
