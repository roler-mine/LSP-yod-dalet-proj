import assert from "node:assert/strict";

import {
  WATCH_CHANGE_CHANGED,
  WATCH_CHANGE_CREATED,
  WATCH_CHANGE_DELETED,
  mergeWatchedChangeType,
  queueWatchedFileChange,
  shouldIgnoreWatchedPath,
  takeWatchedFileBatch,
} from "../src/watched_file_queue";

export function run(): void {
  assert.equal(
    mergeWatchedChangeType(WATCH_CHANGE_CREATED, WATCH_CHANGE_DELETED),
    null,
  );
  assert.equal(
    mergeWatchedChangeType(WATCH_CHANGE_DELETED, WATCH_CHANGE_CREATED),
    WATCH_CHANGE_CHANGED,
  );
  assert.equal(
    mergeWatchedChangeType(WATCH_CHANGE_CREATED, WATCH_CHANGE_CHANGED),
    WATCH_CHANGE_CREATED,
  );

  const pending = new Map();

  queueWatchedFileChange(
    pending,
    { fsPath: "C:\\repo\\file.j73", type: WATCH_CHANGE_CHANGED },
    {
      platform: "win32",
      isOpenFilePath: (fsPath) => fsPath.endsWith("file.j73"),
    },
  );
  assert.equal(pending.size, 0);

  queueWatchedFileChange(
    pending,
    { fsPath: "C:\\repo\\file.j73", type: WATCH_CHANGE_CREATED },
    { platform: "win32" },
  );
  queueWatchedFileChange(
    pending,
    { fsPath: "C:\\repo\\file.j73", type: WATCH_CHANGE_CHANGED },
    { platform: "win32" },
  );
  assert.equal(pending.size, 1);
  assert.equal([...pending.values()][0].type, WATCH_CHANGE_CREATED);

  const batchState = new Map([
    ["a", { fsPath: "/repo/a.j73", type: WATCH_CHANGE_CREATED }],
    ["b", { fsPath: "/repo/b.j73", type: WATCH_CHANGE_DELETED }],
    ["c", { fsPath: "/repo/c.j73", type: WATCH_CHANGE_CHANGED }],
  ]);

  const batch = takeWatchedFileBatch(batchState, 2);
  assert.equal(batch.length, 2);
  assert.equal(batchState.size, 1);

  assert.equal(
    shouldIgnoreWatchedPath("/repo/node_modules/pkg/file.j73"),
    true,
  );
  assert.equal(shouldIgnoreWatchedPath("/repo/build/file.j73"), true);
  assert.equal(shouldIgnoreWatchedPath("/repo/dist/file.j73"), true);
  assert.equal(shouldIgnoreWatchedPath("/repo/out/file.j73"), true);
  assert.equal(shouldIgnoreWatchedPath("/repo/.vscode-test/file.j73"), true);
  assert.equal(shouldIgnoreWatchedPath("/repo/src/file.j73"), false);
  assert.equal(shouldIgnoreWatchedPath("/repo/src/file.j"), true);
  assert.equal(
    shouldIgnoreWatchedPath("/repo/src/file.j", "linux", [
      ".jov",
      ".j73",
      ".jvl",
      ".j",
    ]),
    false,
  );
}
