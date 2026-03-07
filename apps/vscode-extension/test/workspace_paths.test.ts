import assert from "node:assert/strict";

import {
  pathWithinRoot,
  pickBestWorkspaceRoot,
  watchPathKey,
} from "../src/workspace_paths";

export function run(): void {
  assert.equal(
    watchPathKey("C:\\Repo\\File.j73", "win32"),
    "c:\\repo\\file.j73",
  );
  assert.equal(watchPathKey("/Repo/File.j73", "linux"), "/Repo/File.j73");

  assert.equal(pathWithinRoot("/repo/src/file.j73", "/repo/src"), true);
  assert.equal(pathWithinRoot("/repo/src-other/file.j73", "/repo/src"), false);

  const root = pickBestWorkspaceRoot("/repo/app/nested/file.j73", [
    "/repo",
    "/repo/app",
    "/other",
  ]);
  assert.equal(root, "/repo/app");
}
