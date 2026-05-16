// Module overview: Tests for the workspace paths.test extension module.

import assert from "node:assert/strict";

import {
  dirnamePath,
  lowestCommonAncestorPath,
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
  assert.equal(pathWithinRoot("c:\\repo\\file.j73", "c:\\"), true);

  const root = pickBestWorkspaceRoot("/repo/app/nested/file.j73", [
    "/repo",
    "/repo/app",
    "/other",
  ]);
  assert.equal(root, "/repo/app");

  assert.equal(
    lowestCommonAncestorPath(
      [
        "/ws/system/source/A.j73",
        "/ws/system/source/B.j73",
        "/ws/system/source/sub/C.j73",
      ].map((p) => dirnamePath(p, "linux")),
      "linux",
    ),
    "/ws/system/source",
  );
  assert.equal(
    lowestCommonAncestorPath(
      ["/ws/system/source/A.j73", "/ws/system/source/sub/deep/B.j73"].map((p) =>
        dirnamePath(p, "linux"),
      ),
      "linux",
    ),
    "/ws/system/source",
  );
  assert.equal(
    lowestCommonAncestorPath(
      [dirnamePath("/ws/system/source/A.j73", "linux")],
      "linux",
    ),
    "/ws/system/source",
  );
  assert.equal(
    lowestCommonAncestorPath(
      ["/ws/systemA/source/A.j73", "/ws/systemB/source/B.j73"].map((p) =>
        dirnamePath(p, "linux"),
      ),
      "linux",
    ),
    "/ws",
  );
  assert.equal(
    lowestCommonAncestorPath(
      [
        "C:\\work\\system\\source\\A.j73",
        "C:\\work\\system\\source\\sub\\B.j73",
      ].map((p) => dirnamePath(p, "win32")),
      "win32",
    ),
    "C:\\work\\system\\source",
  );
  assert.equal(
    lowestCommonAncestorPath(
      ["/ws/foo/A.j73", "/ws/foobar/B.j73"].map((p) => dirnamePath(p, "linux")),
      "linux",
    ),
    "/ws",
  );
}
