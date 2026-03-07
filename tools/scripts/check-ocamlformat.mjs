#!/usr/bin/env node

import { spawnSync } from "child_process";
import path from "path";
import { fileURLToPath } from "url";

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(scriptDir, "..", "..");

function run(command, args) {
  const result = spawnSync(command, args, {
    cwd: repoRoot,
    stdio: "inherit",
  });
  if (result.error) {
    throw result.error;
  }
  if ((result.status ?? 0) !== 0) {
    process.exit(result.status ?? 1);
  }
}

const rgResult = spawnSync(
  "rg",
  [
    "--files",
    "apps/lsp-server",
    "-g",
    "*.ml",
    "-g",
    "*.mli",
    "-g",
    "*.mll",
    "-g",
    "*.mly",
  ],
  {
    cwd: repoRoot,
    encoding: "utf8",
  }
);

if (rgResult.error) {
  throw rgResult.error;
}
if ((rgResult.status ?? 0) !== 0) {
  process.exit(rgResult.status ?? 1);
}

const files = rgResult.stdout
  .split(/\r?\n/)
  .map((line) => line.trim())
  .filter((line) => line.length > 0);

if (files.length === 0) {
  process.exit(0);
}

run("opam", [
  "exec",
  "--",
  "ocamlformat",
  "--enable-outside-detected-project",
  "--check",
  ...files,
]);
