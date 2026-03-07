#!/usr/bin/env node

import fs from "fs";
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

const workspaceRoot = path.join(repoRoot, "apps", "lsp-server");
const allowedExtensions = new Set([".ml", ".mli"]);
const ignoredDirs = new Set(["_build", "node_modules", ".git"]);

function collectOcamlFiles(dirPath, out) {
  for (const entry of fs.readdirSync(dirPath, { withFileTypes: true })) {
    if (entry.isDirectory()) {
      if (!ignoredDirs.has(entry.name)) {
        collectOcamlFiles(path.join(dirPath, entry.name), out);
      }
      continue;
    }
    if (!entry.isFile()) {
      continue;
    }
    if (!allowedExtensions.has(path.extname(entry.name))) {
      continue;
    }
    const fullPath = path.join(dirPath, entry.name);
    out.push(path.relative(repoRoot, fullPath).replace(/\\/g, "/"));
  }
}

const files = [];
collectOcamlFiles(workspaceRoot, files);
files.sort();

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
