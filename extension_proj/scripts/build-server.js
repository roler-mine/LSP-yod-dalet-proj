#!/usr/bin/env node

const cp = require("child_process");
const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..", "..");
const serverProjectDir = path.join(repoRoot, "server_proj");
const extensionServerDir = path.join(repoRoot, "extension_proj", "server");
const exeSuffix = process.platform === "win32" ? ".exe" : "";

const sourceCandidates = [
  path.join(serverProjectDir, "_build", "install", "default", "bin", `jovial-lsp${exeSuffix}`),
  path.join(serverProjectDir, "_build", "default", "bin", `Main${exeSuffix}`),
];

function run(command, args, cwd) {
  process.stdout.write(`[build-server] ${command} ${args.join(" ")}\n`);
  const res = cp.spawnSync(command, args, { cwd, stdio: "inherit" });
  if (res.error) {
    return { ok: false, error: res.error };
  }
  return { ok: res.status === 0, status: res.status };
}

function buildServer() {
  const opamBuild = run("opam", ["exec", "--", "dune", "build", "@install"], serverProjectDir);
  if (opamBuild.ok) return;
  if (opamBuild.error && opamBuild.error.code === "ENOENT") {
    process.stdout.write("[build-server] opam was not found; trying dune directly.\n");
  } else {
    process.stdout.write("[build-server] opam build failed; trying dune directly.\n");
  }

  const duneBuild = run("dune", ["build", "@install"], serverProjectDir);
  if (!duneBuild.ok) {
    process.stderr.write("[build-server] failed to build the OCaml server with opam and dune.\n");
    process.exit(1);
  }
}

function findBuiltBinary() {
  for (const candidate of sourceCandidates) {
    if (fs.existsSync(candidate)) return candidate;
  }
  process.stderr.write(
    `[build-server] build completed but no server binary was found in:\n- ${sourceCandidates.join("\n- ")}\n`
  );
  process.exit(1);
}

function isBundledServerFile(name) {
  if (!name.startsWith("jovial-lsp")) return false;
  if (exeSuffix === ".exe") return name.endsWith(".exe");
  return !name.endsWith(".exe");
}

function cleanupOldBundles(keepNames) {
  let entries = [];
  try {
    entries = fs.readdirSync(extensionServerDir);
  } catch {
    return;
  }
  for (const name of entries) {
    if (!isBundledServerFile(name)) continue;
    if (keepNames.has(name)) continue;
    try {
      fs.unlinkSync(path.join(extensionServerDir, name));
    } catch {
      // Ignore locked files from running extension hosts.
    }
  }
}

function bundleBinary(sourcePath) {
  fs.mkdirSync(extensionServerDir, { recursive: true });
  const versionedName = `jovial-lsp-bundled-${Date.now()}${exeSuffix}`;
  const versionedPath = path.join(extensionServerDir, versionedName);
  fs.copyFileSync(sourcePath, versionedPath);
  if (process.platform !== "win32") {
    fs.chmodSync(versionedPath, 0o755);
  }
  const keepNames = new Set([versionedName]);
  cleanupOldBundles(keepNames);

  process.stdout.write(`[build-server] bundled server: ${versionedPath}\n`);
}

buildServer();
bundleBinary(findBuiltBinary());
