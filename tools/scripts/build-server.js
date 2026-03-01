#!/usr/bin/env node

const cp = require("child_process");
const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..", "..");
const serverProjectDir = path.join(repoRoot, "apps", "lsp-server");
const extensionProjectDir = path.join(repoRoot, "apps", "vscode-extension");

const targetRuntimeMap = {
  "win32-x64": path.join(extensionProjectDir, "runtime", "server", "win32-x64", "jovial-lsp.exe"),
  "win32-arm64": path.join(extensionProjectDir, "runtime", "server", "win32-arm64", "jovial-lsp.exe"),
};

const expectedPeMachineByTarget = {
  "win32-x64": 0x8664,
  "win32-arm64": 0xAA64,
};

function parseArgs(argv) {
  let target;
  let outPath;
  let allowArchMismatch = false;

  for (let i = 0; i < argv.length; i += 1) {
    const a = argv[i];
    if (a === "--target") {
      target = argv[i + 1];
      i += 1;
      continue;
    }
    if (a.startsWith("--target=")) {
      target = a.slice("--target=".length);
      continue;
    }
    if (a === "--out") {
      outPath = argv[i + 1];
      i += 1;
      continue;
    }
    if (a.startsWith("--out=")) {
      outPath = a.slice("--out=".length);
      continue;
    }
    if (a === "--allow-arch-mismatch") {
      allowArchMismatch = true;
      continue;
    }
  }

  return { target, outPath, allowArchMismatch };
}

function inferHostTarget() {
  if (process.platform !== "win32") return undefined;
  if (process.arch !== "x64" && process.arch !== "arm64") return undefined;
  return `win32-${process.arch}`;
}

function run(command, args, cwd) {
  process.stdout.write(`[build-server] ${command} ${args.join(" ")}\n`);
  const res = cp.spawnSync(command, args, { cwd, stdio: "inherit" });
  if (res.error) {
    return { ok: false, error: res.error };
  }
  return { ok: res.status === 0, status: res.status ?? 1 };
}

function buildServer() {
  const opamBuild = run("opam", ["exec", "--", "dune", "build", "@install"], serverProjectDir);
  if (!opamBuild.ok) {
    if (opamBuild.error && opamBuild.error.code === "ENOENT") {
      process.stderr.write("[build-server] opam was not found. Install opam and retry.\n");
    } else {
      process.stderr.write("[build-server] opam exec -- dune build @install failed.\n");
    }
    process.exit(opamBuild.status || 1);
  }
}

function findBuiltBinary() {
  const sourceCandidates = [
    path.join(serverProjectDir, "_build", "install", "default", "bin", "jovial-lsp.exe"),
    path.join(serverProjectDir, "_build", "install", "default", "bin", "jovial-lsp"),
    path.join(serverProjectDir, "_build", "default", "bin", "Main.exe"),
    path.join(serverProjectDir, "_build", "default", "bin", "Main"),
  ];
  for (const candidate of sourceCandidates) {
    if (fs.existsSync(candidate)) return candidate;
  }
  process.stderr.write(
    `[build-server] build completed but no server binary was found in:\n- ${sourceCandidates.join("\n- ")}\n`
  );
  process.exit(1);
}

function resolveTarget(configuredTarget) {
  const target = configuredTarget ?? inferHostTarget();
  if (!target) {
    process.stderr.write(
      "[build-server] target is required on non-Windows hosts. Use --target win32-x64 or --target win32-arm64.\n"
    );
    process.exit(1);
  }
  if (!Object.prototype.hasOwnProperty.call(targetRuntimeMap, target)) {
    process.stderr.write(
      `[build-server] unsupported target '${target}'. Supported targets: ${Object.keys(targetRuntimeMap).join(", ")}.\n`
    );
    process.exit(1);
  }
  return target;
}

function readPeMachine(filePath) {
  const fd = fs.openSync(filePath, "r");
  try {
    const mz = Buffer.allocUnsafe(64);
    fs.readSync(fd, mz, 0, mz.length, 0);
    const peOffset = mz.readUInt32LE(0x3c);
    const peHeader = Buffer.allocUnsafe(6);
    fs.readSync(fd, peHeader, 0, peHeader.length, peOffset);
    return peHeader.readUInt16LE(4);
  } finally {
    fs.closeSync(fd);
  }
}

function machineName(machine) {
  if (machine === 0x8664) return "x64";
  if (machine === 0xAA64) return "arm64";
  return `0x${machine.toString(16)}`;
}

function validateBinaryArchitecture(sourcePath, target, allowArchMismatch) {
  if (!sourcePath.toLowerCase().endsWith(".exe")) return;
  const expectedMachine = expectedPeMachineByTarget[target];
  if (!expectedMachine) return;
  const actualMachine = readPeMachine(sourcePath);
  if (actualMachine === expectedMachine) return;

  const msg =
    `[build-server] built binary architecture mismatch for ${target}: ` +
    `expected ${machineName(expectedMachine)}, found ${machineName(actualMachine)} at ${sourcePath}\n` +
    "[build-server] build this target on a matching host/runner or with a proper cross toolchain/switch.\n";

  if (!allowArchMismatch) {
    process.stderr.write(msg);
    process.stderr.write(
      "[build-server] pass --allow-arch-mismatch only if you intentionally want a non-matching binary.\n"
    );
    process.exit(1);
  }

  process.stderr.write(
    msg +
    "[build-server] warning: continuing because --allow-arch-mismatch was set.\n"
  );
}

function bundleBinary(sourcePath, destinationPath) {
  fs.mkdirSync(path.dirname(destinationPath), { recursive: true });
  try {
    fs.copyFileSync(sourcePath, destinationPath);
    process.stdout.write(`[build-server] bundled server: ${destinationPath}\n`);
    return;
  } catch (err) {
    const code = err && typeof err === "object" ? err.code : undefined;
    if (code !== "EPERM" && code !== "EBUSY") {
      throw err;
    }

    const pendingPath = `${destinationPath}.pending-${Date.now()}`;
    fs.copyFileSync(sourcePath, pendingPath);
    process.stderr.write(
      `[build-server] destination is locked; wrote updated binary to ${pendingPath}\n` +
      `[build-server] stop running extension/server processes and replace ${destinationPath}\n`
    );
  }
}

const args = parseArgs(process.argv.slice(2));
const target = resolveTarget(args.target);
const destinationPath = args.outPath
  ? path.resolve(process.cwd(), args.outPath)
  : targetRuntimeMap[target];

buildServer();
const builtBinary = findBuiltBinary();
validateBinaryArchitecture(builtBinary, target, args.allowArchMismatch);
bundleBinary(builtBinary, destinationPath);
