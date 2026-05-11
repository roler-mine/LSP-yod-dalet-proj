#!/usr/bin/env node

const cp = require("child_process");
const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..", "..");
const serverProjectDir = path.join(repoRoot, "apps", "lsp-server");
const extensionProjectDir = path.join(repoRoot, "apps", "vscode-extension");

const targetRuntimeMap = {
  "win32-x64": path.join(
    extensionProjectDir,
    "runtime",
    "server",
    "win32-x64",
    "jovial-lsp.exe",
  ),
  "win32-arm64": path.join(
    extensionProjectDir,
    "runtime",
    "server",
    "win32-arm64",
    "jovial-lsp.exe",
  ),
  "linux-x64": path.join(
    extensionProjectDir,
    "runtime",
    "server",
    "linux-x64",
    "jovial-lsp",
  ),
  "linux-arm64": path.join(
    extensionProjectDir,
    "runtime",
    "server",
    "linux-arm64",
    "jovial-lsp",
  ),
};

const expectedBinaryInfoByTarget = {
  "win32-x64": { format: "pe", machine: 0x8664, arch: "x64" },
  "win32-arm64": { format: "pe", machine: 0xaa64, arch: "arm64" },
  "linux-x64": { format: "elf", machine: 62, arch: "x64" },
  "linux-arm64": { format: "elf", machine: 183, arch: "arm64" },
};

function parseArgs(argv) {
  let target;
  let outPath;
  let allowArchMismatch = false;

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--target") {
      target = argv[i + 1];
      i += 1;
      continue;
    }
    if (arg.startsWith("--target=")) {
      target = arg.slice("--target=".length);
      continue;
    }
    if (arg === "--out") {
      outPath = argv[i + 1];
      i += 1;
      continue;
    }
    if (arg.startsWith("--out=")) {
      outPath = arg.slice("--out=".length);
      continue;
    }
    if (arg === "--allow-arch-mismatch") {
      allowArchMismatch = true;
    }
  }

  return { target, outPath, allowArchMismatch };
}

function inferHostTarget() {
  if (
    (process.platform !== "win32" && process.platform !== "linux") ||
    (process.arch !== "x64" && process.arch !== "arm64")
  ) {
    return undefined;
  }
  return `${process.platform}-${process.arch}`;
}

function resolveTarget(configuredTarget) {
  const target = configuredTarget ?? inferHostTarget();
  if (!target) {
    process.stderr.write(
      "[build-server] target is required on unsupported hosts. " +
        `Use one of: ${Object.keys(targetRuntimeMap).join(", ")}.\n`,
    );
    process.exit(1);
  }
  if (!Object.prototype.hasOwnProperty.call(targetRuntimeMap, target)) {
    process.stderr.write(
      `[build-server] unsupported target '${target}'. Supported targets: ${Object.keys(
        targetRuntimeMap,
      ).join(", ")}.\n`,
    );
    process.exit(1);
  }
  return target;
}

function splitTarget(target) {
  const [platform, arch] = target.split("-");
  return { platform, arch };
}

function run(command, args, cwd, options = {}) {
  process.stdout.write(`[build-server] ${command} ${args.join(" ")}\n`);
  const res = cp.spawnSync(command, args, {
    cwd,
    stdio: "inherit",
    env: {
      ...process.env,
      ...(options.env ?? {}),
    },
  });
  if (res.error) {
    return { ok: false, error: res.error, status: res.status ?? 1 };
  }
  return { ok: res.status === 0, status: res.status ?? 1 };
}

function shellQuotePosix(value) {
  return `'${String(value).replace(/'/g, `'\"'\"'`)}'`;
}

function windowsPathToWslPath(windowsPath) {
  const resolved = path.resolve(windowsPath).replace(/\\/g, "/");
  const match = /^([A-Za-z]):\/(.*)$/.exec(resolved);
  if (!match) {
    throw new Error(`failed to translate Windows path to WSL path: ${windowsPath}`);
  }
  return `/mnt/${match[1].toLowerCase()}/${match[2]}`;
}

function duneBuildDirForTarget(target) {
  return `_build-${target}`;
}

function buildServerNative(target) {
  return run(
    "opam",
    ["exec", "--", "dune", "build", "@install"],
    serverProjectDir,
    {
      env: {
        DUNE_BUILD_DIR: duneBuildDirForTarget(target),
      },
    },
  );
}

function buildServerViaWsl(target) {
  let wslProjectDir;
  try {
    wslProjectDir = windowsPathToWslPath(serverProjectDir);
  } catch (err) {
    process.stderr.write(`[build-server] ${String(err)}\n`);
    process.exit(1);
  }

  const buildDir = duneBuildDirForTarget(target);
  const buildCommand = `cd ${shellQuotePosix(
    wslProjectDir,
  )} && DUNE_BUILD_DIR=${shellQuotePosix(buildDir)} opam exec -- dune build @install`;
  return run("wsl.exe", ["bash", "-lc", buildCommand], repoRoot);
}

function buildServer(target) {
  const { platform } = splitTarget(target);
  const canBuildNatively = platform === process.platform;
  const canBuildViaWsl = process.platform === "win32" && platform === "linux";

  const buildResult = canBuildNatively
    ? buildServerNative(target)
    : canBuildViaWsl
      ? buildServerViaWsl(target)
      : null;

  if (!buildResult) {
    process.stderr.write(
      `[build-server] cannot build target ${target} from ${process.platform}-${process.arch}. ` +
        "Use a matching host/runner or build Linux targets from Windows via WSL.\n",
    );
    process.exit(1);
  }

  if (buildResult.ok) return;

  if (buildResult.error && buildResult.error.code === "ENOENT") {
    if (canBuildViaWsl) {
      process.stderr.write(
        "[build-server] WSL was not found. Install WSL with an OCaml toolchain and retry.\n",
      );
    } else {
      process.stderr.write("[build-server] opam was not found. Install opam and retry.\n");
    }
  } else {
    process.stderr.write(
      `[build-server] server build failed for ${target}. ` +
        (canBuildViaWsl
          ? "Verify WSL has opam and dune installed."
          : "Verify the local opam switch can build the server.") +
        "\n",
    );
  }

  process.exit(buildResult.status || 1);
}

function binaryCandidatesForTarget(target) {
  const { platform } = splitTarget(target);
  const buildDir = duneBuildDirForTarget(target);
  const names =
    platform === "win32"
      ? ["jovial-lsp.exe", "Main.exe", "jovial-lsp", "Main"]
      : ["jovial-lsp", "Main", "jovial-lsp.exe", "Main.exe"];

  return names.flatMap((name) => [
    path.join(serverProjectDir, buildDir, "install", "default", "bin", name),
    path.join(serverProjectDir, buildDir, "default", "bin", name),
  ]);
}

function findBuiltBinary(target) {
  const sourceCandidates = binaryCandidatesForTarget(target);
  for (const candidate of sourceCandidates) {
    if (fs.existsSync(candidate)) return candidate;
  }
  process.stderr.write(
    `[build-server] build completed but no server binary was found in:\n- ${sourceCandidates.join(
      "\n- ",
    )}\n`,
  );
  process.exit(1);
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

function readElfMachine(filePath) {
  const fd = fs.openSync(filePath, "r");
  try {
    const header = Buffer.allocUnsafe(20);
    fs.readSync(fd, header, 0, header.length, 0);
    const isBigEndian = header[5] === 2;
    return isBigEndian ? header.readUInt16BE(18) : header.readUInt16LE(18);
  } finally {
    fs.closeSync(fd);
  }
}

function readBinaryInfo(filePath) {
  const fd = fs.openSync(filePath, "r");
  try {
    const header = Buffer.allocUnsafe(4);
    fs.readSync(fd, header, 0, header.length, 0);
    if (header[0] === 0x4d && header[1] === 0x5a) {
      return { format: "pe", machine: readPeMachine(filePath) };
    }
    if (
      header[0] === 0x7f &&
      header[1] === 0x45 &&
      header[2] === 0x4c &&
      header[3] === 0x46
    ) {
      return { format: "elf", machine: readElfMachine(filePath) };
    }
    return { format: "unknown", machine: undefined };
  } finally {
    fs.closeSync(fd);
  }
}

function machineName(machine) {
  if (machine === 0x8664 || machine === 62) return "x64";
  if (machine === 0xaa64 || machine === 183) return "arm64";
  return `0x${machine.toString(16)}`;
}

function validateBinaryArchitecture(sourcePath, target, allowArchMismatch) {
  const expected = expectedBinaryInfoByTarget[target];
  if (!expected) return;

  const actual = readBinaryInfo(sourcePath);
  if (actual.format === expected.format && actual.machine === expected.machine) {
    return;
  }

  const mismatchMessage =
    `[build-server] built binary mismatch for ${target}: expected ${expected.format.toUpperCase()} ${expected.arch}, ` +
    `found ${actual.format.toUpperCase()} ${machineName(actual.machine ?? 0)} at ${sourcePath}\n` +
    "[build-server] build this target on a matching host/runner or with a proper cross toolchain.\n";

  if (!allowArchMismatch) {
    process.stderr.write(mismatchMessage);
    process.stderr.write(
      "[build-server] pass --allow-arch-mismatch only if you intentionally want a non-matching binary.\n",
    );
    process.exit(1);
  }

  process.stderr.write(
    mismatchMessage +
      "[build-server] warning: continuing because --allow-arch-mismatch was set.\n",
  );
}

function applyBundledBinaryMode(destinationPath, target) {
  const { platform } = splitTarget(target);
  if (platform !== "linux") return;
  try {
    fs.chmodSync(destinationPath, 0o755);
  } catch (err) {
    process.stderr.write(
      `[build-server] warning: failed to set execute permissions on ${destinationPath}: ${String(
        err,
      )}\n`,
    );
  }
}

function bundleBinary(sourcePath, destinationPath, target) {
  fs.mkdirSync(path.dirname(destinationPath), { recursive: true });
  try {
    fs.copyFileSync(sourcePath, destinationPath);
    applyBundledBinaryMode(destinationPath, target);
    process.stdout.write(`[build-server] bundled server: ${destinationPath}\n`);
    return;
  } catch (err) {
    const code = err && typeof err === "object" ? err.code : undefined;
    if (code !== "EPERM" && code !== "EBUSY") {
      throw err;
    }

    const pendingPath = `${destinationPath}.pending-${Date.now()}`;
    fs.copyFileSync(sourcePath, pendingPath);
    applyBundledBinaryMode(pendingPath, target);
    process.stderr.write(
      `[build-server] destination is locked; wrote updated binary to ${pendingPath}\n` +
        `[build-server] stop running extension/server processes and replace ${destinationPath}\n`,
    );
  }
}

const args = parseArgs(process.argv.slice(2));
const target = resolveTarget(args.target);
const destinationPath = args.outPath
  ? path.resolve(process.cwd(), args.outPath)
  : targetRuntimeMap[target];

buildServer(target);
const builtBinary = findBuiltBinary(target);
validateBinaryArchitecture(builtBinary, target, args.allowArchMismatch);
bundleBinary(builtBinary, destinationPath, target);
