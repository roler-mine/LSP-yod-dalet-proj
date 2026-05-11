#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..", "..");
const extensionRoot = path.join(repoRoot, "apps", "vscode-extension");

const targetRuntimeMap = {
  "win32-x64": path.join(
    extensionRoot,
    "runtime",
    "server",
    "win32-x64",
    "jovial-lsp.exe",
  ),
  "win32-arm64": path.join(
    extensionRoot,
    "runtime",
    "server",
    "win32-arm64",
    "jovial-lsp.exe",
  ),
  "linux-x64": path.join(
    extensionRoot,
    "runtime",
    "server",
    "linux-x64",
    "jovial-lsp",
  ),
  "linux-arm64": path.join(
    extensionRoot,
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

function inferHostTarget() {
  if (
    (process.platform !== "win32" && process.platform !== "linux") ||
    (process.arch !== "x64" && process.arch !== "arm64")
  ) {
    return undefined;
  }
  return `${process.platform}-${process.arch}`;
}

function parseTargets(argv) {
  let rawTargets;
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--targets") {
      rawTargets = (argv[i + 1] ?? "")
        .split(",")
        .map((item) => item.trim())
        .filter(Boolean);
      break;
    }
    if (arg.startsWith("--targets=")) {
      rawTargets = arg
        .slice("--targets=".length)
        .split(",")
        .map((item) => item.trim())
        .filter(Boolean);
      break;
    }
  }

  if (!rawTargets || rawTargets.length === 0) {
    rawTargets = Object.keys(targetRuntimeMap);
  }

  const resolvedTargets = [];
  for (const target of rawTargets) {
    if (target !== "host") {
      resolvedTargets.push(target);
      continue;
    }

    const hostTarget = inferHostTarget();
    if (!hostTarget) {
      process.stderr.write(
        "[verify-runtime] --targets host is supported only on win32/linux x64/arm64 hosts.\n",
      );
      process.exit(1);
    }
    resolvedTargets.push(hostTarget);
  }

  return Array.from(new Set(resolvedTargets));
}

function isArchCheckDisabled(argv) {
  return argv.includes("--skip-arch-check");
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
  if (typeof machine !== "number") return "unknown";
  return `0x${machine.toString(16)}`;
}

const targets = parseTargets(process.argv.slice(2));
const skipArchCheck = isArchCheckDisabled(process.argv.slice(2));
const missing = [];
const archMismatches = [];

for (const target of targets) {
  const expectedPath = targetRuntimeMap[target];
  if (!expectedPath) {
    process.stderr.write(`[verify-runtime] unknown target '${target}'.\n`);
    process.exit(1);
  }
  if (!fs.existsSync(expectedPath)) {
    missing.push(expectedPath);
    continue;
  }

  if (skipArchCheck) continue;

  const expectedInfo = expectedBinaryInfoByTarget[target];
  const actualInfo = readBinaryInfo(expectedPath);
  if (
    actualInfo.format !== expectedInfo.format ||
    actualInfo.machine !== expectedInfo.machine
  ) {
    archMismatches.push({
      target,
      path: expectedPath,
      expectedInfo,
      actualInfo,
    });
  }
}

if (missing.length > 0) {
  process.stderr.write("[verify-runtime] missing runtime binaries:\n");
  for (const filePath of missing) {
    process.stderr.write(`- ${filePath}\n`);
  }
  process.exit(1);
}

if (archMismatches.length > 0) {
  process.stderr.write("[verify-runtime] architecture mismatch:\n");
  for (const mismatch of archMismatches) {
    process.stderr.write(
      `- ${mismatch.target}: expected ${mismatch.expectedInfo.format.toUpperCase()} ${mismatch.expectedInfo.arch}, ` +
        `found ${mismatch.actualInfo.format.toUpperCase()} ${machineName(
          mismatch.actualInfo.machine,
        )} at ${mismatch.path}\n`,
    );
  }
  process.stderr.write("[verify-runtime] build each target on a matching host/runner.\n");
  process.exit(1);
}

process.stdout.write(
  `[verify-runtime] runtime binaries present for targets: ${targets.join(", ")}\n`,
);
