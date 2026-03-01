#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..", "..");
const extensionRoot = path.join(repoRoot, "apps", "vscode-extension");

const targetRuntimeMap = {
  "win32-x64": path.join(extensionRoot, "runtime", "server", "win32-x64", "jovial-lsp.exe"),
  "win32-arm64": path.join(extensionRoot, "runtime", "server", "win32-arm64", "jovial-lsp.exe"),
};

const expectedPeMachineByTarget = {
  "win32-x64": 0x8664,
  "win32-arm64": 0xAA64,
};

function parseTargets(argv) {
  let rawTargets;
  for (let i = 0; i < argv.length; i += 1) {
    const a = argv[i];
    if (a === "--targets") {
      rawTargets = (argv[i + 1] ?? "").split(",").map((x) => x.trim()).filter(Boolean);
      break;
    }
    if (a.startsWith("--targets=")) {
      rawTargets = a.slice("--targets=".length).split(",").map((x) => x.trim()).filter(Boolean);
      break;
    }
  }
  if (!rawTargets || rawTargets.length === 0) {
    rawTargets = Object.keys(targetRuntimeMap);
  }

  const out = [];
  for (const t of rawTargets) {
    if (t === "host") {
      if (process.platform !== "win32" || (process.arch !== "x64" && process.arch !== "arm64")) {
        process.stderr.write("[verify-runtime] --targets host is supported only on win32-x64/win32-arm64 hosts.\n");
        process.exit(1);
      }
      out.push(`win32-${process.arch}`);
    } else {
      out.push(t);
    }
  }

  return Array.from(new Set(out));
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

function machineName(machine) {
  if (machine === 0x8664) return "x64";
  if (machine === 0xAA64) return "arm64";
  return `0x${machine.toString(16)}`;
}

const targets = parseTargets(process.argv.slice(2));
const skipArchCheck = isArchCheckDisabled(process.argv.slice(2));
const missing = [];
const archMismatches = [];

for (const target of targets) {
  const expected = targetRuntimeMap[target];
  if (!expected) {
    process.stderr.write(`[verify-runtime] unknown target '${target}'.\n`);
    process.exit(1);
  }
  if (!fs.existsSync(expected)) {
    missing.push(expected);
    continue;
  }

  if (!skipArchCheck && expected.toLowerCase().endsWith(".exe")) {
    const expectedMachine = expectedPeMachineByTarget[target];
    if (expectedMachine) {
      const actualMachine = readPeMachine(expected);
      if (actualMachine !== expectedMachine) {
        archMismatches.push({
          target,
          path: expected,
          expectedMachine,
          actualMachine,
        });
      }
    }
  }
}

if (missing.length > 0) {
  process.stderr.write("[verify-runtime] missing runtime binaries:\n");
  for (const p of missing) {
    process.stderr.write(`- ${p}\n`);
  }
  process.exit(1);
}

if (archMismatches.length > 0) {
  process.stderr.write("[verify-runtime] architecture mismatch:\n");
  for (const mismatch of archMismatches) {
    process.stderr.write(
      `- ${mismatch.target}: expected ${machineName(mismatch.expectedMachine)}, ` +
      `found ${machineName(mismatch.actualMachine)} at ${mismatch.path}\n`
    );
  }
  process.stderr.write(
    "[verify-runtime] build each target on a matching host/runner.\n"
  );
  process.exit(1);
}

process.stdout.write(
  `[verify-runtime] runtime binaries present for targets: ${targets.join(", ")}\n`
);
