// Module overview: Bundles the VS Code extension host code and LSIF worker with esbuild.

import * as fs from "node:fs";

import esbuild from "esbuild";

fs.rmSync("out", { recursive: true, force: true });

const common = {
  bundle: true,
  platform: "node",
  target: "node18",
  format: "cjs",
  sourcemap: true,
  external: ["vscode"],
  logLevel: "info",
};

await Promise.all([
  esbuild.build({
    ...common,
    entryPoints: ["src/extension.ts"],
    outfile: "out/extension.js",
  }),
  esbuild.build({
    ...common,
    entryPoints: ["src/lsif_worker.ts"],
    outfile: "out/lsif_worker.js",
  }),
]);
