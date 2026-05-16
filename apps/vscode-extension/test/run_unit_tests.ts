// Module overview: Minimal unit-test runner that loads compiled extension tests without VS Code.

import { run as runConfigTests } from "./jovial_config.test";
import { run as runProviderRaceTests } from "./provider_race.test";
import { run as runWatchedFileQueueTests } from "./watched_file_queue.test";
import { run as runWorkspacePathTests } from "./workspace_paths.test";

const suites: Array<[string, () => void | Promise<void>]> = [
  ["jovial_config", runConfigTests],
  ["provider_race", runProviderRaceTests],
  ["watched_file_queue", runWatchedFileQueueTests],
  ["workspace_paths", runWorkspacePathTests],
];

async function main(): Promise<void> {
  for (const [name, run] of suites) {
    await run();
    console.log(`ok ${name}`);
  }
}

void main().catch((error: unknown) => {
  console.error(error);
  process.exitCode = 1;
});
