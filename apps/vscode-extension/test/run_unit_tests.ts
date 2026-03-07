import { run as runConfigTests } from "./jovial_config.test";
import { run as runWatchedFileQueueTests } from "./watched_file_queue.test";
import { run as runWorkspacePathTests } from "./workspace_paths.test";

const suites: Array<[string, () => void]> = [
  ["jovial_config", runConfigTests],
  ["watched_file_queue", runWatchedFileQueueTests],
  ["workspace_paths", runWorkspacePathTests],
];

for (const [name, run] of suites) {
  run();
  console.log(`ok ${name}`);
}
