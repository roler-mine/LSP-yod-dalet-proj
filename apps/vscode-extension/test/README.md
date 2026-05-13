# VS Code Extension Tests

Unit tests run without VS Code:

```powershell
npm run test:unit
```

Integration tests run through a downloaded, pinned VS Code test host:

```powershell
npm run test:integration:pinned
```

`test:integration` and `test:integration:ci` both use the same pinned path. The
version is configured by `config.vscodeTestVersion` in `package.json`; set
`VSCODE_TEST_VERSION=<version>` to try a different downloaded build.

The runner avoids locally installed VS Code by default so local updater state
does not affect automation. Set `VSCODE_TEST_EXECUTABLE_PATH=<path-to-Code>` to
use a specific executable, or `JOVIAL_INTEGRATION_USE_INSTALLED_VSCODE=1` to
opt into the normal local installation search.

If launch fails, the runner prints the pinned version, resolved executable,
temporary workspace paths, and recovery hints. On headless Linux, run under Xvfb
or another display server.
