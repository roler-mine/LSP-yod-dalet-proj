# Module overview: Removes generated build, cache, extension, and packaging artifacts from the workspace.

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)

$dirsToRemove = @(
  (Join-Path $repoRoot "_build"),
  (Join-Path $repoRoot ".jovial-lsp"),
  (Join-Path $repoRoot "apps\\.jovial-lsp"),
  (Join-Path $repoRoot "apps\\lsp-server\\_build"),
  (Join-Path $repoRoot "apps\\lsp-server\\_build-linux-x64"),
  (Join-Path $repoRoot "apps\\lsp-server\\_build-linux-arm64"),
  (Join-Path $repoRoot "apps\\lsp-server\\_build-win32-x64"),
  (Join-Path $repoRoot "apps\\lsp-server\\_build-win32-arm64"),
  (Join-Path $repoRoot "apps\\vscode-extension\\out"),
  (Join-Path $repoRoot "apps\\vscode-extension\\out-test")
)

foreach ($dir in $dirsToRemove) {
  if (Test-Path $dir) {
    try {
      Remove-Item -Recurse -Force $dir -ErrorAction Stop
      Write-Host "Removed directory: $dir"
    } catch {
      Write-Warning "Skipped directory: $dir ($($_.Exception.Message))"
    }
  }
}

$filesToRemove = @()
$filesToRemove += Get-ChildItem (Join-Path $repoRoot "apps\\vscode-extension") -Filter "*.vsix" -File -ErrorAction SilentlyContinue

$legacyArchive = Join-Path $repoRoot "apps\\vscode-extension.zip"
if (Test-Path $legacyArchive) {
  $filesToRemove += Get-Item $legacyArchive
}

$legacyArchiveRoot = Join-Path $repoRoot "extension_proj.zip"
if (Test-Path $legacyArchiveRoot) {
  $filesToRemove += Get-Item $legacyArchiveRoot
}

$legacyAppsArchiveRoot = Join-Path $repoRoot "apps.zip"
if (Test-Path $legacyAppsArchiveRoot) {
  $filesToRemove += Get-Item $legacyAppsArchiveRoot
}

$runtimeServerDir = Join-Path $repoRoot "apps\\vscode-extension\\runtime\\server"
if (Test-Path $runtimeServerDir) {
  $filesToRemove += Get-ChildItem $runtimeServerDir -Recurse -Filter "jovial-lsp*" -File -ErrorAction SilentlyContinue
}

$legacyBundleDir = Join-Path $repoRoot "apps\\vscode-extension\\server"
if (Test-Path $legacyBundleDir) {
  $filesToRemove += Get-ChildItem $legacyBundleDir -Filter "jovial-lsp*" -File -ErrorAction SilentlyContinue
}

foreach ($file in $filesToRemove) {
  try {
    Remove-Item -Force $file.FullName -ErrorAction Stop
    Write-Host "Removed file: $($file.FullName)"
  } catch {
    Write-Warning "Skipped file: $($file.FullName) ($($_.Exception.Message))"
  }
}

Write-Host "Cleanup complete."
