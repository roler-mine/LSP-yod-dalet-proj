$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)

$dirsToRemove = @(
  (Join-Path $repoRoot "apps\\lsp-server\\_build"),
  (Join-Path $repoRoot "apps\\lsp-server\\_build-linux-x64"),
  (Join-Path $repoRoot "apps\\lsp-server\\_build-linux-arm64"),
  (Join-Path $repoRoot "apps\\lsp-server\\_build-win32-x64"),
  (Join-Path $repoRoot "apps\\lsp-server\\_build-win32-arm64"),
  (Join-Path $repoRoot "apps\\vscode-extension\\out")
)

foreach ($dir in $dirsToRemove) {
  if (Test-Path $dir) {
    Remove-Item -Recurse -Force $dir
    Write-Host "Removed directory: $dir"
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

$runtimeServerDir = Join-Path $repoRoot "apps\\vscode-extension\\runtime\\server"
if (Test-Path $runtimeServerDir) {
  $filesToRemove += Get-ChildItem $runtimeServerDir -Recurse -Filter "jovial-lsp*" -File -ErrorAction SilentlyContinue
}

$legacyBundleDir = Join-Path $repoRoot "apps\\vscode-extension\\server"
if (Test-Path $legacyBundleDir) {
  $filesToRemove += Get-ChildItem $legacyBundleDir -Filter "jovial-lsp*" -File -ErrorAction SilentlyContinue
}

foreach ($file in $filesToRemove) {
  Remove-Item -Force $file.FullName
  Write-Host "Removed file: $($file.FullName)"
}

Write-Host "Cleanup complete."
