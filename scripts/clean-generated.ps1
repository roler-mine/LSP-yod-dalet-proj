$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot

$dirsToRemove = @(
  (Join-Path $repoRoot "server_proj\\_build"),
  (Join-Path $repoRoot "extension_proj\\out")
)

foreach ($dir in $dirsToRemove) {
  if (Test-Path $dir) {
    Remove-Item -Recurse -Force $dir
    Write-Host "Removed directory: $dir"
  }
}

$filesToRemove = @()
$filesToRemove += Get-ChildItem (Join-Path $repoRoot "extension_proj") -Filter "*.vsix" -File -ErrorAction SilentlyContinue

$legacyArchive = Join-Path $repoRoot "extension_proj.zip"
if (Test-Path $legacyArchive) {
  $filesToRemove += Get-Item $legacyArchive
}

$bundleDir = Join-Path $repoRoot "extension_proj\\server"
if (Test-Path $bundleDir) {
  $filesToRemove += Get-ChildItem $bundleDir -Filter "jovial-lsp*" -File -ErrorAction SilentlyContinue
}

foreach ($file in $filesToRemove) {
  Remove-Item -Force $file.FullName
  Write-Host "Removed file: $($file.FullName)"
}

Write-Host "Cleanup complete."
