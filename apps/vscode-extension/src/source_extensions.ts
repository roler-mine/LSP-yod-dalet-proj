// Module overview: Builds normalized source-extension sets and workspace watcher globs for Jovial files.

export const DEFAULT_JOVIAL_SOURCE_EXTENSIONS = [".jov", ".j73", ".jvl"];
export const DEFAULT_ASSEMBLY_SOURCE_EXTENSIONS = [".asm"];

export function normalizeSourceExtension(value: string): string | undefined {
  const trimmed = value.trim().toLowerCase();
  if (!trimmed) return undefined;
  const ext = trimmed.startsWith(".") ? trimmed : `.${trimmed}`;
  if (!/^\.[a-z0-9_]+$/.test(ext)) return undefined;
  return ext;
}

export function sanitizeSourceExtensions(value: unknown): string[] {
  if (!Array.isArray(value)) return [];
  const seen = new Set<string>();
  const out: string[] = [];
  for (const item of value) {
    if (typeof item !== "string") continue;
    const ext = normalizeSourceExtension(item);
    if (!ext || seen.has(ext)) continue;
    seen.add(ext);
    out.push(ext);
  }
  return out;
}

export function sourceExtensionsWithDefaults(
  extra: readonly string[],
): string[] {
  const seen = new Set<string>();
  const out: string[] = [];
  for (const item of [...DEFAULT_JOVIAL_SOURCE_EXTENSIONS, ...extra]) {
    const ext = normalizeSourceExtension(item);
    if (!ext || seen.has(ext)) continue;
    seen.add(ext);
    out.push(ext);
  }
  return out;
}

export function assemblyExtensionsWithDefaults(
  extra: readonly string[],
): string[] {
  const seen = new Set<string>();
  const out: string[] = [];
  for (const item of [...DEFAULT_ASSEMBLY_SOURCE_EXTENSIONS, ...extra]) {
    const ext = normalizeSourceExtension(item);
    if (!ext || seen.has(ext)) continue;
    seen.add(ext);
    out.push(ext);
  }
  return out;
}

export function hasJovialSourceExtension(
  fsPath: string,
  extensions: readonly string[],
): boolean {
  const lower = fsPath.toLowerCase();
  return extensions.some((ext) => lower.endsWith(ext));
}

export function watcherGlobForSourceExtensions(
  extensions: readonly string[],
): string {
  const names = extensions
    .map((ext) => normalizeSourceExtension(ext))
    .filter((ext): ext is string => !!ext)
    .map((ext) => ext.slice(1));
  if (names.length === 0) return "**/*.{jov,j73,jvl}";
  if (names.length === 1) return `**/*.${names[0]}`;
  return `**/*.{${names.join(",")}}`;
}
