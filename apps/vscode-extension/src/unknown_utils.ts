export function asRecord(v: unknown): Record<string, unknown> | undefined {
  return v !== null && typeof v === "object"
    ? (v as Record<string, unknown>)
    : undefined;
}

export function asFiniteInt(value: unknown): number | undefined {
  if (typeof value !== "number" || !Number.isFinite(value)) return undefined;
  return Math.max(0, Math.trunc(value));
}
