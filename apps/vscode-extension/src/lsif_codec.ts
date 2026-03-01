export type LsifLocationData = {
  uri: string;
  startLine: number;
  startCharacter: number;
  endLine: number;
  endCharacter: number;
};

export type LsifReferenceData = {
  location: LsifLocationData;
  declaration: boolean;
};

export type LsifSymbolEntryData = {
  key: string;
  kind: number;
  definitions: LsifLocationData[];
  implementations: LsifLocationData[];
  references: LsifReferenceData[];
};

export type ParsedLsifIndex = {
  symbols: LsifSymbolEntryData[];
  symbolCount: number;
  docCount: number;
  revision: number;
};

export type ParsedLsifDelta = {
  baseRevision: number;
  revision: number;
  reset: boolean;
  deletes: string[];
  upserts: LsifSymbolEntryData[];
};

function asRecord(v: unknown): Record<string, unknown> | undefined {
  return v !== null && typeof v === "object" ? (v as Record<string, unknown>) : undefined;
}

function normalizeCount(v: unknown, fallback = 0): number {
  if (typeof v !== "number") return fallback;
  return Math.max(0, Math.trunc(v));
}

function locationKey(loc: LsifLocationData): string {
  return [
    loc.uri,
    loc.startLine,
    loc.startCharacter,
    loc.endLine,
    loc.endCharacter,
  ].join("|");
}

function parseLsifLocation(v: unknown): LsifLocationData | undefined {
  const rec = asRecord(v);
  if (!rec) return undefined;
  const uriRaw = rec["uri"];
  const rangeRaw = rec["range"];
  if (typeof uriRaw !== "string" || !uriRaw) return undefined;
  const rangeRec = asRecord(rangeRaw);
  if (!rangeRec) return undefined;
  const startRec = asRecord(rangeRec["start"]);
  const endRec = asRecord(rangeRec["end"]);
  if (!startRec || !endRec) return undefined;

  const sl = startRec["line"];
  const sc = startRec["character"];
  const el = endRec["line"];
  const ec = endRec["character"];
  if (
    typeof sl !== "number" ||
    typeof sc !== "number" ||
    typeof el !== "number" ||
    typeof ec !== "number"
  ) {
    return undefined;
  }

  return {
    uri: uriRaw,
    startLine: Math.max(0, Math.trunc(sl)),
    startCharacter: Math.max(0, Math.trunc(sc)),
    endLine: Math.max(0, Math.trunc(el)),
    endCharacter: Math.max(0, Math.trunc(ec)),
  };
}

function dedupeLocations(xs: LsifLocationData[]): LsifLocationData[] {
  const seen = new Set<string>();
  const out: LsifLocationData[] = [];
  for (const x of xs) {
    const key = locationKey(x);
    if (seen.has(key)) continue;
    seen.add(key);
    out.push(x);
  }
  return out;
}

function parseLocArray(value: unknown): LsifLocationData[] {
  if (!Array.isArray(value)) return [];
  return dedupeLocations(
    value
      .map((x) => parseLsifLocation(x))
      .filter((x): x is LsifLocationData => !!x)
  );
}

function parseLsifSymbolEntry(v: unknown): LsifSymbolEntryData | undefined {
  const rec = asRecord(v);
  if (!rec) return undefined;
  const keyRaw = rec["key"];
  if (typeof keyRaw !== "string") return undefined;
  const key = keyRaw.trim().toUpperCase();
  if (!key) return undefined;

  const kind = typeof rec["kind"] === "number" ? Math.trunc(rec["kind"]) : 0;
  const definitions = parseLocArray(rec["definitions"]);
  const implementations = parseLocArray(rec["implementations"]);

  const references: LsifReferenceData[] = [];
  const seenRefs = new Set<string>();
  const refsRaw = rec["references"];
  if (Array.isArray(refsRaw)) {
    for (const r of refsRaw) {
      let location: LsifLocationData | undefined;
      let declaration = false;
      const asRef = asRecord(r);
      if (asRef) {
        location = parseLsifLocation(asRef["location"] ?? r);
        declaration = asRef["declaration"] === true;
      } else {
        location = parseLsifLocation(r);
      }
      if (!location) continue;
      const dedupeKey = `${locationKey(location)}|${declaration ? 1 : 0}`;
      if (seenRefs.has(dedupeKey)) continue;
      seenRefs.add(dedupeKey);
      references.push({ location, declaration });
    }
  }

  return {
    key,
    kind,
    definitions,
    implementations,
    references,
  };
}

export function parseLsifIndexPayload(payload: unknown): ParsedLsifIndex | undefined {
  const root = asRecord(payload);
  if (!root) return undefined;
  const symbolsRaw = root["symbols"];
  if (!Array.isArray(symbolsRaw)) return undefined;

  const symbolsByKey = new Map<string, LsifSymbolEntryData>();
  for (const rawEntry of symbolsRaw) {
    const parsed = parseLsifSymbolEntry(rawEntry);
    if (!parsed) continue;
    symbolsByKey.set(parsed.key, parsed);
  }
  const symbols = Array.from(symbolsByKey.values());

  return {
    symbols,
    symbolCount: normalizeCount(root["symbolCount"], symbols.length),
    docCount: normalizeCount(root["docCount"], 0),
    revision: normalizeCount(root["revision"], 0),
  };
}

export function parseLsifDeltaPayload(payload: unknown): ParsedLsifDelta | undefined {
  const root = asRecord(payload);
  if (!root) return undefined;

  const baseRaw = root["baseRevision"];
  const revRaw = root["revision"];
  const resetRaw = root["reset"];
  if (typeof baseRaw !== "number" || typeof revRaw !== "number" || typeof resetRaw !== "boolean") {
    return undefined;
  }

  const deletes: string[] = [];
  const deletesRaw = root["deletes"];
  if (Array.isArray(deletesRaw)) {
    for (const d of deletesRaw) {
      if (typeof d !== "string") continue;
      const key = d.trim().toUpperCase();
      if (!key) continue;
      deletes.push(key);
    }
  }

  const upserts: LsifSymbolEntryData[] = [];
  const upsertsRaw = root["upserts"];
  if (Array.isArray(upsertsRaw)) {
    for (const u of upsertsRaw) {
      const entry = parseLsifSymbolEntry(u);
      if (!entry) continue;
      upserts.push(entry);
    }
  }

  return {
    baseRevision: normalizeCount(baseRaw, 0),
    revision: normalizeCount(revRaw, 0),
    reset: resetRaw,
    deletes,
    upserts,
  };
}
