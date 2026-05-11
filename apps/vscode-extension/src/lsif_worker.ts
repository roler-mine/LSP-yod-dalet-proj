import { parentPort } from "worker_threads";
import { parseLsifDeltaPayload, parseLsifIndexPayload } from "./lsif_codec";
import { asRecord } from "./unknown_utils";

type WorkerRequestKind = "parseIndex" | "parseDelta";

type WorkerRequest = {
  id: number;
  kind: WorkerRequestKind;
  payload: unknown;
};

type WorkerSuccessResponse = {
  id: number;
  ok: true;
  result: unknown;
};

type WorkerErrorResponse = {
  id: number;
  ok: false;
  error: string;
};

function parseRequest(value: unknown): WorkerRequest | undefined {
  const rec = asRecord(value);
  if (!rec) return undefined;
  const idRaw = rec["id"];
  const kindRaw = rec["kind"];
  if (typeof idRaw !== "number") return undefined;
  if (kindRaw !== "parseIndex" && kindRaw !== "parseDelta") return undefined;
  return {
    id: Math.trunc(idRaw),
    kind: kindRaw,
    payload: rec["payload"],
  };
}

function send(payload: WorkerSuccessResponse | WorkerErrorResponse): void {
  if (!parentPort) return;
  parentPort.postMessage(payload);
}

if (parentPort) {
  parentPort.on("message", (value: unknown) => {
    const req = parseRequest(value);
    if (!req) return;
    try {
      const result =
        req.kind === "parseIndex"
          ? parseLsifIndexPayload(req.payload)
          : parseLsifDeltaPayload(req.payload);
      send({
        id: req.id,
        ok: true,
        result: result ?? null,
      });
    } catch (e) {
      send({
        id: req.id,
        ok: false,
        error: e instanceof Error ? e.message : String(e),
      });
    }
  });
}
