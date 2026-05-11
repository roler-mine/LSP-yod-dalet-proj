export type ProviderRaceDisposable = {
  dispose(): unknown;
};

export type ProviderRaceCancellationToken = {
  readonly isCancellationRequested: boolean;
  onCancellationRequested?: (
    listener: (event: unknown) => unknown,
  ) => ProviderRaceDisposable;
};

type ProviderRaceSignal = { kind: "budget" } | { kind: "cancelled" };

type ProviderServerOutcome<T> =
  | { kind: "server"; value: T }
  | { kind: "serverError"; error: unknown };

type ProviderRaceGate = {
  promise: Promise<ProviderRaceSignal>;
  dispose(): void;
};

export type RaceServerWithFallbackOptions<T> = {
  budgetMs: number;
  token: ProviderRaceCancellationToken;
  server: () => T | PromiseLike<T>;
  fallback: () => T | undefined;
  hasResult: (value: T | undefined) => boolean;
  normalizeServerResult?: (value: T) => T;
  normalizeFallbackResult?: (value: T) => T;
  onServerError?: (error: unknown) => void;
};

function createRaceGate(
  token: ProviderRaceCancellationToken,
  budgetMs?: number,
): ProviderRaceGate {
  let timer: ReturnType<typeof setTimeout> | undefined;
  let disposable: ProviderRaceDisposable | undefined;
  let settled = false;
  let resolvePromise: (signal: ProviderRaceSignal) => void = () => undefined;

  const cleanup = (): void => {
    if (timer) {
      clearTimeout(timer);
      timer = undefined;
    }
    if (disposable) {
      disposable.dispose();
      disposable = undefined;
    }
  };

  const finish = (signal: ProviderRaceSignal): void => {
    if (settled) return;
    settled = true;
    cleanup();
    resolvePromise(signal);
  };

  const promise = new Promise<ProviderRaceSignal>((resolve) => {
    resolvePromise = resolve;
  });

  if (token.isCancellationRequested) {
    finish({ kind: "cancelled" });
    return { promise, dispose: cleanup };
  }

  if (budgetMs !== undefined) {
    timer = setTimeout(
      () => finish({ kind: "budget" }),
      Math.max(0, Math.trunc(budgetMs)),
    );
  }

  if (token.onCancellationRequested) {
    disposable = token.onCancellationRequested(() =>
      finish({ kind: "cancelled" }),
    );
  }

  if (token.isCancellationRequested) {
    finish({ kind: "cancelled" });
  }

  return { promise, dispose: cleanup };
}

function startServer<T>(
  server: () => T | PromiseLike<T>,
): Promise<ProviderServerOutcome<T>> {
  return Promise.resolve()
    .then(server)
    .then(
      (value) => ({ kind: "server", value }) as const,
      (error: unknown) => ({ kind: "serverError", error }) as const,
    );
}

export async function raceServerWithFallback<T>(
  options: RaceServerWithFallbackOptions<T>,
): Promise<T | undefined> {
  const serverOutcome = startServer(options.server);
  let fallbackRead = false;
  let fallbackResult: T | undefined;

  const readFallback = (): T | undefined => {
    if (options.token.isCancellationRequested) return undefined;
    if (!fallbackRead) {
      fallbackRead = true;
      fallbackResult = options.fallback();
    }
    return fallbackResult;
  };

  const normalizeServer = (value: T): T =>
    options.normalizeServerResult
      ? options.normalizeServerResult(value)
      : value;
  const normalizeFallback = (value: T): T =>
    options.normalizeFallbackResult
      ? options.normalizeFallbackResult(value)
      : value;

  const settleServerOutcome = (
    outcome: ProviderServerOutcome<T>,
  ): T | undefined => {
    if (outcome.kind === "serverError") {
      options.onServerError?.(outcome.error);
      const fallback = readFallback();
      return options.hasResult(fallback)
        ? normalizeFallback(fallback as T)
        : undefined;
    }

    if (options.hasResult(outcome.value)) {
      return normalizeServer(outcome.value);
    }
    const fallback = readFallback();
    return options.hasResult(fallback)
      ? normalizeFallback(fallback as T)
      : outcome.value;
  };

  const initialGate = createRaceGate(options.token, options.budgetMs);
  const first = await Promise.race([serverOutcome, initialGate.promise]);
  initialGate.dispose();

  if (first.kind === "server" || first.kind === "serverError") {
    return settleServerOutcome(first);
  }
  if (first.kind === "cancelled") {
    return undefined;
  }

  const fallback = readFallback();
  if (options.hasResult(fallback)) {
    return normalizeFallback(fallback as T);
  }

  const cancellationGate = createRaceGate(options.token);
  const late = await Promise.race([serverOutcome, cancellationGate.promise]);
  cancellationGate.dispose();

  if (late.kind === "cancelled" || late.kind === "budget") {
    return undefined;
  }
  return settleServerOutcome(late);
}
