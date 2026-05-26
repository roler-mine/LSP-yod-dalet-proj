// Module overview: Tests for the provider race.test extension module.

import assert from "node:assert/strict";

import { raceServerWithFallback } from "../src/provider_race";
import type { ProviderRaceCancellationToken } from "../src/provider_race";

type CancellationListener = (event: unknown) => unknown;

class TestCancellationToken implements ProviderRaceCancellationToken {
  public isCancellationRequested = false;

  private readonly listeners = new Set<CancellationListener>();

  public onCancellationRequested(listener: CancellationListener): {
    dispose(): void;
  } {
    this.listeners.add(listener);
    return {
      dispose: () => {
        this.listeners.delete(listener);
      },
    };
  }

  public cancel(): void {
    if (this.isCancellationRequested) return;
    this.isCancellationRequested = true;
    for (const listener of Array.from(this.listeners)) {
      listener(undefined);
    }
  }

  public listenerCount(): number {
    return this.listeners.size;
  }
}

function delay<T>(ms: number, value: T): Promise<T> {
  return new Promise((resolve) => setTimeout(() => resolve(value), ms));
}

function never<T>(): Promise<T> {
  return new Promise<T>(() => undefined);
}

function hasResult(value: string | undefined): boolean {
  return value !== undefined && value.length > 0;
}

async function testServerWinsBeforeBudget(): Promise<void> {
  const token = new TestCancellationToken();
  let fallbackCalls = 0;

  const result = await raceServerWithFallback<string | undefined>({
    budgetMs: 25,
    token,
    server: () => Promise.resolve("server"),
    fallback: () => {
      fallbackCalls += 1;
      return "fallback";
    },
    hasResult,
  });

  assert.equal(result, "server");
  assert.equal(fallbackCalls, 0);
  assert.equal(token.listenerCount(), 0);
}

async function testFallbackWinsAfterBudget(): Promise<void> {
  const token = new TestCancellationToken();

  const result = await raceServerWithFallback<string | undefined>({
    budgetMs: 1,
    token,
    server: () => delay(25, "server"),
    fallback: () => "fallback",
    hasResult,
  });

  assert.equal(result, "fallback");
  assert.equal(token.listenerCount(), 0);
}

async function testLateServerCanBeatFallbackWhenPreferred(): Promise<void> {
  const token = new TestCancellationToken();

  const result = await raceServerWithFallback<string | undefined>({
    budgetMs: 1,
    fallbackServerBudgetMs: 40,
    preferLateServerResult: true,
    token,
    server: () => delay(10, "server"),
    fallback: () => "fallback",
    hasResult,
  });

  assert.equal(result, "server");
  assert.equal(token.listenerCount(), 0);
}

async function testFallbackWinsAfterServerGrace(): Promise<void> {
  const token = new TestCancellationToken();

  const result = await raceServerWithFallback<string | undefined>({
    budgetMs: 1,
    fallbackServerBudgetMs: 5,
    preferLateServerResult: true,
    token,
    server: () => delay(50, "server"),
    fallback: () => "fallback",
    hasResult,
  });

  assert.equal(result, "fallback");
  assert.equal(token.listenerCount(), 0);
}

async function testQuickEmptyServerFallsBack(): Promise<void> {
  const token = new TestCancellationToken();

  const result = await raceServerWithFallback<string | undefined>({
    budgetMs: 25,
    token,
    server: () => undefined,
    fallback: () => "fallback",
    hasResult,
  });

  assert.equal(result, "fallback");
  assert.equal(token.listenerCount(), 0);
}

async function testMissingFallbackWaitsForServer(): Promise<void> {
  const token = new TestCancellationToken();
  let fallbackCalls = 0;

  const result = await raceServerWithFallback<string | undefined>({
    budgetMs: 1,
    lateBudgetMs: 25,
    token,
    server: () => delay(10, "server"),
    fallback: () => {
      fallbackCalls += 1;
      return undefined;
    },
    hasResult,
  });

  assert.equal(result, "server");
  assert.equal(fallbackCalls, 1);
  assert.equal(token.listenerCount(), 0);
}

async function testMissingFallbackStopsAtLateBudget(): Promise<void> {
  const token = new TestCancellationToken();
  let fallbackCalls = 0;
  const started = Date.now();

  const result = await raceServerWithFallback<string | undefined>({
    budgetMs: 1,
    lateBudgetMs: 5,
    token,
    server: () => delay(50, "server"),
    fallback: () => {
      fallbackCalls += 1;
      return undefined;
    },
    hasResult,
  });

  assert.equal(result, undefined);
  assert.equal(fallbackCalls, 1);
  assert.equal(token.listenerCount(), 0);
  assert.ok(Date.now() - started < 45);
}

async function testCancellationUsesFallback(): Promise<void> {
  const token = new TestCancellationToken();
  let fallbackCalls = 0;

  const resultPromise = raceServerWithFallback<string | undefined>({
    budgetMs: 50,
    token,
    server: () => never(),
    fallback: () => {
      fallbackCalls += 1;
      return "fallback";
    },
    hasResult,
  });
  setTimeout(() => token.cancel(), 1);

  const result = await resultPromise;
  assert.equal(result, "fallback");
  assert.equal(fallbackCalls, 1);
  assert.equal(token.listenerCount(), 0);
}

async function testLateCancellationKeepsFallback(): Promise<void> {
  const token = new TestCancellationToken();

  const resultPromise = raceServerWithFallback<string | undefined>({
    budgetMs: 1,
    fallbackServerBudgetMs: 50,
    preferLateServerResult: true,
    token,
    server: () => never(),
    fallback: () => "fallback",
    hasResult,
  });
  setTimeout(() => token.cancel(), 5);

  const result = await resultPromise;
  assert.equal(result, "fallback");
  assert.equal(token.listenerCount(), 0);
}

async function testServerFailureFallsBack(): Promise<void> {
  const token = new TestCancellationToken();
  const errors: unknown[] = [];

  const result = await raceServerWithFallback<string | undefined>({
    budgetMs: 25,
    token,
    server: () => Promise.reject(new Error("boom")),
    fallback: () => "fallback",
    hasResult,
    onServerError: (error) => {
      errors.push(error);
    },
  });

  assert.equal(result, "fallback");
  assert.equal(errors.length, 1);
  assert.equal(token.listenerCount(), 0);
}

export async function run(): Promise<void> {
  await testServerWinsBeforeBudget();
  await testFallbackWinsAfterBudget();
  await testLateServerCanBeatFallbackWhenPreferred();
  await testFallbackWinsAfterServerGrace();
  await testQuickEmptyServerFallsBack();
  await testMissingFallbackWaitsForServer();
  await testMissingFallbackStopsAtLateBudget();
  await testCancellationUsesFallback();
  await testLateCancellationKeepsFallback();
  await testServerFailureFallsBack();
}
