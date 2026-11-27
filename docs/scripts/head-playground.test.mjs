import assert from "node:assert/strict";
import { after, test } from "node:test";
import { loadPlayground } from "./compiled-playground.mjs";
import {
  exercisePlayground,
  exercisePayments,
  rejectionRequests,
} from "./playground-scenarios.mjs";

const { evaluate, close } = await loadPlayground();
after(close);

test("symbolic witnesses bind signers, snapshots and accumulator contents", () => {
  assert.deepEqual(evaluate({ diagnostic: "witnesses" }), {
    valid: true,
    bothSigners: true,
    wrongSigner: false,
    missingSignature: false,
    extraSignature: false,
    reorderedSignatures: false,
    changedNumber: false,
    changedVersion: false,
    changedVersionAndNumber: false,
    changedOutputs: false,
    changedDeposit: false,
    accumulatorOrderIndependent: true,
    accumulatorBindsMultiplicity: true,
    accumulatorBindsContents: true,
  });
});

test("production Crem workflows open, deposit, pay and withdraw for every operator count", () => {
  const scenarios = exercisePlayground(evaluate);
  assert.ok(scenarios.length >= 80);
});

test("payments spend several outputs through NewTx and replay reservations", () => {
  exercisePayments(evaluate);
});

test("real protocol guards reject or defer invalid client requests", () => {
  for (const request of rejectionRequests()) {
    const result = evaluate(request);
    assert.equal(result.status, "ok", result.reason);
    const refused = result.decisions.flatMap((e) =>
      e.steps.map((s) => s.label)
    );
    assert.ok(
      refused.some((label) =>
        /invalid|client effect|Rejected|Waiting/.test(label)
      ),
      refused.join(", ")
    );
    assert.equal(result.totals.backing, 0);
  }
});

test("invalid setup, impossible moves and excessive replay are explicit errors", () => {
  const setup = { operators: 2, users: 2 };
  for (const request of [
    { setup: { ...setup, operators: 0 } },
    { setup: { ...setup, users: 5 } },
    { setup: { ...setup, connections: [0, 4] } },
    {
      setup,
      actions: [
        { type: "move", fund: "coin-0", owner: "operator-0", layer: "l2" },
      ],
    },
    { setup, actions: [{ type: "step" }] },
    { setup, actions: Array.from({ length: 257 }, () => ({ type: "step" })) },
  ])
    assert.equal(evaluate(request).status, "error");
});
