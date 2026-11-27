import assert from "node:assert/strict";

function assertAssets(state, initial) {
  const assets = (funds) =>
    funds
      .map(({ id, ada, tokens }) => ({ id, ada, tokens }))
      .sort((a, b) => a.id.localeCompare(b.id));
  assert.deepEqual(
    assets(state.funds),
    assets(initial.funds),
    "each output keeps its assets"
  );
  for (const [totals, original] of [
    [state.totals, initial.totals],
    [state.totals.tokens, initial.totals.tokens],
  ]) {
    assert.equal(
      totals.wallets + totals.deposits + totals.backing,
      original.wallets,
      "L1 value is conserved"
    );
  }
}

export function exercisePlayground(evaluate) {
  const requests = [];
  for (let operators = 1; operators <= 4; operators++) {
    for (const users of [0, 2, 4]) {
      const setup = {
        operators,
        users,
        connections: Array.from(
          { length: users },
          (_, i) => operators - 1 - (i % operators)
        ),
      };
      const initial = evaluate({ setup, actions: [] });
      const balances = initial.people.map((person) =>
        initial.funds
          .filter((fund) => fund.owner === person.id)
          .reduce((sum, fund) => sum + fund.ada, 0)
      );
      assert.equal(
        new Set(balances).size,
        initial.people.length,
        "each person starts with a different ADA balance"
      );
      const actions = [];
      let state;
      const run = (action) => {
        if (action) actions.push(action);
        state = evaluate({ setup, actions });
        assert.equal(state.status, "ok", state.reason);
        assertAssets(state, initial);
        for (const entry of state.decisions) {
          assert.equal(
            entry.outcome.tag,
            "Continue",
            JSON.stringify(entry.outcome)
          );
          for (const effect of entry.outcome.effects) {
            if (
              effect.tag === "NetworkEffect" &&
              effect.message.tag === "ReqSn"
            ) {
              assert.equal(state.signing.number, effect.message.snapshotNumber);
              assert.equal(
                state.signing.leader,
                `operator-${entry.operator}`,
                "the marker follows the actual protocol proposer"
              );
              assert.deepEqual(
                state.signing.signers,
                [],
                "a new proposal clears the previous round's checkmarks"
              );
            }
          }
        }
        return state;
      };
      const drain = () => {
        for (let steps = 0; state.pending; steps++) {
          assert.ok(
            steps < 30,
            "request must finish without invented acknowledgements"
          );
          run({ type: "step" });
        }
      };
      const checkpoint = () =>
        requests.push({
          setup,
          actions: structuredClone(actions),
          audit: true,
        });
      run();
      assert.equal(state.phase, "setup");
      assert.equal(state.signing, null);
      run({ type: "open" });
      assert.equal(state.phase, "opening");
      assert.ok(state.nodes.every((n) => n.state.headState.tag === "Idle"));
      run({ type: "step" });
      assert.equal(state.phase, "open");
      assert.equal(state.totals.backing, 0);
      assert.deepEqual(state.signing, {
        number: 1,
        status: "next",
        leader: "operator-0",
        signers: [],
      });
      checkpoint();

      for (let round = 0; round < 2; round++) {
        const sender = users ? "user-0" : "operator-0";
        const fund = `coin-${(users ? operators * 2 : 0) + round}`;
        const ada = initial.funds.find((f) => f.id === fund).ada;
        run({ type: "move", fund, owner: sender, layer: "l2" });
        assert.match(
          state.funds.find((f) => f.id === fund).reservation,
          /deposit/
        );
        assert.ok(
          Object.values(state.targets).every((moves) => moves.length === 0)
        );
        assert.equal(
          state.totals.deposits,
          0,
          "submission alone does not lock a wallet output"
        );
        run({ type: "step" });
        assert.equal(state.funds.find((f) => f.id === fund).reservation, null);
        assert.equal(state.totals.deposits, ada);
        assert.equal(state.totals.spendable, 0);
        assert.equal(state.totals.backing, 0);
        assert.ok(
          state.nodes.every((n) =>
            Object.values(n.state.deposits)
              .filter((d) => d.consumedAt == null)
              .every((d) => d.deposit.status === "Inactive")
          )
        );
        const before = state.snapshot;
        run({ type: "step" });
        assert.equal(state.signing.number, before + 1);
        assert.equal(state.signing.status, "collecting");
        assert.deepEqual(state.signing.signers, []);
        run({ type: "step" });
        assert.deepEqual(state.signing.signers, []);
        for (let signatures = 1; signatures <= operators; signatures++) {
          run({ type: "step" });
          assert.deepEqual(
            state.signing.signers,
            Array.from({ length: signatures }, (_, i) => `operator-${i}`)
          );
          if (signatures < operators) {
            assert.equal(state.snapshot, before);
            assert.ok(
              state.nodes.every((n) => n.signers.length === signatures)
            );
          }
        }
        assert.equal(state.snapshot, before + 1);
        assert.equal(state.signing.status, "confirmed");
        assert.equal(
          state.totals.spendable,
          0,
          "a signed commit is not yet settled"
        );
        assert.equal(state.totals.deposits, ada);
        run({ type: "step" });
        assert.equal(state.totals.spendable, ada);
        assert.equal(state.totals.backing, ada);
        assert.equal(state.pending, null);
        checkpoint();

        let recipient = sender;
        if (operators + users > 1) {
          recipient = users ? "operator-0" : "operator-1";
          run(
            round === 0
              ? { type: "pay", funds: [fund], owner: recipient }
              : { type: "move", fund, owner: recipient, layer: "l2" }
          );
          assert.equal(
            state.funds.find((f) => f.id === fund).owner,
            sender,
            "a client request does not apply the payment"
          );
          run({ type: "step" });
          assert.equal(state.funds.find((f) => f.id === fund).place, "payment");
          assert.equal(state.totals.backing, ada);
          drain();
          assert.equal(state.funds.find((f) => f.id === fund).owner, recipient);
          assert.equal(state.totals.spendable, ada);
          checkpoint();
        }

        run({ type: "move", fund, owner: recipient, layer: "l1" });
        while (!state.action.button.includes("Confirm withdrawal"))
          run({ type: "step" });
        assert.equal(state.totals.backing, ada);
        assert.equal(state.totals.pending, ada);
        run({ type: "step" });
        assert.equal(state.totals.backing, 0);
        assert.equal(state.totals.spendable, 0);
        assert.equal(state.funds.find((f) => f.id === fund).place, "l1");
        assert.ok(state.nodes.every((n) => n.version === (round + 1) * 2));
        checkpoint();
      }
      const rewind = requests[requests.length - 2];
      assert.deepEqual(evaluate(rewind), evaluate(structuredClone(rewind)));
      requests.push(
        structuredClone(rewind),
        structuredClone(requests[requests.length - 1])
      );
    }
  }
  return requests;
}

export function exercisePayments(evaluate) {
  const setup = { operators: 2, users: 1, connections: [1] };
  const initial = evaluate({ setup, actions: [] });
  const actions = [];
  const requests = [];
  let state;
  const run = (action) => {
    actions.push(action);
    const request = { setup, actions: structuredClone(actions), audit: true };
    state = evaluate(request);
    assert.equal(state.status, "ok", state.reason);
    assertAssets(state, initial);
    requests.push(request);
  };
  const finish = () => {
    for (let steps = 0; state.pending; steps++) {
      assert.ok(steps < 20);
      run({ type: "step" });
    }
  };
  run({ type: "open" });
  finish();
  for (const [fund, owner] of [
    ["coin-0", "operator-0"],
    ["coin-4", "user-0"],
    ["coin-5", "user-0"],
  ]) {
    run({ type: "move", fund, owner, layer: "l2" });
    assert.match(state.funds.find((f) => f.id === fund).reservation, /deposit/);
    const before = evaluate({ setup, actions: actions.slice(0, -1) });
    assert.equal(before.funds.find((f) => f.id === fund).reservation, null);
    assert.deepEqual(evaluate(requests.at(-1)), state);
    finish();
  }
  for (const funds of [
    [],
    ["coin-4", "coin-4"],
    ["coin-0", "coin-4"],
    ["coin-1"],
    ["missing"],
  ]) {
    assert.equal(
      evaluate({
        setup,
        actions: [...actions, { type: "pay", funds, owner: "operator-1" }],
      }).status,
      "error"
    );
  }
  const beforePayment = requests.at(-1);
  run({ type: "pay", funds: ["coin-4", "coin-5"], owner: "operator-0" });
  const entry = state.decisions[0];
  assert.equal(
    entry.operator,
    1,
    "the wallet submits through its chosen operator"
  );
  assert.equal(entry.input.clientInput.tag, "NewTx");
  assert.equal(entry.input.clientInput.transaction.inputs.length, 2);
  assert.equal(entry.input.clientInput.transaction.outputs.length, 2);
  assert.equal(
    state.funds.filter((f) => f.reservation?.includes("payment")).length,
    2
  );
  assert.equal(
    evaluate({
      setup,
      actions: [
        ...actions,
        { type: "pay", funds: ["coin-4"], owner: "operator-1" },
      ],
    }).status,
    "error"
  );
  run({ type: "step" });
  assert.equal(state.funds.filter((f) => f.place === "payment").length, 2);
  finish();
  for (const id of ["coin-4", "coin-5"]) {
    const fund = state.funds.find((f) => f.id === id);
    assert.equal(fund.owner, "operator-0");
    assert.equal(fund.place, "l2");
    assert.equal(fund.reservation, null);
  }
  assert.equal(state.totals.backing, 71);
  assert.equal(state.totals.spendable, 71);
  assert.equal(
    evaluate(beforePayment).funds.find((f) => f.id === "coin-4").owner,
    "user-0"
  );
  assert.deepEqual(evaluate(requests.at(-1)), state);
  return requests;
}

export function rejectionRequests() {
  const setup = { operators: 2, users: 1 };
  const client = (input) => ({ type: "client", node: 0, input });
  const tx = { id: 42, inputs: [9999], outputs: [10000] };
  return [
    {
      setup,
      actions: [client({ tag: "NewTx", transaction: tx })],
      audit: true,
    },
    {
      setup,
      actions: [
        { type: "open" },
        { type: "step" },
        client({ tag: "Decommit", decommitTx: tx }),
      ],
      audit: true,
    },
    {
      setup,
      actions: [
        { type: "open" },
        { type: "step" },
        client({ tag: "NewTx", transaction: tx }),
        { type: "step" },
      ],
      audit: true,
    },
  ];
}

export function exerciseTokenLifecycle(evaluate) {
  const requests = [];
  for (let operators = 1; operators <= 4; operators++) {
    for (const [tokenId, trade] of [
      ["coin-0", false],
      ["coin-0", true],
      ["token-only", false],
      ["token-only", true],
    ]) {
      const setup = { operators, users: 1, connections: [operators - 1] };
      const initial = evaluate({ setup, actions: [] });
      const actions = [];
      let state;
      const run = (action) => {
        if (action) actions.push(action);
        const request = {
          setup,
          actions: structuredClone(actions),
          audit: true,
        };
        state = evaluate(request);
        assert.equal(state.status, "ok", state.reason);
        assertAssets(state, initial);
        requests.push(request);
      };
      const reject = (action) =>
        assert.equal(
          evaluate({ setup, actions: [...actions, action] }).status,
          "error"
        );
      const finish = () => {
        for (let steps = 0; state.pending; steps++) {
          assert.ok(steps < 30);
          run({ type: "step" });
        }
      };
      const token = () => state.funds.find((fund) => fund.id === tokenId);
      run();
      assert.equal(token().ada, tokenId === "token-only" ? 0 : 10);
      assert.equal(token().owner, "operator-0");
      assert.equal(token().place, "l1");
      reject({ type: "close" });
      run({ type: "open" });
      finish();
      run({ type: "move", fund: tokenId, owner: "operator-0", layer: "l2" });
      assert.ok(token().reservation);
      assert.equal(state.canClose, false);
      reject({ type: "close" });
      run({ type: "step" });
      assert.equal(state.totals.tokens.deposits, 1);
      assert.equal(state.totals.tokens.spendable, 0);
      finish();
      assert.equal(
        state.phase,
        "open",
        "a deposit leaves closing up to the user"
      );
      assert.equal(state.totals.tokens.backing, 1);
      assert.equal(state.totals.tokens.spendable, 1);

      const swap = { type: "swap", fund: tokenId, owner: "user-0" };
      reject(swap);
      if (trade) {
        for (let i = 0; i < 2; i++) {
          run({
            type: "move",
            fund: `coin-${operators * 2 + i}`,
            owner: "user-0",
            layer: "l2",
          });
          finish();
          if (i === 0) reject(swap);
        }
        reject({ ...swap, owner: "operator-0" });
        reject({ ...swap, fund: `coin-${operators * 2}` });
        const before = requests.at(-1);
        const backing = state.totals.backing;
        run(swap);
        const input = state.decisions[0].input.clientInput;
        assert.equal(input.tag, "NewTx");
        assert.equal(input.transaction.inputs.length, 3);
        assert.equal(input.transaction.outputs.length, 3);
        assert.equal(state.funds.filter((fund) => fund.reservation).length, 3);
        assert.equal(token().owner, "operator-0");
        run({ type: "step" });
        assert.equal(
          state.totals.backing,
          backing,
          "the exchange does not settle on L1"
        );
        finish();
        assert.equal(token().owner, "user-0");
        assert.equal(token().place, "l2");
        for (let i = 0; i < 2; i++)
          assert.equal(
            state.funds.find((fund) => fund.id === `coin-${operators * 2 + i}`)
              .owner,
            "operator-0"
          );
        assert.equal(
          evaluate(before).funds.find((fund) => fund.id === tokenId).owner,
          "operator-0"
        );
        assert.deepEqual(evaluate(requests.at(-1)), state);
      }

      const locked = token().ada + (trade ? 20 * (operators + 1) + 1 : 0);
      assert.equal(state.canClose, true);
      run({ type: "close" });
      assert.equal(state.phase, "open", "posting a close is not confirmation");
      assert.equal(state.canClose, false);
      assert.ok(
        state.nodes.every((node) => node.state.headState.tag === "Open")
      );
      run({ type: "step" });
      assert.equal(state.phase, "closed");
      assert.equal(state.signing, null);
      assert.equal(state.totals.closed, locked);
      assert.equal(state.totals.tokens.closed, 1);
      assert.equal(state.totals.spendable, 0);
      assert.equal(state.canFanout, false);
      assert.equal(token().place, "closed");
      assert.ok(
        Object.values(state.targets).every((targets) => targets.length === 0)
      );
      assert.ok(
        state.nodes.every(
          (node) =>
            node.state.headState.tag === "Closed" &&
            !node.state.headState.contents.readyToFanoutSent
        )
      );
      reject({ type: "fanout" });
      reject(swap);
      run({ type: "step" });
      assert.equal(state.canFanout, true);
      assert.ok(
        state.nodes.every(
          (node) => node.state.headState.contents.readyToFanoutSent
        )
      );
      assert.ok(
        state.decisions.every((entry) =>
          entry.outcome.stateChanges.some(
            (change) => change.tag === "HeadIsReadyToFanout"
          )
        )
      );
      const beforeFanout = requests.at(-1);
      run({ type: "fanout" });
      assert.equal(state.phase, "fanout");
      assert.equal(state.totals.backing, locked);
      assert.equal(state.totals.closed, locked);
      assert.equal(state.canFanout, false);
      run({ type: "step" });
      assert.equal(state.phase, "finalized");
      assert.equal(state.totals.backing, 0);
      assert.equal(state.totals.closed, 0);
      assert.equal(state.totals.tokens.wallets, 2);
      assert.equal(token().owner, trade ? "user-0" : "operator-0");
      assert.ok(state.funds.every((fund) => fund.place === "l1"));
      assert.ok(
        state.nodes.every((node) => node.state.headState.tag === "Finalized")
      );
      assert.equal(evaluate(beforeFanout).phase, "closed");
      assert.deepEqual(evaluate(requests.at(-1)), state);
    }
    const emptyHead = {
      setup: { operators, users: 0 },
      actions: ["open", "step", "close", "step", "step", "fanout", "step"].map(
        (type) => ({ type })
      ),
      audit: true,
    };
    const finalized = evaluate(emptyHead);
    assert.equal(finalized.phase, "finalized");
    assertAssets(finalized, evaluate({ setup: emptyHead.setup, actions: [] }));
    assert.equal(finalized.totals.tokens.wallets, 2);
    requests.push(emptyHead);
  }
  return requests;
}
