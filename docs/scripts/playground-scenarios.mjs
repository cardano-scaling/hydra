import assert from "node:assert/strict";

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
      const actions = [];
      let state;
      const run = (action) => {
        if (action) actions.push(action);
        state = evaluate({ setup, actions });
        assert.equal(state.status, "ok", state.reason);
        assert.equal(
          state.totals.wallets + state.totals.deposits + state.totals.backing,
          (operators + users) * 20,
          "L1 value is conserved at every step"
        );
        assert.equal(
          new Set(state.funds.map((fund) => fund.id)).size,
          (operators + users) * 2,
          "each chip appears exactly once"
        );
        assert.equal(state.funds.length, (operators + users) * 2);
        for (const entry of state.decisions)
          assert.equal(
            entry.outcome.tag,
            "Continue",
            JSON.stringify(entry.outcome)
          );
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
      run({ type: "open" });
      assert.equal(state.phase, "opening");
      assert.ok(state.nodes.every((n) => n.state.headState.tag === "Idle"));
      run({ type: "step" });
      assert.equal(state.phase, "open");
      assert.equal(state.totals.backing, 0);
      checkpoint();

      for (let round = 0; round < 2; round++) {
        const sender = users ? "user-0" : "operator-0";
        const fund = `coin-${(users ? operators * 2 : 0) + round}`;
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
        assert.equal(state.totals.deposits, 10);
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
        run({ type: "step" });
        for (let signatures = 1; signatures <= operators; signatures++) {
          run({ type: "step" });
          if (signatures < operators) {
            assert.equal(state.snapshot, before);
            assert.ok(
              state.nodes.every((n) => n.signers.length === signatures)
            );
          }
        }
        assert.equal(state.snapshot, before + 1);
        assert.equal(
          state.totals.spendable,
          0,
          "a signed commit is not yet settled"
        );
        assert.equal(state.totals.deposits, 10);
        run({ type: "step" });
        assert.equal(state.totals.spendable, 10);
        assert.equal(state.totals.backing, 10);
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
          assert.equal(state.totals.backing, 10);
          drain();
          assert.equal(state.funds.find((f) => f.id === fund).owner, recipient);
          assert.equal(state.totals.spendable, 10);
          checkpoint();
        }

        run({ type: "move", fund, owner: recipient, layer: "l1" });
        while (!state.action.button.includes("Confirm withdrawal"))
          run({ type: "step" });
        assert.equal(state.totals.backing, 10);
        assert.equal(state.totals.pending, 10);
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
  const actions = [];
  const requests = [];
  let state;
  const run = (action) => {
    actions.push(action);
    const request = { setup, actions: structuredClone(actions), audit: true };
    state = evaluate(request);
    assert.equal(state.status, "ok", state.reason);
    assert.equal(
      state.totals.wallets + state.totals.deposits + state.totals.backing,
      60
    );
    assert.equal(state.funds.length, 6);
    assert.equal(new Set(state.funds.map((fund) => fund.id)).size, 6);
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
  assert.equal(state.totals.backing, 30);
  assert.equal(state.totals.spendable, 30);
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
