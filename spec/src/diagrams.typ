// Native state-machine diagrams (replacing the TikZ figures), drawn with the
// vendored fletcher package (offline; see spec/typst-packages). The head state
// machine is rendered from `head-fsm-transitions`, the single data source that
// check-refs.sh cross-checks against the Agda `_⟶⟨_⟩_` relation.

#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge
#import "/macros.typ": *

// Layout positions for the head state machine (x right, y down).
#let _fsm-pos = (
  "Open": (0, 0),
  "Closed": (2.6, 0),
  "Final": (5.2, 0),
  "FanoutProgress": (2.6, 1.8),
)

// Display label for each state (the spec's state symbols).
#let _fsm-disp = (
  "Open": $sans("open")$,
  "Closed": $sans("closed")$,
  "Final": $sans("final")$,
  "FanoutProgress": $sans("fanoutProgress")$,
)

// The authoritative head-protocol transitions. `from`/`rule`/`to` are checked
// against the Agda relation by check-refs.sh; `label`/`bend` are presentation.
#let head-fsm-transitions = (
  (from: "Open", rule: "increment", to: "Open", label: $sans("increment")$, bend: 130deg),
  (from: "Open", rule: "decrement", to: "Open", label: $sans("decrement")$, bend: -130deg),
  (from: "Open", rule: "close", to: "Closed", label: $sans("close")$, bend: 0deg),
  (from: "Closed", rule: "contest", to: "Closed", label: $sans("contest")$, bend: 130deg),
  (from: "Closed", rule: "fanout", to: "Final", label: $sans("fanout")$, bend: 25deg),
  (from: "Closed", rule: "partialFanoutStart", to: "FanoutProgress", label: [], bend: 0deg),
  (from: "FanoutProgress", rule: "partialFanoutStep", to: "FanoutProgress", label: $sans("partialFanout")$, bend: 130deg),
  (from: "FanoutProgress", rule: "finalPartialFanout", to: "Final", label: $sans("finalPartialFanout")$, bend: -20deg),
)

// Source/target state symbol of a transition rule, single-sourced from the
// Agda-checked `head-fsm-transitions`. Transaction diagrams label their head
// in/out boxes via these, so a tx diagram cannot depict a state the Agda
// relation `_⟶⟨_⟩_` disagrees with. (check-refs.sh validates the `tx-rule` map.)
#let _rule-from(rule) = _fsm-disp.at(head-fsm-transitions.find(x => x.rule == rule).from)
#let _rule-to(rule) = _fsm-disp.at(head-fsm-transitions.find(x => x.rule == rule).to)

// Which tx diagram realises which `_⟶⟨_⟩_` rule (init/deposit/recover are not
// head-state transitions and are absent here).
#let tx-rule = (
  incrementTx: "increment",
  decrementTx: "decrement",
  closeTx: "close",
  contestTx: "contest",
  fanoutTx: "fanout",
  partialFanoutTx: "partialFanoutStart",
  finalPartialFanoutTx: "finalPartialFanout",
)

// Datum field display per state (single source for the inline transition arrows;
// field set/order mirrors the Agda `HeadDatum` constructors). check-refs.sh
// verifies these keys equal the HeadDatum constructors.
#let state-fields = (
  "Open": ($cid$, $hydraKeys$, $nop$, $Tcontest$, $v$, $eta$, $adaO$),
  "Closed": ($cid$, $hydraKeys$, $nop$, $Tcontest$, $v$, $s$, $eta$, $contesters$, $tfinal$, $adaO$),
  "FanoutProgress": ($cid$, $hydraKeys$, $nop$, $tfinal$, $eta$, $adaO$),
  "Final": (),
)

#let _state-tuple(st) = {
  let fs = state-fields.at(st)
  if fs.len() == 0 { _fsm-disp.at(st) } else {
    $(#_fsm-disp.at(st), #fs.join($\,$))$
  }
}

// Render a transaction's inline state-transition arrow, derived from the same
// head-fsm-transitions data that check-refs.sh verifies against the Agda
// relation `_⟶⟨_⟩_`. So the arrow cannot drift from the formal state machine.
#let transition-arrow(rule) = {
  let t = head-fsm-transitions.find(x => x.rule == rule)
  assert(t != none, message: "unknown transition rule: " + rule)
  align(center, $#_state-tuple(t.from) stretch(-->)^(sans(#rule)) #_state-tuple(t.to)$)
}

#let head-fsm = {
  set text(size: 9pt)
  diagram(
    node-stroke: 0.6pt,
    node-corner-radius: 3pt,
    spacing: (8mm, 10mm),
    .._fsm-pos.keys().map(n => node(
      _fsm-pos.at(n),
      _fsm-disp.at(n),
      name: label("fsm-" + n),
      extrude: if n == "Final" { (0, -3pt) } else { (0pt,) },
    )),
    ..head-fsm-transitions.map(t => edge(
      label("fsm-" + t.from),
      label("fsm-" + t.to),
      t.label,
      "-|>",
      bend: t.bend,
      label-size: 8pt,
    )),
    // initial-state entry arrow into `open`
    edge((-0.9, 0), label("fsm-Open"), $sans("init")$, "-|>", label-size: 8pt),
  )
}

// The small deposit-protocol state machine (separate from the head FSM).
#let deposit-fsm = {
  set text(size: 9pt)
  diagram(
    node-stroke: 0.6pt,
    node-corner-radius: 3pt,
    spacing: (16mm, 10mm),
    node((0, 0), $sans("pending")$, name: <dep-pending>),
    node((1, 0), $sans("final")$, name: <dep-final>, extrude: (0pt, -3pt)),
    edge((-0.7, 0), <dep-pending>, $sans("deposit")$, "-|>", label-size: 8pt),
    edge(<dep-pending>, <dep-final>, $sans("increment")$, "-|>", bend: 25deg),
    edge(<dep-pending>, <dep-final>, $sans("recover")$, "-|>", bend: -25deg),
  )
}

// ===== Unified transaction diagrams =====
// A transaction is drawn as [inputs] → [tx box: name / mint / validity] →
// [outputs]. Every tx figure uses this one renderer, so colours and styling are
// unified; the data is the single source (and, for txs that correspond to a
// `_⟶⟨_⟩_` rule, cross-checkable against the Agda — see check-refs.sh).
#let _box(c) = box(stroke: 0.5pt, inset: 4pt, radius: 2pt, c)

#let tx-diagram(name, inputs, outputs, mint: none, validity: none) = {
  set text(size: 8pt)
  let h = calc.max(inputs.len(), outputs.len(), 1)
  let mid = (h - 1) / 2
  let txbody = {
    set align(center)
    strong(name)
    for m in (mint, validity) {
      if m != none { linebreak(); m }
    }
  }
  diagram(
    node-stroke: 0.5pt,
    node-corner-radius: 2pt,
    node-inset: 5pt,
    spacing: (18mm, 7mm),
    ..inputs.enumerate().map(((i, c)) => node((0, i), c, name: label("txin-" + str(i)))),
    node((2, mid), txbody, name: <txbox>, fill: luma(240)),
    ..outputs.enumerate().map(((i, c)) => node((4, i), c, name: label("txout-" + str(i)))),
    ..inputs.enumerate().map(((i, _)) => edge(label("txin-" + str(i)), <txbox>, "-|>")),
    ..outputs.enumerate().map(((i, _)) => edge(<txbox>, label("txout-" + str(i)), "-|>")),
  )
}

// Init transaction (spec §5.1): spends the seed input, mints ST + PTs, and
// produces the head output directly in the Open state.
#let initTx-diagram = tx-diagram(
  $mtxInit$,
  ($txOutRef_sans("seed")$,),
  ([$o_sans("head")$ in state $stOpen$ \ datum $= datumHead$ \ value $= st + sum pt_i$],),
  mint: $sans("mint") = {st, pt_1 ... pt_n} :: cid$,
)

// Deposit (§5.2): spends committed UTxOs into a νDeposit output.
#let depositTx-diagram = tx-diagram(
  $mtxDeposit$,
  ([$o_(sans("deposited"), 1)$], [$dots.v$], [$o_(sans("deposited"), m)$]),
  ([$o_sans("deposit")$ governed by $nuDeposit$ \ datum $= datumDeposit = (cid, t_sans("recover"), C)$ \ value $= valDeposit$],),
)

// Recover (§5.3): restores the deposited UTxOs after the deadline.
#let recoverTx-diagram = tx-diagram(
  $mtxRecover$,
  ([$o_sans("deposit")$ \ datum $= (cid, t_sans("recover"), C)$],),
  ([recovered UTxOs $C$],),
  validity: $sans("validity") = (t_sans("recover"), infinity)$,
)

// Increment (§5.4): folds a deposit into the open head.
#let incrementTx-diagram = tx-diagram(
  $mtxIncrement$,
  ([$o_sans("head")$ in #_rule-from("increment") \ datum $= datumHead$], [$o_sans("deposit")$ \ datum $= datumDeposit$]),
  ([$o_sans("head")'$ in #_rule-to("increment") \ datum $= datumHead'$ \ value $= valHead union valDeposit$],),
  mint: $sans("mint") = emptyset$,
)

// Decrement (§5.5): removes UTxOs from the open head.
#let decrementTx-diagram = tx-diagram(
  $mtxDecrement$,
  ([$o_sans("head")$ in #_rule-from("decrement") \ datum $= datumHead$],),
  ([$o_sans("head")'$ in #_rule-to("decrement") \ datum $= datumHead'$], [decommitted \ $o_1 dots.h o_k$]),
  mint: $sans("mint") = emptyset$,
)

// Close (§5.6): moves the open head to closed.
#let closeTx-diagram = tx-diagram(
  $mtxClose$,
  ([$o_sans("head")$ in #_rule-from("close") \ datum $= datumHead$],),
  ([$o_sans("head")'$ in #_rule-to("close") \ datum $= datumHead'$ \ (unified $eta'$, $contesters = emptyset$)],),
  mint: $sans("mint") = emptyset$,
  validity: $sans("validity") = (t_sans("min"), t_sans("max"))$,
)

// Contest (§5.7): re-closes with a newer snapshot.
#let contestTx-diagram = tx-diagram(
  $mtxContest$,
  ([$o_sans("head")$ in #_rule-from("contest") \ datum $= datumHead$],),
  ([$o_sans("head")'$ in #_rule-to("contest") \ datum $= datumHead'$ \ ($contesters' = contesters union {keyHash}$)],),
  mint: $sans("mint") = emptyset$,
  validity: $sans("validity") = (t_sans("min"), t_sans("max"))$,
)

// Fan-out (§5.8): distributes all UTxOs and burns the head tokens (→ final).
#let fanoutTx-diagram = tx-diagram(
  $mtxFanout$,
  ([$o_sans("head")$ in #_rule-from("fanout") \ unified $eta$],),
  ([distributed \ $o_1 dots.h o_m$ \ (head reaches #_rule-to("fanout"))],),
  mint: $sans("mint") = {st, pt_1 ... pt_n}^(-1) :: cid$,
  validity: $sans("validity") = (t_sans("final"), infinity)$,
)

// Partial fan-out (§5.8.1): distributes a batch, staying in FanoutProgress.
#let partialFanoutTx-diagram = tx-diagram(
  $mtxPartialFanout$,
  ([$o_sans("head")$ in #_rule-from("partialFanoutStart")],),
  ([distributed \ $o_1 dots.h o_m$], [$o_sans("head")'$ in #_rule-to("partialFanoutStart") \ updated $eta'$]),
  validity: $sans("validity") = (t_sans("final"), infinity)$,
)

// Final partial fan-out (§5.8.2): distributes the last batch and burns tokens.
#let finalPartialFanoutTx-diagram = tx-diagram(
  $mtxFinalPartialFanout$,
  ([$o_sans("head")$ in #_rule-from("finalPartialFanout")],),
  ([final batch \ $o_1 dots.h o_m$ \ (head reaches #_rule-to("finalPartialFanout"))],),
  mint: $sans("mint") = {st, pt_1 ... pt_n}^(-1) :: cid$,
  validity: $sans("validity") = (t_sans("final"), infinity)$,
)


// Illustrative plain UTxO graph (§3.3): transactions as boxes, edges as UTxOs,
// with two dangling (unspent) outputs.
#let utxo-graph = {
  set text(size: 9pt)
  diagram(
    node-stroke: 0.6pt,
    node-corner-radius: 2pt,
    spacing: (12mm, 7mm),
    node((0, 0.5), $sans("tx")_1$, name: <u1>),
    node((1, 0), $sans("tx")_2$, name: <u2>),
    node((1, 1), $sans("tx")_3$, name: <u3>),
    edge((-0.8, 0.5), <u1>, "-|>"),
    edge(<u1>, <u2>, "-|>"),
    edge(<u1>, <u3>, "-|>"),
    edge(<u2>, (1.9, 0), "-|>"),   // dangling UTxO
    edge(<u3>, (1.9, 1), "-|>"),   // dangling UTxO
  )
}
