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
// A transaction is drawn as [input UTxOs] → [tx box] → [output UTxOs] with ONE
// data-driven renderer that reproduces the original figures' visual language:
//   * UTxO boxes are rounded with a coloured title bar — blue for an INPUT
//     (spent) UTxO, green for an OUTPUT (produced) one — an optional datum body
//     (split with the spending redeemer when both are given) and a value footer.
//   * the transaction is a square-cornered box with a yellow title bar and a
//     stack of rows: redeemer | output-ref, validity, signer set κ, mint.
// Head UTxO boxes take their state fields from `state-fields` (the same
// Agda-checked source as the inline transition arrows), so a tx figure cannot
// depict head-state fields the `_⟶⟨_⟩_` relation disagrees with.

#let _hdr-in = rgb("#a9d3ec") // input UTxO title bar (blue)
#let _hdr-out = rgb("#9fdcab") // output UTxO title bar (green)
#let _hdr-tx = rgb("#f4dc82") // transaction title bar (yellow)
#let _cell = rgb("#fcfdff") // box body

// state KEY at each end of a rule (for state-fields / _fsm-disp lookup).
#let _from(rule) = head-fsm-transitions.find(x => x.rule == rule).from
#let _to(rule) = head-fsm-transitions.find(x => x.rule == rule).to
// The datum line of a state: the state symbol followed by its HeadDatum fields.
#let _state-line(st) = ((_fsm-disp.at(st),) + state-fields.at(st)).join([, ])

// One full-width band of a box (title bar with `sep: false`, else a body row).
#let _band(body, fill: _cell, sep: true) = block(
  width: 100%,
  fill: fill,
  inset: (x: 6pt, y: 3.5pt),
  stroke: if sep { (top: 0.5pt) } else { none },
)[#align(center, body)]

// A body row split into two cells (datum | redeemer, or redeemer | output-ref).
#let _split(a, b) = block(width: 100%, stroke: (top: 0.5pt), inset: 0pt, grid(
  columns: (1.3fr, 1fr),
  block(width: 100%, inset: (x: 5pt, y: 3.5pt))[#align(center, a)],
  block(width: 100%, inset: (x: 5pt, y: 3.5pt), stroke: (left: 0.5pt))[#align(center, b)],
))

// A UTxO box: rounded rect, coloured title bar, optional datum (split with the
// spending redeemer when both are given) and a value footer.
#let utxo-box(title, datum: none, redeemer: none, value: none, kind: "in", width: 22mm) = {
  let bar = if kind == "in" { _hdr-in } else if kind == "out" { _hdr-out } else { _cell }
  set text(size: 7.5pt)
  box(stroke: 0.6pt, radius: 3pt, clip: true, inset: 0pt, fill: _cell, width: width)[
    #set block(spacing: 0pt)
    #_band(strong(title), fill: bar, sep: false)
    #{
      if datum != none and redeemer != none { _split(datum, redeemer) } else if datum != none { _band(datum) } else if redeemer != none { _band(redeemer) }
    }
    #if value != none { _band(emph(value)) }
  ]
}

// A head UTxO box in `state` (KEY); datum = the state's `state-fields` line.
#let head-utxo(state, value: none, redeemer: none, kind: "in") = utxo-box(
  $nuHead$,
  datum: _state-line(state),
  redeemer: redeemer,
  value: value,
  kind: kind,
  width: 42mm,
)

// The transaction box: square corners, yellow title bar, then the given bands.
#let tx-box(name, ..bands) = {
  set text(size: 7.5pt)
  box(stroke: 0.7pt, clip: true, inset: 0pt, fill: _cell, width: 42mm)[
    #set block(spacing: 0pt)
    #_band(strong(name), fill: _hdr-tx, sep: false)
    #if bands.pos().len() == 0 {
      // a tx with no on-chain checks to show (e.g. deposit): a tall empty body,
      // like the original figures, so the box does not collapse to a thin bar.
      block(width: 100%, height: 12mm, stroke: (top: 0.5pt))[]
    } else { bands.pos().join() }
  ]
}

#let tx-diagram(name, inputs, outputs, redeemer: none, outref: none, validity: none, kappa: none, mint: none, qty: none) = {
  let h = calc.max(inputs.len(), outputs.len(), 1)
  let mid = (h - 1) / 2
  let bands = ()
  if redeemer != none { bands.push(if outref != none { _split(redeemer, outref) } else { _band(redeemer) }) }
  for r in (validity, kappa, mint) { if r != none { bands.push(_band(r)) } }
  diagram(
    node-stroke: none,
    node-inset: 0pt,
    spacing: (9mm, 5mm),
    ..inputs.enumerate().map(((i, c)) => node((0, i), c, name: label("txin-" + str(i)))),
    node((2, mid), tx-box(name, ..bands), name: <txbox>),
    ..outputs.enumerate().map(((i, c)) => node((4, i), c, name: label("txout-" + str(i)))),
    ..inputs.enumerate().map(((i, _)) => edge(label("txin-" + str(i)), <txbox>, "-|>")),
    ..outputs.enumerate().map(((i, _)) => {
      if i == 0 and qty != none { edge(<txbox>, label("txout-" + str(i)), "-|>", label: qty, label-side: right, label-size: 7pt) } else { edge(<txbox>, label("txout-" + str(i)), "-|>") }
    }),
  )
}

// Init (§5.1): spends the seed, mints ST + PTs, produces the Open head output.
#let initTx-diagram = tx-diagram(
  $mtxInit$,
  (utxo-box($o_sans("seed")$, kind: "in"),),
  (head-utxo("Open", value: $st + sum pt_i$, kind: "out"),),
  mint: $sans("mint") = {st, pt_1 ... pt_n} :: cid$,
  qty: $1$,
)

// Deposit (§5.2): spends committed UTxOs into a νDeposit output.
#let depositTx-diagram = tx-diagram(
  $mtxDeposit$,
  (utxo-box($o_(sans("dep"), 1)$, kind: "in"), utxo-box($o_(sans("dep"), m)$, kind: "in")),
  (utxo-box($nuDeposit$, datum: $cid, t_sans("rec"), C$, value: $valDeposit$, kind: "out", width: 34mm),),
)

// Recover (§5.3): restores the deposited UTxOs after the deadline.
#let recoverTx-diagram = tx-diagram(
  $mtxRecover$,
  (utxo-box($nuDeposit$, datum: $cid, t_sans("rec"), C$, redeemer: $sans("Recover") med m$, value: $valDeposit$, kind: "in", width: 42mm),),
  (utxo-box($sans("recovered") med C$, kind: "plain", width: 28mm),),
  validity: $sans("validity") = (t_sans("rec"), infinity)$,
)

// Increment (§5.4): folds a deposit into the open head.
#let incrementTx-diagram = tx-diagram(
  $mtxIncrement$,
  (
    head-utxo(_from("increment"), redeemer: $sans("Increment") med xi med sans("ref")$, value: $valHead$, kind: "in"),
    utxo-box($nuDeposit$, datum: $cid, t_sans("rec"), C$, redeemer: $sans("Claim")$, value: $valDeposit$, kind: "in", width: 42mm),
  ),
  (head-utxo(_to("increment"), value: $valHead union valDeposit$, kind: "out"),),
  validity: $t_sans("max")$,
  kappa: $kappa = {k_i^\#}$,
  mint: $sans("mint") = emptyset$,
  qty: $1$,
)

// Decrement (§5.5): removes UTxOs from the open head.
#let decrementTx-diagram = tx-diagram(
  $mtxDecrement$,
  (head-utxo(_from("decrement"), redeemer: $sans("decrement") med xi_sans("ms")$, value: $valHead$, kind: "in"),),
  (
    head-utxo(_to("decrement"), value: $valHead'$, kind: "out"),
    utxo-box($sans("decommitted")$, datum: $o_1 dots.h o_k$, kind: "plain", width: 30mm),
  ),
  validity: $sans("validity") = (t_sans("min"), t_sans("max"))$,
  kappa: $kappa = {k_i^\#}$,
  mint: $sans("mint") = emptyset$,
  qty: $1$,
)

// Close (§5.6): moves the open head to closed.
#let closeTx-diagram = tx-diagram(
  $mtxClose$,
  (head-utxo(_from("close"), value: $valHead$, kind: "in"),),
  (head-utxo(_to("close"), value: $valHead'$, kind: "out"),),
  redeemer: $sans("close") \ xi, eta^\#$,
  outref: $o_sans("head")$,
  validity: $sans("validity") = (t_sans("min"), t_sans("max"))$,
  kappa: $kappa = {k_i^\#}$,
  mint: $sans("mint") = emptyset$,
  qty: $1$,
)

// Contest (§5.7): re-closes with a newer snapshot.
#let contestTx-diagram = tx-diagram(
  $mtxContest$,
  (head-utxo(_from("contest"), value: $valHead$, kind: "in"),),
  (head-utxo(_to("contest"), value: $valHead'$, kind: "out"),),
  redeemer: $sans("contest") \ xi, eta^\#$,
  outref: $o_sans("head")$,
  validity: $sans("validity") = (t_sans("min"), t_sans("max"))$,
  kappa: $kappa = {k_i^\#}$,
  mint: $sans("mint") = emptyset$,
  qty: $1$,
)

// Fan-out (§5.8): distributes all UTxOs and burns the head tokens (→ final).
#let fanoutTx-diagram = tx-diagram(
  $mtxFanout$,
  (head-utxo(_from("fanout"), value: $valHead$, kind: "in"),),
  (utxo-box($o_1$, kind: "plain"), utxo-box($dots.v$, kind: "plain"), utxo-box($o_m$, kind: "plain")),
  redeemer: $sans("fanout") \ m, n, n'$,
  outref: $o_1 dots.h o_m$,
  validity: $sans("validity") = (t_sans("final"), infinity)$,
  kappa: $kappa = {k_i^\#}$,
  mint: $sans("mint") = {st, pt_1 ... pt_n}^(-1) :: cid$,
)

// Partial fan-out (§5.8.1): distributes a batch, staying in FanoutProgress.
#let partialFanoutTx-diagram = tx-diagram(
  $mtxPartialFanout$,
  (head-utxo(_from("partialFanoutStart"), value: $valHead$, kind: "in"),),
  (
    head-utxo(_to("partialFanoutStart"), value: $valHead'$, kind: "out"),
    utxo-box($o_1$, kind: "plain"),
    utxo-box($dots.v$, kind: "plain"),
    utxo-box($o_m$, kind: "plain"),
  ),
  redeemer: $sans("partialFanout") \ m, sans("crsRef")$,
  outref: $o_sans("head")$,
  validity: $sans("validity") = (t_sans("final"), infinity)$,
  kappa: $kappa = {k_i^\#}$,
  mint: $sans("mint") = emptyset$,
  qty: $1$,
)

// Final partial fan-out (§5.8.2): distributes the last batch and burns tokens.
#let finalPartialFanoutTx-diagram = tx-diagram(
  $mtxFinalPartialFanout$,
  (head-utxo(_from("finalPartialFanout"), value: $valHead$, kind: "in"),),
  (utxo-box($o_1$, kind: "plain"), utxo-box($dots.v$, kind: "plain"), utxo-box($o_m$, kind: "plain")),
  redeemer: $sans("finalPartialFanout") \ m, pi$,
  outref: $o_1 dots.h o_m$,
  validity: $sans("validity") = (t_sans("final"), infinity)$,
  kappa: $kappa = {k_i^\#}$,
  mint: $sans("mint") = {st, pt_1 ... pt_n}^(-1) :: cid$,
)


// Illustrative plain UTxO graph (§3.3): transactions as boxes, edges as UTxOs.
// Consumed (input) edges are red and the two dangling unspent outputs black, so
// the picture matches the Preliminaries prose ("(red) inputs ... (black) outputs
// ... two UTxOs in the figure").
#let utxo-graph = {
  set text(size: 9pt)
  diagram(
    node-stroke: 0.6pt,
    node-corner-radius: 2pt,
    spacing: (12mm, 7mm),
    node((0, 0.5), $sans("tx")_1$, name: <u1>),
    node((1, 0), $sans("tx")_2$, name: <u2>),
    node((1, 1), $sans("tx")_3$, name: <u3>),
    edge((-0.8, 0.5), <u1>, "-|>", stroke: red),   // input consumed by tx1
    edge(<u1>, <u2>, "-|>", stroke: red),           // input consumed by tx2
    edge(<u1>, <u3>, "-|>", stroke: red),           // input consumed by tx3
    edge(<u2>, (1.9, 0), "-|>"),   // dangling UTxO (unspent output)
    edge(<u3>, (1.9, 1), "-|>"),   // dangling UTxO (unspent output)
  )
}
