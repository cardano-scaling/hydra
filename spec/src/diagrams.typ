// Native state-machine diagrams (replacing the TikZ figures), drawn with the
// vendored fletcher package (offline; see spec/typst-packages). The head state
// machine is rendered from `head-fsm-transitions`, the single data source that
// check-refs.sh cross-checks against the Agda `_⟶⟨_⟩_` relation.

#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge
#import "@preview/cetz:0.3.4"
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
// `primed` lists the TARGET-tuple field indices whose values change in the
// step (rendered with a prime in the inline transition arrows, as in the
// per-transaction prose); `redeemer` is the redeemer payload rendered under
// the arrow. Both presentation-only (check-refs.sh checks from/rule/to).
#let head-fsm-transitions = (
  (from: "Open", rule: "increment", to: "Open", label: $sans("increment")$, bend: 130deg,
   primed: (4, 5), redeemer: $xi, s, txOutRef_(sans("increment")), delta^(\#)$),
  (from: "Open", rule: "decrement", to: "Open", label: $sans("decrement")$, bend: -130deg,
   primed: (4, 5), redeemer: $xi, s, m, kappa^(\#)$),
  (from: "Open", rule: "close", to: "Closed", label: $sans("close")$, bend: 0deg,
   primed: (4, 5, 6), redeemer: $sans("CloseType")$),
  (from: "Closed", rule: "contest", to: "Closed", label: $sans("contest")$, bend: 130deg,
   primed: (4, 5, 6, 7, 8), redeemer: $sans("ContestType")$),
  (from: "Closed", rule: "fanout", to: "Final", label: $sans("fanout")$, bend: 25deg,
   primed: (), redeemer: $m, pi, sans("crsRef")$),
  (from: "Closed", rule: "partialFanoutStart", to: "FanoutProgress", label: [], bend: 40deg,
   primed: (4,), redeemer: $m, sans("crsRef")$),
  (from: "FanoutProgress", rule: "partialFanoutStep", to: "FanoutProgress", label: $sans("partialFanout")$, bend: 130deg,
   primed: (4,), redeemer: $m, sans("crsRef")$),
  (from: "FanoutProgress", rule: "finalPartialFanout", to: "Final", label: $sans("finalPartialFanout")$, bend: -20deg,
   primed: (), redeemer: $m, pi, sans("crsRef")$),
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

#let _state-tuple(st, primes: ()) = {
  let fs = state-fields.at(st)
  if fs.len() == 0 { _fsm-disp.at(st) } else {
    let fs = fs.enumerate().map(((i, f)) => if i in primes { math.attach(f, tr: sym.prime) } else { f })
    $(#_fsm-disp.at(st), #fs.join($\,$))$
  }
}

// Render a transaction's inline state-transition arrow, derived from the same
// head-fsm-transitions data that check-refs.sh verifies against the Agda
// relation `_⟶⟨_⟩_`. So the arrow cannot drift from the formal state machine.
#let transition-arrow(rule) = {
  let t = head-fsm-transitions.find(x => x.rule == rule)
  assert(t != none, message: "unknown transition rule: " + rule)
  align(center, $#_state-tuple(t.from)
    stretch(-->)^(sans(#rule))_(#text(size: 0.8em, t.redeemer))
    #_state-tuple(t.to, primes: t.primed)$)
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

// Standard box widths — every box of a given kind uses the SAME width so figures line up
// consistently (do not override these per-diagram).
#let _w-utxo = 28mm // a plain UTxO box (o_seed, o_i, decommitted, recovered, …)
#let _w-script = 42mm // a script UTxO box carrying a datum (νHead, νDeposit)
#let _w-tx = 42mm // the transaction box

// state KEY at each end of a rule (for state-fields / _fsm-disp lookup).
#let _from(rule) = head-fsm-transitions.find(x => x.rule == rule).from
#let _to(rule) = head-fsm-transitions.find(x => x.rule == rule).to
// The rule's changed target fields (see head-fsm-transitions), primed in the
// produced head box exactly as in the inline transition arrows.
#let _primes(rule) = head-fsm-transitions.find(x => x.rule == rule).primed
// The datum line of a state: the state symbol followed by its HeadDatum fields
// (`primes` = field indices rendered with a prime, for produced states).
#let _state-line(st, primes: ()) = {
  let fs = state-fields.at(st).enumerate().map(((i, f)) => if i in primes { math.attach(f, tr: sym.prime) } else { f })
  ((_fsm-disp.at(st),) + fs).join([, ])
}

// One full-width band of a box (title bar with `sep: false`, else a body row).
#let _band(body, fill: _cell, sep: true) = block(
  width: 100%,
  fill: fill,
  inset: (x: 7pt, y: 5.5pt),
  stroke: if sep { (top: 0.5pt) } else { none },
)[#align(center, body)]

// A body row split into two cells (datum | redeemer, or redeemer | output-ref).
// The divider is a full-height grid line (not the right cell's border), so it spans the
// whole row even when the datum wraps to more lines than the redeemer; cells are centred.
#let _split(a, b) = block(width: 100%, stroke: (top: 0.5pt), inset: 0pt, grid(
  columns: (1.3fr, 1fr),
  inset: (x: 6pt, y: 5.5pt),
  align: center + horizon,
  grid.vline(x: 1, stroke: 0.5pt),
  a, b,
))

// A UTxO box: rounded rect, coloured title bar, optional datum (split with the
// spending redeemer when both are given) and a value footer.
#let utxo-box(title, datum: none, redeemer: none, value: none, kind: "in", width: _w-utxo) = {
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

// A script UTxO box (νHead / νDeposit): a UTxO governed by a validator, carrying a datum + value,
// drawn at the standard script-box width so all of them line up.
#let script-utxo(title, datum: none, value: none, redeemer: none, kind: "in") = utxo-box(
  title,
  datum: datum,
  value: value,
  redeemer: redeemer,
  kind: kind,
  width: _w-script,
)

// A "wallet" UTxO entry: a hollow circle pin followed by a concrete-value label, for the external
// (wallet) UTxOs the originals draw inside a dashed box (e.g. in₁: 15 ada). Use with `inGroup`.
#let wallet-utxo(body) = {
  set text(7.5pt)
  box(grid(columns: (auto, auto), column-gutter: 4pt, align: horizon, circle(radius: 2.2pt, stroke: 0.6pt, fill: white), body))
}

// A head UTxO box in `state` (KEY); datum = the state's `state-fields` line.
#let head-utxo(state, value: none, redeemer: none, kind: "in", primes: ()) = script-utxo(
  $nuHead$,
  datum: _state-line(state, primes: primes),
  value: value,
  redeemer: redeemer,
  kind: kind,
)

// The transaction box: square corners, yellow title bar, then the given bands.
#let tx-box(name, ..bands) = {
  set text(size: 7.5pt)
  box(stroke: 0.7pt, radius: 3pt, clip: true, inset: 0pt, fill: _cell, width: _w-tx)[
    #set block(spacing: 0pt)
    // the box label is the transaction name + "Tx" (e.g. depositTx), as in the original figures
    #_band(strong(name + $italic("Tx")$), fill: _hdr-tx, sep: false)
    #if bands.pos().len() == 0 {
      // a tx with no on-chain checks to show (e.g. deposit): a tall empty body,
      // like the original figures, so the box does not collapse to a thin bar.
      block(width: 100%, height: 12mm, stroke: (top: 0.5pt))[]
    } else { bands.pos().join() }
  ]
}

#let tx-diagram(name, inputs, outputs, redeemer: none, outref: none, validity: none, kappa: none, mint: none, qty: none, refArc: none, inGroup: false, outGroup: false) = {
  let h = calc.max(inputs.len(), outputs.len(), 1)
  let mid = (h - 1) / 2
  let bands = ()
  if redeemer != none { bands.push(if outref != none { _split(redeemer, outref) } else { _band(redeemer) }) }
  for r in (validity, kappa, mint) { if r != none { bands.push(_band(r)) } }
  diagram(
    node-stroke: none,
    node-inset: 0pt,
    spacing: (9mm, 5mm),
    // shape: "rect" on every box — fletcher otherwise auto-switches near-square boxes to a
    // circle, whose larger bounding shape leaves the edges disconnected from the box.
    ..inputs.enumerate().map(((i, c)) => node((0, i), c, name: label("txin-" + str(i)), shape: "rect")),
    // optional dashed "wallet" box enclosing all inputs (the external UTxOs, as in the originals)
    ..(if inGroup {
      (node(
        enclose: range(inputs.len()).map(i => label("txin-" + str(i))),
        stroke: (thickness: 0.6pt, dash: "dashed"), corner-radius: 5pt, inset: 8pt,
      ),)
    } else { () }),
    node((2, mid), tx-box(name, ..bands), name: <txbox>, shape: "rect"),
    ..outputs.enumerate().map(((i, c)) => node((4, i), c, name: label("txout-" + str(i)), shape: "rect")),
    // optional dashed "wallet" box enclosing all outputs (e.g. recover restores the
    // deposited UTxOs to the wallet, as in the originals); mirrors inGroup.
    ..(if outGroup {
      (node(
        enclose: range(outputs.len()).map(i => label("txout-" + str(i))),
        stroke: (thickness: 0.6pt, dash: "dashed"), corner-radius: 5pt, inset: 8pt,
      ),)
    } else { () }),
    // curved edges (hand-drawn look): inputs arrow INTO the tx (left), outputs end in a hollow
    // circle "pin" at the produced UTxO (right); the bend fans them out from/into the tx box.
    ..inputs.enumerate().map(((i, _)) => edge(label("txin-" + str(i)), <txbox>, "-|>", bend: (mid - i) * 14deg)),
    ..outputs.enumerate().map(((i, _)) => {
      let b = (i - mid) * 14deg
      // wallet outputs: aim at the node's WEST anchor so the circle pin lands at a fixed
      // left-centre point (left of the label) regardless of the curve's approach angle.
      let tgt = if outGroup { (name: "txout-" + str(i), anchor: "west") } else { label("txout-" + str(i)) }
      // qty label ("1") sits ABOVE the first output edge (toward that output), so with several
      // outputs it reads on the head-output arrow rather than drifting into the gap below it.
      if i == 0 and qty != none { edge(<txbox>, tgt, marks: (none, "o"), bend: b, label: qty, label-side: left, label-pos: 0.7, label-size: 7pt) } else { edge(<txbox>, tgt, marks: (none, "o"), bend: b) }
    }),
    // optional dashed reference arc from an input, up and OVER the tx box, to an output (the
    // committed-UTxO / datum reference the originals draw on deposit and recover). refArc = (in, out).
    ..(if refArc != none {
      (edge(
        label("txin-" + str(refArc.at(0))), label("txout-" + str(refArc.at(1))), "-|>",
        stroke: (thickness: 0.5pt, dash: "dashed"), bend: 18deg,
      ),)
    } else { () }),
  )
}

// Init (§5.1): spends the seed, mints ST + PTs, produces the Open head output.
// the seed input carries a dashed reference to its datum φ_seed (as in the original figure)
#let _seedRef = stack(
  dir: ttb,
  spacing: 2pt,
  align(center, text(7.5pt, $phi_sans("seed")$)),
  align(center, line(length: 5mm, angle: 90deg, stroke: (thickness: 0.5pt, dash: "dashed"))),
  utxo-box($o_sans("seed")$, kind: "in"),
)
#let initTx-diagram = tx-diagram(
  $mtxInit$,
  (_seedRef,),
  (head-utxo("Open", value: ${st, pt_1, dots.h, pt_n}$, kind: "out"),),
  redeemer: $rho_sans("seed")$,
  outref: $o_sans("head") quad 1$,
  mint: $sans("mint") = {st, pt_1 ... pt_n} :: cid$,
)

// Deposit (§5.2): spends committed UTxOs into a νDeposit output.
#let depositTx-diagram = tx-diagram(
  $mtxDeposit$,
  (wallet-utxo([$italic("in")_1$: 15 ada]), wallet-utxo([$italic("in")_2$: 7 ada])),
  (script-utxo($nuDeposit$, datum: $cid, t_sans("rec"), C$, value: [22 ada], kind: "out"),),
  inGroup: true,
  refArc: (0, 0),
)

// Recover (§5.3): restores the deposited UTxOs after the deadline.
#let recoverTx-diagram = tx-diagram(
  $mtxRecover$,
  (script-utxo($nuDeposit$, datum: $cid, t_sans("rec"), C$, redeemer: $sans("Recover") med m$, value: $valDeposit$, kind: "in"),),
  (box(inset: (left: 6pt), text(7.5pt)[$italic("out")_1$: 15 ada]), box(inset: (left: 6pt), text(7.5pt)[$italic("out")_2$: 7 ada])),
  validity: $sans("validity") = (t_sans("rec"), infinity)$,
  outGroup: true,
  refArc: (0, 0),
)

// Increment (§5.4): folds a deposit into the open head.
#let incrementTx-diagram = tx-diagram(
  $mtxIncrement$,
  (
    head-utxo(_from("increment"), redeemer: $sans("Increment") med xi, s, sans("ref"), delta^\#$, value: ${st, pt_sans("alice"), dots.h}$, kind: "in"),
    script-utxo($nuDeposit$, datum: $cid, t_sans("rec"), C$, redeemer: $sans("Claim")$, value: [22 ada], kind: "in"),
  ),
  (head-utxo(_to("increment"), value: [${st, pt_sans("alice"), dots.h}$ + 22 ada], kind: "out", primes: _primes("increment")),),
  validity: $t_sans("max")$,
  kappa: $kappa = {k_i^\#}$,
  mint: $sans("mint") = emptyset$,
  qty: $1$,
)

// Decrement (§5.5): removes UTxOs from the open head.
#let decrementTx-diagram = tx-diagram(
  $mtxDecrement$,
  (head-utxo(_from("decrement"), value: $valHead$, kind: "in"),),
  (
    head-utxo(_to("decrement"), value: $valHead'$, kind: "out", primes: _primes("decrement")),
    utxo-box($U_omega$, datum: $o_1 dots.h o_k$, kind: "plain"),
  ),
  redeemer: $sans("decrement") \ xi, s, m, kappa^\#$,
  outref: $o_sans("head")$,
  validity: $sans("validity") = (t_sans("min"), t_sans("max"))$,
  kappa: $kappa = {k_i^\#}$,
  mint: $sans("mint") = emptyset$,
  qty: $1$,
)

// Close (§5.6): moves the open head to closed.
#let closeTx-diagram = tx-diagram(
  $mtxClose$,
  (head-utxo(_from("close"), value: $valHead$, kind: "in"),),
  (head-utxo(_to("close"), value: $valHead'$, kind: "out", primes: _primes("close")),),
  redeemer: $sans("close") \ xi, (eta')^\#, delta^\#, kappa^\#$,
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
  (head-utxo(_to("contest"), value: $valHead'$, kind: "out", primes: _primes("contest")),),
  redeemer: $sans("contest") \ xi, (eta')^\#, delta^\#, kappa^\#$,
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
  redeemer: $sans("fanout") \ m, pi, sans("crsRef")$,
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
    head-utxo(_to("partialFanoutStart"), value: $valHead'$, kind: "out", primes: _primes("partialFanoutStart")),
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
  redeemer: $sans("finalPartialFanout") \ m, pi, sans("crsRef")$,
  outref: $o_1 dots.h o_m$,
  validity: $sans("validity") = (t_sans("final"), infinity)$,
  kappa: $kappa = {k_i^\#}$,
  mint: $sans("mint") = {st, pt_1 ... pt_n}^(-1) :: cid$,
)


// EUTxO worked example (§3.3), a faithful reproduction of the original figure: four transactions
// (tall GRAY boxes) whose INPUTS enter on the LEFT and OUTPUTS leave on the RIGHT; every UTxO is a
// BLACK output edge to a hollow circle labelled (index, value); a SPENT output is consumed by a RED
// input edge labelled ρᵢ and its circle is drawn RED; the two UNSPENT UTxOs dangle at a BLACK-outline
// hollow circle. Matches the Preliminaries prose "(red) inputs ... (black) outputs ... two UTxOs".
// Drawn with the vendored cetz package for precise geometry (the FSM / tx-flow figures use fletcher).
#let utxo-graph = {
  set text(size: 7.5pt)
  cetz.canvas(length: 1cm, {
    import cetz.draw: *
    let R = (paint: rgb(85.9%, 21.6%, 19.6%), thickness: 0.6pt) // the original's red
    let K = (paint: black, thickness: 0.6pt)
    let G = (paint: rgb(60%, 60%, 60%), thickness: 0.6pt) // the original's gray boxes
    // transactions (tall gray rectangles); every edge meets a box on the flat LEFT or RIGHT SIDE
    // (never a corner). Box half-width 0.2, half-height 0.62.
    let tx(x, y) = rect((x - 0.2, y - 0.62), (x + 0.2, y + 0.62), stroke: G)
    tx(2.0, 4.4)
    tx(4.4, 3.3)
    tx(6.6, 2.9)
    tx(2.9, 1.3)
    // pins ON the box sides (left x = centre − 0.2 = inputs; right x = centre + 0.2 = outputs), each
    // inset from the corners so a line only ever meets a flat side
    let a-i6 = (1.8, 4.72)
    let a-i7 = (1.8, 4.08)
    let a-o51 = (2.2, 4.72)
    let a-o12 = (2.2, 4.12)
    let b-i2 = (4.2, 3.6)
    let b-i3 = (4.2, 3.0)
    let b-o36 = (4.6, 3.3)
    let c-i1 = (6.4, 3.25)
    let c-i5 = (6.4, 2.9)
    let c-i4 = (6.4, 2.55)
    let c-o147 = (6.8, 2.9)
    let d-i8 = (2.7, 1.3)
    let d-o23 = (3.1, 1.65)
    let d-o64 = (3.1, 1.3)
    let d-o95 = (3.1, 0.95)
    // UTxO circle positions (drawn LAST so their fill masks the line stub at the centre → clean borders)
    let i6 = (0.4, 5.0)
    let i7 = (0.4, 3.9)
    let i8 = (0.4, 1.3)
    let o12 = (3.0, 4.15)
    let o51 = (5.55, 4.7)
    let o23 = (3.7, 2.55)
    let o36 = (5.5, 3.05)
    let o64 = (4.55, 1.6)
    let u95 = (3.85, 0.9)
    let u147 = (7.7, 2.9)
    // external inputs (red) → LEFT side
    bezier(i6, a-i6, (1.2, 5.05), stroke: R)
    bezier(i7, a-i7, (1.2, 3.75), stroke: R)
    line(i8, d-i8, stroke: R)
    // tx A outputs (black) from RIGHT side; consumed into B / C (red) at LEFT side
    line(a-o12, o12, stroke: K)
    bezier(a-o51, o51, (3.8, 5.2), stroke: K)
    line(o12, b-i2, stroke: R)
    bezier(o51, c-i1, (6.0, 4.1), stroke: R)
    // tx D outputs (black) from RIGHT side; consumed into B / C (red) at LEFT; (9,v₅) unspent
    line(d-o23, o23, stroke: K)
    line(d-o64, o64, stroke: K)
    line(d-o95, u95, stroke: K)
    line(o23, b-i3, stroke: R)
    bezier(o64, c-i4, (5.6, 1.7), stroke: R)
    // tx B output (black) from RIGHT side; consumed into C (red) at LEFT side
    line(b-o36, o36, stroke: K)
    line(o36, c-i5, stroke: R)
    // tx C output (black) from RIGHT side → unspent
    line(c-o147, u147, stroke: K)
    // UTxO circles, hollow: RED = spent output / external input, BLACK = the two unspent UTxOs
    let rc(p) = circle(p, radius: 0.09, fill: white, stroke: R)
    let bc(p) = circle(p, radius: 0.09, fill: white, stroke: K)
    rc(i6); rc(i7); rc(i8); rc(o12); rc(o51); rc(o23); rc(o36); rc(o64)
    bc(u95); bc(u147)
    // labels (placed beside the edges, never crossing them)
    let lbl(x, y, b) = content((x, y), b)
    lbl(1.0, 5.2, $rho_6$); lbl(1.0, 3.6, $rho_7$); lbl(1.5, 1.45, $rho_8$)
    lbl(2.55, 3.9, $(1, v_2)$); lbl(3.5, 3.7, $rho_2$)
    lbl(4.0, 5.25, $(5, v_1)$); lbl(6.25, 4.0, $rho_1$)
    lbl(3.0, 2.65, $(2, v_3)$); lbl(3.95, 2.4, $rho_3$)
    lbl(5.0, 3.35, $(3, v_6)$); lbl(5.9, 2.72, $rho_5$)
    lbl(3.7, 1.2, $(6, v_4)$); lbl(5.3, 1.5, $rho_4$)
    lbl(3.45, 0.55, $(9, v_5)$); lbl(7.25, 3.1, $(14, v_7)$)
  })
}
