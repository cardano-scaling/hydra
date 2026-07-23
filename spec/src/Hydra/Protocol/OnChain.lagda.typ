
#import "/template.typ": *
#import "/macros.typ": *
#import "/diagrams.typ": transition-arrow, initTx-diagram, depositTx-diagram, recoverTx-diagram, incrementTx-diagram, decrementTx-diagram, closeTx-diagram, contestTx-diagram, fanoutTx-diagram, partialFanoutTx-diagram, finalPartialFanoutTx-diagram

#pagebreak()
= On-chain Protocol <sec:on-chain>

The following sections describe the _on-chain_ protocol
controlling the life-cycle of a Hydra head, which can be intuitively described
as a state machine (see @fig:head-protocol-states). Each transition
in this state machine is represented and caused by a corresponding Hydra
protocol transaction
on-chain: $mtxInit$~@sec:init-tx, $mtxIncrement$~@sec:increment-tx, $mtxDecrement$~@sec:decrement-tx, $mtxClose$~@sec:close-tx, $mtxContest$~@sec:contest-tx, $mtxFanout$~@sec:fanout-tx, $mtxPartialFanout$~@sec:partial-fanout-tx, and $mtxFinalPartialFanout$~@sec:final-partial-fanout-tx.

The protocol uses KZG accumulators (see @sec:accumulators) to enable partial fanout when UTxO sets exceed transaction size limits. When all UTxOs fit in a single transaction, $mtxFanout$ distributes them all at once. When UTxO sets are too large, $mtxPartialFanout$ distributes subsets across multiple transactions using membership witnesses, transitioning through an intermediate $stFanoutProgress$ state, until $mtxFinalPartialFanout$ completes the distribution.

Besides the main state transitions of the head protocol, there is
the related "deposit protocol" with two transactions in support of
$mtxIncrement$: $mtxDeposit$~@sec:deposit-tx and $mtxRecover$~@sec:recover-tx.
There is also a $mtxDecrement$ transaction~@sec:decrement-tx that allows for taking funds from the Head back to L1.

The head protocol defines one minting policy script and one
validator script:
- $muHead$ governs minting of state and participation tokens in
  $mtxInit$ and burning of these tokens in $mtxFanout$ or $mtxFinalPartialFanout$.
- $nuHead$ represents the main protocol state machine logic and ensures
  contract continuity throughout $mtxIncrement$, $mtxDecrement$,
  $mtxClose$, $mtxContest$, $mtxFanout$, $mtxPartialFanout$ and $mtxFinalPartialFanout$.

The deposit protocol defines one validator script:
- $nuDeposit$ controls that $mtxDeposit$ transaction output is
  claimed correctly into a head via $mtxIncrement$ or recovered after
  the deadline has passed in a $mtxRecover$ transaction.

The head output datum $datumHead$ ranges over the protocol states. The state
machine and its per-state fields (as enumerated in the transitions below) are
captured by an Agda type, with the redeemer $redeemerHead$ selecting
the $nuHead$ transition.


The admissible $nuHead$ state transitions are captured as a typed relation
$d ⟶⟨ r ⟩ d'$ ("datum $d$ steps to $d'$ under redeemer $r$"). The relation
encodes the *state-machine shape* and the *version discipline* in the types:
$sans("increment")$/$sans("decrement")$ bump the version (`suc v`),
$sans("close")$/$sans("contest")$ preserve it (the same `v` reappears),
$sans("close")$ initialises the contester list to the empty list, $sans("contest")$
requires the new $keyHash in.not contesters$ (so the list grows by exactly one),
the partial-fanout rules thread the intermediate $stFanoutProgress$ state through to
$stFinal$, and every rule reuses the same `ada` binder on both sides, so the ADA
overhead $adaO$ of @sec:increment-tx–@sec:contest-tx is preserved by construction (there is no separate
$adaO$ conjunct in the bundles below). A rule violating any of *these* would fail to type-check. The
remaining per-transaction conditions (signatures, value conservation, deadlines)
are separate predicates (e.g. `closeDeadlineOK`/`contestDeadlineOK`) applied
alongside it.


Beyond the state-machine shape, individual $nuHead$ *checks* are stated as
predicates over the validation $sans("Context")$ and the datums. For example,
the close transaction (@sec:close-tx) requires the recorded contestation
deadline to be the transaction's upper validity bound extended by the
contestation period. This condition is stated as a checkable proposition over
the context and the produced datum.

The shared trust surface - the accumulator scheme and its specifying laws, the
value projections, the signature check - and the predicates common to several
transactions are stated once here; each transaction's own conditions are then
given in its section below.






#dparagraph[Scope of the validity bundles.] The per-transaction conditions of the
following sections are _validity bundles_: records conjoining the state-machine
step with the checks expressible from the datums, redeemer and context, inhabited
exactly for genuinely valid transactions. What the bundles type-enforce is the
state-machine shape, the version discipline, contester growth and deduplication,
the deadline equations, close-initialises-to-$emptyset$, the head value in/out
(derived: `headValue`/`headValueIn` sum the value at `ownHash` over the produced
outputs / resolved inputs), the increment deposit value (derived: `depositsValue`
sums the value at the $nuDeposit$ script `depHash` over _all_ spent inputs, as
Plutus `totalNonHeadInputValue`), the decrement decommit value (derived:
`decommitValue` sums the `m` outputs after the head output, as Plutus
`take m (tail outputs)`), and the participant signature: close, increment and
decrement carry the structural `signedByParticipant cid ctx` (an existence witness
`∃ kh` naming both a transaction signer and a participation token of the head
value), while contest carries the sharper `contesterSigned`/`contesterIsParticipant`
pair about the appended contester, from which `signedByParticipant` is derived
(`contest-participantSigned`); fanout and partial fanout have no such field. What
remains abstracted: the value arithmetic laws (`_+ᵛ_`/`_≤ᵛ_`/`εᵛ`) and the
per-asset projection `quantityOf` on the opaque `Value`, crypto
(`msVfy`/`snapshotSigOK`) and the accumulator operations
(`accVerify`/`accVerifyExclude`/`accUTxO`), all via postulated laws, plus the
context lookups the `Context` model does not expose: the recomputed increment
commit-set hash (`depositCommitsHashOf`, from the claimed deposit's datum) and the
CRS reference-input datum hash with its canonical constant
(`crsDatumHashAt`/`canonicalCRS#`). So value conservation is stated over the real
head, increment-deposit and decrement-decommit values (modulo the abstract value
algebra), while signature and accumulator soundness are assumed. None of this
on-chain layer is rendered in @agda-appendix: the datum/redeemer types, the
transition relation, the postulated trust base, the helper predicates and the
bundles themselves are all typechecked as part of this document's build but not
shown — the appendix is reserved for the machine-checked theorem statements
(@sec:onchain-theorems, @sec:offchain-theorems, @sec:security-theorems), and the
trust base is inventoried in prose in @sec:assumption-inventory.

== Init transaction <sec:init-tx>

The $mtxInit$ transaction creates a head instance and establishes the initial
state of the protocol and is shown in @fig:initTx. The head
instance is represented by the unique currency identifier $cid$ created by
minting tokens using the $muHead$ minting policy script which is parameterized
by a single output reference parameter $seed in tyOutRef$:
$ cid = hash(muHead(seed)) $

Two kinds of tokens are minted:
- A single _State Thread (ST)_ token marking the head output. This
  output contains the state of the protocol on-chain and the token ensures
  contract continuity. The token name is the well known string
  `HydraHeadV2`, i.e.
  $st = {cid |-> #raw("HydraHeadV2") |-> 1}$.
- One _Participation Token (PT)_ per participant
  $i in {1 dots.h |hydraKeys|}$ to be used for authenticating further
  transactions. The token name is the participant's verification key hash
  $keyHash_i = hash(msVK_i)$ of the verification key as received
  during protocol setup, i.e.
  $pt_i = {cid |-> keyHash_i |-> 1}$.

All minted tokens ($st$ and all $pt_i$) are placed directly into
the head output, which is created in the $stOpen$ state with an empty UTxO set.
Consequently, the $mtxInit$ transaction
- has at least input $seed$,
- mints the state thread token $st$, and one $pt$ for each of the $|hydraKeys|$
  participants with policy $cid$,
- has one head output
  $o_(sans("head"))$, which captures
  the open state of the protocol in the datum, i.e. the `Open` constructor of
  `HeadDatum` (defined above) instantiated with
  - $stOpen$ is the state identifier,
  - $cid'$ is the unique currency id of this instance,
  - $hydraKeys$ are the off-chain multi-signature keys from the setup
    phase,
  - $nop$ is the number of participants,
  - $Tcontest$ is the contestation period,
  - $v = 0$ is the initial snapshot version,
  - $eta = accUTxO(emptyset)$ is the accumulator commitment to the (empty) initial
    UTxO set (its hash $eta^(\#) = hash(eta)$ is what later snapshot signatures attest to).

#block(inset: (left: 1em), {
  emph[Implementation note (datum representation).]
  [ The Agda `Open` datum carries the accumulator _commitment_ $eta$, whereas the on-chain
  implementation stores only its _hash_ $eta^(\#)$ in the open state (the
  `OpenDatum.accumulatorHash` field); the full commitment is materialised only in the
  $stClosed$ / $stFanoutProgress$ states, where fan-out membership proofs require it. The
  two coincide for every open-state check, since those reference $eta$ only through
  $hash(eta)$ (e.g. the close / increment / decrement signature over
  $cid || v || s || eta^(\#) || delta^(\#) || kappa^(\#)$). Similarly, the datum's `hydraKey` field is the single
  _aggregate_ key of the multisignature scheme rather than the per-party key list
  $hydraKeys$; the checks written $msVfy(hydraKeys, dots.h)$ in @sec:increment-tx–@sec:contest-tx are verified
  under this aggregate key (Agda `hk`).]
})

The $muHead(seed)$ minting policy is the only script that verifies
init transactions and can be redeemed with either a $sans("Mint")$ or
$sans("Burn")$ redeemer:
- When evaluated with the $sans("Mint")$ redeemer,
  + The seed output is spent in this transaction. This guarantees uniqueness of the policy $cid$ because the EUTxO ledger ensures that $seed$ cannot be spent twice in the same chain.
    $(seed, dot.c) in txInputs$
  + All minted tokens of this policy are of single quantity $forall {cid |-> dot.c |-> q} in txMint : q = 1$. (The policy counts only its own currency and enforces unit quantities indirectly, via the head-output checks below; $txMint$ entries of _other_ policies are not constrained by $muHead$ - they validate themselves.)
  + Right number of tokens are minted $|txMint| = |hydraKeys| + 1$
  + State token is sent to the head validator $st in valHead$
  + All participation tokens are sent to the head output alongside the state token $forall i in [1 dots.h |hydraKeys|] : pt_i in valHead$
  + The $datum_(sans("head"))$ contains own currency id $cid = cid'$ and the right seed reference $seed = seed'$
- When evaluated with the $sans("Burn")$ redeemer,
  + All tokens for this policy in $txMint$ need to be of negative quantity
    $forall {cid |-> dot.c |-> q} in txMint : q < 0$.
    (Formalised as the `BurnValid` bundle below; _which_ burns are legitimate is
    $nuHead$'s concern, via the fan-out family's $n + 1$ burn count.)

*Important:* The $muHead$ minting policy only ensures
uniqueness of $cid$ and that the right amount of tokens have been minted and
sent to $nuHead$, while $nuHead$ in turn ensures continuity of the contract.
However, it is *crucial* that all head members check:
- That the transaction mints exactly the correct tokens: one $st$ token and one $pt$ for each head member (total $|hydraKeys| + 1$ tokens). This distinguishes $mtxInit$ from $mtxIncrement$ and $mtxDecrement$ transactions, which only move tokens without minting.
- That the head output contains an $st$ token of policy $cid$ which satisfies $cid = hash(muHead(seed))$. The $seed$ spent by this transaction can be used to determine this.
- That the correct verification key hashes are used in the $pt$s and the open state is consistent with parameters agreed during setup.
See the initialTx behavior in @fig:off-chain-prot for details about these checks.
The decidable core of these checks is formalised as the `initValid` predicate (a _creation_
predicate - init has no predecessor datum, so it is not a `_⟶⟨_⟩_` step): $cid = hash(muHead(seed))$,
the seed is spent (a structural conjunct, `seedSpent`), exactly $nop + 1$ tokens of $cid$ are minted,
and the produced Open is initial ($v = 0$, $eta = accUTxO(emptyset)$). Token placement into the head
value is also modelled (the state token is present and the head output carries exactly the $nop + 1$
head-policy tokens). What remains hand-reviewed: the datum bindings - $cid = hash(muHead(seed))$ is
stated over the law-free `hash`, and the datum's seed-reference field $seed = seed'$ has no Agda
counterpart (the `Open` datum carries no seed field).

The conditions are conjoined in the `InitValid` bundle with its dispatching `initValid` predicate (typechecked, not rendered).


The $sans("Burn")$ arm's single check is the `BurnValid` bundle: every
head-policy entry of the mint field is a burn - no head-policy token is minted
and at least one is burned (the policy only runs when its currency appears in
the mint field, and rejects a mint field without head-policy entries). It is
bridged (`burnValid→ref`) and differentially tested (the
`HeadValidatorAgreement` burn agreement, calling the real
`validateTokensBurning`).


#figure(initTx-diagram, caption: [$mtxInit$ transaction spending a seed UTxO and producing the head output directly in state $stOpen$.]) <fig:initTx>

== Deposit Transaction <sec:deposit-tx>

The $mtxDeposit$ transaction locks funds in $nuDeposit$ for later
collection into the head via an $mtxIncrement$ transaction. Any transaction
paying to $nuDeposit$ is a $mtxDeposit$ transaction as there is no on-chain
verification in $mtxDeposit$ transactions. Consequently, protocol actors
*must ensure off-chain* that a valid datum is used when paying to the
$nuDeposit$ validator. This is sufficient because a deposit is inert until
claimed: on-chain validation at creation time is not even expressible
(validators run on spend, not on receive), no head funds are exposed by a
deposit's existence, and collection requires an $mtxIncrement$ transaction
carrying the $n$-of-$n$ multisignature (@sec:increment-tx) — whose message
binds the claimed deposit's exact commit set, recomputed on-chain from its
datum. Every honest node refuses to treat a deposit as claimable unless its
datum decodes and its declared commits total exactly the value locked — a
mismatch would otherwise make the head's accumulator insolvent and the head
unfanoutable — so a single honest party blocks any malformed deposit: the
same unanimity gate that authenticates all other head content. A deposit
whose datum does not decode is unspendable by both the claim and recover
arms (@sec:recover-tx); only the depositor's own funds are at risk from a
malformed datum.

A valid deposit output is governed by $nuDeposit$ with value $valDeposit$ and datum
$ datumDeposit = (cid, t_(sans("recover")), C) $
where
- $cid$ is the currency id of the target head protocol instance (see~@sec:init-tx),
- $t_(sans("recover"))$ is a deadline after which the deposit can be recovered, and
- $C in (txInputs times tyBytes)^(*)$ is a list of transaction output
  references with along with serialized outputs that should become available in
  the head.

In Agda the deposit datum and the $nuDeposit$ redeemer are the `DepositDatum` /
`DepositRedeemer` types; the deposit transaction itself has no
on-chain verification, so there is no corresponding validity bundle.


Head protocol participants determine *off-chain* whether a
deposit output $o_(sans("deposit"))$ is eligible for their head by checking
+ $cid$ matches their head identifier,
+ $t_(sans("recover"))$ is reasonably far in the future, and#todo[explain; relate to contestation period?]
+ $valDeposit$ contains the value of all decoded outputs of $C$ from $datumDeposit$.

An example transaction which records all its spent inputs in a deposit output is
shown in @fig:depositTx. The $j in {1 dots.h m}$ inputs of this example with reference $txOutRef_(sans("deposited")_j)$ each spend output $o_(sans("deposited")_j)$ with $val_(sans("deposited")_j)$ would be recorded in the output datum as
$ C = forall j in {1 dots.h m} : [(txOutRef_(sans("deposited")_j), bytes(o_(sans("deposited")_j)))] $
and the value check would need to satisfy
$ valDeposit supset.eq union.big_(j=1)^(m) val_(sans("deposited")_j) $

#figure(depositTx-diagram, caption: [$mtxDeposit$ transaction spending multiple UTxO into a deposit output.]) <fig:depositTx>

== Recover Transaction <sec:recover-tx>

If a $mtxDeposit$ transaction output (see~@sec:deposit-tx) was
not collected into a head by an $mtxIncrement$
transaction~@sec:increment-tx, the $mtxRecover$ transaction
(@fig:recoverTx) allows for restoring the UTxO as recorded in the
deposit after the deadline has passed. It consists of
- one input spending from $nuDeposit$ with datum $datumDeposit = (cid, t_(sans("recover")), C)$, and
- outputs $o_1 dots.h o_m$ to recover UTxOs.

The script validator $nuDeposit$ is spent with redeemer
$redeemerDeposit = (sans("Recover"), m)$, where $m$ is the number of outputs
to recover, and checks:
+ All UTxOs are recovered exactly as they were deposited. This is done by
  comparing hashes of serialised representations of the $m$ recovering outputs
  $o_1 dots.h o_m$ with the canonically combined deposited UTxOs in $C$:
  $ hash(plus.o.big_(j=1)^(m) bytes(o_j)) = hash(sans("concat")(sortOn(1, C)^(arrow.b 2))) $
+ Transaction is posted after the deadline
  $ txValidityMin > t_(sans("recover")) $

The deposit datum and redeemer are formalised as `DepositDatum` / `DepositRedeemer`, and the recover
checks as the `recoverValid` predicate: the recovered outputs match the deposited ones
(`recoveredMatchesDeposited`, the @sec:recover-tx serialisation-hash equality, abstracted) and the transaction
is posted strictly after the deadline (`t_recover < txValidityMin`, concrete). A deposit's collection
into the head (Claim) is authorised by the `incrementValid` predicate's `depositSpentOK` check (@sec:increment-tx);
`depositClaimedBy` records that the deposit's `cid` must match the head it is claimed into.

The recover conditions form the `RecoverValid` bundle (typechecked, not rendered).


The Claim arm's own checks - the head binding and the before-deadline check, the
two checks $nuDeposit$ performs that $nuHead$ does not - form the `ClaimValid`
bundle, consumed by the joint claim obligation stated in @sec:increment-tx.


#figure(recoverTx-diagram, caption: [$mtxRecover$ transaction restoring UTxO of a deposit output.]) <fig:recoverTx>

== Increment Transaction <sec:increment-tx>

The $mtxIncrement$ transaction (@fig:incrementTx) allows
a participant to add a $mtxDeposit$ output~@sec:deposit-tx to an already
open head using a snapshot that approves inclusion. Consequently this
transaction consists of:

- one input spending from $nuHead$ with value $valHead$ holding the
  $st$ and datum $datumHead$,
- one input $txOutRef_(sans("deposit"))$ spending from $nuDeposit$ with value $valDeposit$ and datum
  $datumDeposit = (cid_(sans("deposit")), t_(sans("recover")), C)$,
- one output paying to $nuHead$ with value $valHead'$ and datum
  $datumHead'$.

The deposit validator $nuDeposit$ is spent with
$redeemerDeposit = sans("Claim")$ and ensures:
+ Claiming head id matches the deposit datum
  $ cid = cid_(sans("deposit")) $
+ Transaction is posted before the deadline
  $ txValidityMax <= t_(sans("recover")) $

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("increment"), xi, s, txOutRef_(sans("increment")), delta^(\#))$,
where $xi$ is a multi-signature of the increment snapshot which authorizes
addition of deposited UTxO, $s$ is the snapshot number,
$txOutRef_(sans("deposit"))$ points to the claimed deposit and $delta^(\#)$ is
the hash of the snapshot's decommit output set. The validator
checks:
+ State is advanced from $datumHead tilde stOpen$ to
  $datumHead' tilde stOpen$, parameters $cid, hydraKeys, Tcontest$
  stay unchanged and the new state is governed again by $nuHead$:
  #transition-arrow("increment")
  (the `increment` rule of `_⟶⟨_⟩_`; the version is bumped, $v |-> v + 1$).
+ New version $v'$ is incremented correctly
  $ v' = v + 1 $
+ Claimed deposit is spent
  $ txOutRef_(sans("increment")) = txOutRef_(sans("deposit")) $
+ $xi$ is a valid multi-signature of the new head state $eta'$
  $ msVfy(hydraKeys, (cid || v || s || (eta')^(\#) || delta^(\#) || kappa^(\#)), xi) = mtrue $
  where $(eta')^(\#) = hash(eta')$ is the hash of the new accumulator commitment $eta'$
  stored in the output datum, reflecting the UTxO set after adding the deposited UTxOs;
  $delta^(\#)$ is taken from the redeemer; and $kappa^(\#)$ is _recomputed on-chain_ as the
  hash of the commit list $C$ decoded from the claimed deposit's own datum
  $datumDeposit = (cid_(sans("deposit")), t_(sans("recover")), C)$ (a deposit datum
  that fails to decode rejects the transaction, error `DepositDatumInvalid`).
  Binding $kappa^(\#)$ to the _claimed_ deposit's exact commit set (not just its
  aggregate value) means a participant cannot claim a different equal-value deposit
  under a reused signature.
+ The value in the head output is increased accordingly
  $ valHead plus.o valDeposit = valHead' $
+ Transaction is signed by a participant
  $ exists {cid |-> keyHash_i |-> 1} in valHead' => keyHash_i in txKeys $
+ No minting or burning
  $ txMint = emptyset $
  (the bundle's `mintEmpty` field)
+ The ADA overhead $adaO$ is preserved across the state transition:
  $ adaO' = adaO $
  (enforced by the `step` field's type: the `increment` rule reuses the same `ada` binder on both sides)

These conditions form the `IncrementValid` bundle, over the additive
value-conservation predicate `incrementValueOK`.


A deposit claim must satisfy both validators run in the same transaction:
$nuHead$'s `incrementValid` above and $nuDeposit$'s `claimValid`
(@sec:recover-tx). The joint claim obligation is stated here as `ClaimTxValid`,
since it conjoins `incrementValid` with the deposit-side bundle.


#figure(incrementTx-diagram, caption: [$mtxIncrement$ transaction spending an open head output, producing a new head output which includes the new UTxO.]) <fig:incrementTx>

== Decrement Transaction <sec:decrement-tx>

The $mtxDecrement$ transaction (@fig:decrementTx) allows
a party to remove a UTxO from an already open head and consists of:

- one input spending from $nuHead$ holding the $st$ with $datumHead$,
- one output paying to $nuHead$ with value $valHead'$ and
  datum $datumHead'$,
- one or more decommit outputs $o_2 dots.h o_(m+1)$ with values $val_2 dots.h val_(m+1)$.

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("decrement"), xi, s, m, kappa^(\#))$, where $xi$ is a multi-signature of
the decrement snapshot which authorizes removal of some UTxO, $s$ is the
used snapshot number, $m$ is the number of outputs to distribute and
$kappa^(\#)$ is the hash of the snapshot's commit output set. The
validator checks:
+ State is advanced from $datumHead tilde stOpen$ to
  $datumHead' tilde stOpen$, parameters $cid, hydraKeys, nop, Tcontest$ stay
  unchanged and the new state is governed again by $nuHead$
  #transition-arrow("decrement")
  (the `decrement` rule of `_⟶⟨_⟩_`; the version is bumped, $v |-> v + 1$).
+ New version $v'$ is incremented correctly
  $ v' = v + 1 $
+ $xi$ is a valid multi-signature of the new snapshot state $eta'$
  $ msVfy(hydraKeys, (cid || v || s || (eta')^(\#) || delta^(\#) || kappa^(\#)), xi) = mtrue $
  where $(eta')^(\#) = hash(eta')$ is the hash of the new accumulator commitment $eta'$
  stored in the output datum, reflecting the UTxO set after removing the decommitted UTxOs;
  $kappa^(\#)$ is taken from the redeemer; and $delta^(\#)$ is _recomputed on-chain_ as the
  hash of the $m$ decommit outputs $o_2 dots.h o_(m+1)$ following the head output — the
  same output list the value check below sums. Binding $delta^(\#)$ to the exact decommit
  output set (address, datum, order and count, not just aggregate value) means a signer
  cannot redirect decommitted outputs while reusing a valid signature.
+ The value in the head output is decreased accordingly
  $ valHead' plus.o (plus.o.big_(j=2)^(m+1) val_j) = valHead $
+ Transaction is signed by a participant
  $ exists {cid |-> keyHash_i |-> 1} in valHead' => keyHash_i in txKeys $
+ No minting or burning
  $ txMint = emptyset $
  (the bundle's `mintEmpty` field)
+ The ADA overhead $adaO$ is preserved across the state transition:
  $ adaO' = adaO $
  (enforced by the `step` field's type: the `decrement` rule reuses the same `ada` binder on both sides)

These conditions form the `DecrementValid` bundle.


#figure(decrementTx-diagram, caption: [$mtxDecrement$ transaction spending an open head output, producing a new head output and multiple decommitted outputs.]) <fig:decrementTx>

== Close Transaction <sec:close-tx>

In order to close a head, a head member may post the $mtxClose$ transaction
(see @fig:closeTx). This transaction has
- one input spending from $nuHead$ holding the $st$ with $datumHead$,
- one output paying to $nuHead$ with value $valHead'$ and
  datum $datumHead'$.

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("close"), sans("CloseType"))$, where
$sans("CloseType")$ is a hint against which open state to close. (The closing
party posts $sans("postTx")(mtxClose, hatv, macron(mc(S)).v, macron(mc(S)).s, (eta')^(\#), xi)$
off-chain; on-chain the redeemer carries only $(xi, (eta')^(\#), delta^(\#), kappa^(\#))$ in
$sans("CloseType")$ — the signature, the accumulator hash and the decommit-
and commit-output-set hashes — while the version $v$ and snapshot number $s$ are
authenticated by the multisignature $xi$ over
$cid || v || s || (eta')^(\#) || delta^(\#) || kappa^(\#)$ and
recorded in the datum, rather than being separate redeemer fields.) The
transaction checks:
+ State is advanced from $datumHead tilde stOpen$ to
  $datumHead' tilde stClosed$, parameters $cid, hydraKeys, Tcontest$
  stay unchanged and the new state is governed again by $nuHead$
  #transition-arrow("close")
  (the `close` rule of `_⟶⟨_⟩_`; the version is preserved, $v' = v$).
  The closed state carries a single unified accumulator $eta'$ that combines the snapshotted UTxO set with any pending increment or decrement UTxOs using $accCombine$.
+ Last known open state version is recorded in closed state
  $ v' = v $

+ Based on the redeemer $sans("CloseType") = sans("Initial") union (sans("Any"), xi, (eta')^(\#), delta^(\#), kappa^(\#)) union (sans("Unused"), xi, (eta')^(\#), delta^(\#), kappa^(\#)) union (sans("Used"), xi, (eta')^(\#), delta^(\#), kappa^(\#))$, where $xi$ is a multi-signature of the closing snapshot, $(eta')^(\#)$ is the hash of the unified accumulator commitment stored in the output datum, and $delta^(\#)$/$kappa^(\#)$ are the snapshot's decommit-/commit-output-set hashes (passed verbatim into the signed message; they are authenticated only through $xi$), four cases are distinguished. In each case the closed state carries a single unified accumulator $eta'$:
  + $sans("Initial")$: The initial snapshot is used to close the head and open state was not updated. No signatures are available and it suffices to check
    $ v = 0 $
    $ s' = 0 $
    $ eta' = accUTxO(emptyset) $
  + $sans("Any")$: Closing snapshot refers to current state version $v$ with no pending increments or decrements, and $s' > 0$. The unified accumulator is simply the snapshotted state:
    $ eta' = accUTxO(U') $
    $ msVfy(hydraKeys, (cid || v || s' || (eta')^(\#) || delta^(\#) || kappa^(\#)), xi) = mtrue $
    $ (eta')^(\#) = hash(eta') $
  + $sans("Unused")$: Closing snapshot refers to current state version $v$ and a pending increment or decrement is _not_ applied in the snapshot. The unified accumulator is the snapshotted state only:
    $ eta' = accUTxO(U') $
    $ msVfy(hydraKeys, (cid || v || s' || (eta')^(\#) || delta^(\#) || kappa^(\#)), xi) = mtrue $
    $ (eta')^(\#) = hash(eta') $
  + $sans("Used")$: Closing snapshot refers to the previous state version $v - 1$ and a pending increment or decrement _is_ applied in the snapshot. The unified accumulator combines the snapshotted UTxOs with the pending delta:
    $ eta' = accCombine(accUTxO(U'), eta_Delta) $
    $ msVfy(hydraKeys, (cid || v - 1 || s' || (eta')^(\#) || delta^(\#) || kappa^(\#)), xi) = mtrue $
    $ (eta')^(\#) = hash(eta') $
    where $eta_Delta$ is the accumulator commitment of the pending delta UTxOs.

+ Initializes the set of contesters
  $ contesters = emptyset $
  This allows the closing party to also contest and is required for use
  cases where pre-signed, valid in the future, close transactions are
  used to delegate head closing.

+ Correct contestation deadline is set
  $ tfinal = txValidityMax + T $
+ Transaction validity range is bounded by
  $ txValidityMax - txValidityMin <= T $
  to ensure the contestation deadline $tfinal$ is at most $2*T$ in the future.
+ Value in the head is preserved exactly
  $ valHead' = valHead $
+ Transaction is signed by a participant
  $ exists {cid |-> keyHash_i |-> 1} in valHead' => keyHash_i in txKeys $
+ No minting or burning
  $ txMint = emptyset $
+ The ADA overhead $adaO$ is propagated unchanged from the open datum to the closed datum
  (enforced by the `close` rule's shared `ada` binder):
  $ adaO' = adaO $
  where $adaO$ is the ADA in the head UTxO not belonging to any L2 UTxO (minimum-UTxO overhead), set at initialisation time and unchanged for the head's lifetime. On fanout, the on-chain value conservation check treats $adaO$ as released from the head UTxO without requiring it in any distributed output, so it flows to whoever submits the fanout transaction as change (offsetting their transaction fee).

#dparagraph[Implementation note (accumulator construction).]
The per-case formulas for $eta'$ above ($accUTxO(U')$ for $sans("Any")$/$sans("Unused")$,
$accCombine(accUTxO(U'), eta_Delta)$ for $sans("Used")$) describe how the closing party constructs the
unified accumulator _off-chain_ before signing: $nuHead$ has neither $U'$ nor $eta_Delta$ and does not
recompute them. On-chain it verifies only the multisignature $xi$ over
$cid || v || s' || (eta')^(\#) || delta^(\#) || kappa^(\#)$
and the binding $(eta')^(\#) = hash(eta')$; the $sans("Initial")$ case additionally fixes $eta' =
accUTxO(emptyset) = G_1$ (a constant). The Agda `closeValid` bundle mirrors exactly this on-chain
view - `closeSigOK` (the multisignature, at $v$ or $v-1$ for $sans("Used")$), `closeηOK`
($(eta')^(\#) = hash(eta')$), and `closeInitialOK` ($eta = accUTxO(emptyset)$) - and likewise does
not recompute the off-chain $accUTxO$/$accCombine$ constructions, which are authenticated by $xi$.

The close checks are formalised per condition - the deadline equation, the
Initial-case constraint, the η-hash binding, the positive-snapshot Any case and
the version-dependent signature obligation - and conjoined in the `CloseValid`
bundle.


#figure(closeTx-diagram, caption: [$mtxClose$ transaction spending the $stOpen$ head output and producing a $stClosed$ head output with unified accumulator $eta'$.]) <fig:closeTx>

== Contest Transaction <sec:contest-tx>

The $mtxContest$ transaction (see @fig:contestTx) is posted by a
party to prove the currently $stClosed$ state is not the latest one. This
transaction has
- one input spending from $nuHead$ holding the $st$ with $datumHead$,
- one output paying to $nuHead$ with value $valHead'$ and
  datum $datumHead'$.

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("contest"), sans("ContestType"))$, where
$sans("ContestType")$ is a hint how to verify the snapshot and checks:
+ State is advanced from $datumHead tilde stClosed$ to
  $datumHead' tilde stClosed$, parameters $cid, hydraKeys, Tcontest$
  stay unchanged and the new state is governed again by $nuHead$
  #transition-arrow("contest")
  (the `contest` rule of `_⟶⟨_⟩_`; the version is preserved and the contester set
  grows by one key, $contesters' = contesters union { keyHash }$).
  The closed state carries a single unified accumulator $eta'$ computed using $accCombine$ based on the contest type.

+ Last known open state version stays recorded in closed state
  $ v' = v $

+ Contested snapshot number $s'$ is higher than the currently stored snapshot number $s$
  $ s' > s $
+ Based on the redeemer $sans("ContestType") = (sans("Unused"), xi, (eta')^(\#), delta^(\#), kappa^(\#)) union (sans("Used"), xi, (eta')^(\#), delta^(\#), kappa^(\#))$, where $xi$ is a multi-signature of the contesting snapshot, $(eta')^(\#) = hash(eta')$ is the hash of the unified accumulator commitment stored in the output datum, and $delta^(\#)$/$kappa^(\#)$ are the snapshot's decommit-/commit-output-set hashes (passed verbatim into the signed message, as for close), two cases are distinguished:

  + $sans("Unused")$: Contesting snapshot refers to current state version $v$ (pending delta not applied in snapshot). The unified accumulator reflects only the snapshotted UTxOs:
    $ eta' = accUTxO(U') $
    $ msVfy(hydraKeys, (cid || v || s' || (eta')^(\#) || delta^(\#) || kappa^(\#)), xi) = mtrue $
    $ (eta')^(\#) = hash(eta') $
  + $sans("Used")$: Contesting snapshot refers to the previous state version $v - 1$ (pending delta applied in snapshot). The unified accumulator combines the snapshotted UTxOs with the pending delta:
    $ eta' = accCombine(accUTxO(U'), eta_Delta) $
    $ msVfy(hydraKeys, (cid || v - 1 || s' || (eta')^(\#) || delta^(\#) || kappa^(\#)), xi) = mtrue $
    $ (eta')^(\#) = hash(eta') $
    where $eta_Delta$ is the accumulator commitment of the pending delta UTxOs.

+ The single signer ${keyHash} = txKeys$ has not already contested and is added to the set of contesters
  $ keyHash in.not contesters $
  $ contesters' = contesters union keyHash $
+ Transaction is posted before deadline
  $ txValidityMax <= tfinal $
+ Contestation deadline is updated correctly to
  $ tfinal' = cases(
    tfinal & upright("if") ~ |contesters'| = n",",
    tfinal + T & upright("otherwise.")
  ) $
+ Value in the head is preserved exactly
  $ valHead' = valHead $
+ Transaction is signed by a participant
  $ exists {cid |-> keyHash_i |-> 1} in valHead' => keyHash_i in txKeys $
+ No minting or burning
  $ txMint = emptyset $
+ The ADA overhead $adaO$ is preserved in the output closed datum
  (enforced by the `contest` rule's shared `ada` binder):
  $ adaO' = adaO $

The contest checks - the conditional deadline update, the η-hash binding and the
version-dependent signature obligation - form the `ContestValid` bundle; the
shared signed-by-a-participant obligation is derived from its sharper contester
fields. NB the bundle's `contesterSigned` captures that the
appended contester is _a_ transaction signer; the implementation's stronger
sole-signer cardinality (${keyHash} = txKeys$, exactly one signer) is not
modelled.



#figure(contestTx-diagram, caption: [$mtxContest$ transaction spending the $stClosed$ head output and producing a different $stClosed$ head output.]) <fig:contestTx>

== Fan-Out Transaction <sec:fanout-tx>

Once the contestation phase is over, a head may be finalized by posting a
$mtxFanout$ transaction (see @fig:fanoutTx), which
distributes all UTxOs from the head according to the unified accumulator in the closed state. A fanout transaction consists of
- one input spending from $nuHead$ holding the $st$, and
- outputs $o_1 dots.h o_m$ to distribute all UTxOs.

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("fanout"), m, pi, sans("crsRef"))$, where:
- $m$ is the number of outputs to distribute from the $stClosed$ state,
- $pi$ is the KZG membership witness,
- $sans("crsRef")$ is the output reference of the reference input holding the Common Reference String (CRS).
The validator checks:
+ State is advanced from $datumHead tilde stClosed$ to terminal state $stFinal$:
  #transition-arrow("fanout")
  (the `fanout` rule of `_⟶⟨_⟩_`; a $stClosed$ datum steps to the terminal state).
+ The CRS reference input named by $sans("crsRef")$ carries the _canonical_ trusted
  setup: the hash of its decoded G2-point datum equals the canonical setup hash the
  validator is parameterised with at compile time,
  $ hash(sans("crsData")) = sans("canonicalCRS")^(\#) $
  (`InvalidCRSDatum` otherwise; an unresolvable or undecodable reference input
  rejects with `MissingCRSRefInput`/`MissingCRSDatum`). Without this binding an
  attacker could supply a substituted powers-of-tau setup with known trapdoor
  $tau$ and forge membership witnesses for arbitrary outputs — and since fan-out
  is permissionless after the deadline, that would be direct fund theft. The
  membership check below runs against this canonical CRS.
+ All $m$ outputs are verified as members of the unified accumulator $eta$ using the membership witness $pi$:
  $ accVerify(eta, {o_1, dots.h, o_m}, pi) = mtrue $
+ Transaction is posted after contestation deadline $txValidityMin > tfinal$.
+ All tokens are burnt
  $|{cid |-> dot.c |-> -1} in txMint| = n + 1$.
+ The head input value is fully conserved:
  $ val_(sans("head"))^(sans("in")) = plus.o.big_(i=1)^(m) val(o_i) plus.o val_(sans("burned")) plus.o adaO $
  where $adaO$ is the ADA overhead carried from the closed datum, and $val_(sans("burned"))$ is the value of all burned head tokens.

The fan-out checks - the burn count, membership of the distributed outputs in the
accumulator, the deadline, value conservation and the canonical-CRS binding - form
the `FanoutValid` bundle.


#figure(fanoutTx-diagram, caption: [$mtxFanout$ transaction spending the $stClosed$ head output with unified accumulator $eta$ and distributing funds with outputs $o_1 dots.h o_m$.]) <fig:fanoutTx>

=== Intermediate Partial Fan-Out Transaction <sec:partial-fanout-tx>

When UTxO sets exceed transaction size limits, the protocol distributes UTxOs across
multiple transactions using partial fanout. Each intermediate step distributes a subset of UTxOs and
transitions the head into the $stFanoutProgress$ state, which carries only the fields needed for
subsequent steps:
$ (stFanoutProgress, cid, hydraKeys, nop, tfinal, eta, adaO) $
where $nop$ is the number of participants, $tfinal$ is the contestation deadline (carried from $stClosed$), $eta$ is the
current accumulator commitment, and $adaO$ is the ADA overhead in the head UTxO not
belonging to any L2 UTxO (propagated from the closed datum).

An intermediate partial fanout transaction (see @fig:partialFanoutTx) consists of:
- one input spending from $nuHead$ in state $stClosed$ or $stFanoutProgress$, and
- a continuing head output at index 0 in state $stFanoutProgress$ with updated accumulator, and
- outputs $o_1 dots.h o_m$ at indices $1 dots.h m$ distributing a subset of UTxOs.

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("partialFanout"), m, sans("crsRef"))$, where $m$ is the number of UTxO outputs to distribute in this step and $sans("crsRef")$ is the output reference of the reference input holding the CRS.
The validator checks:
+ $m > 0$ (no zero-output batches).
+ The CRS reference input carries the canonical trusted setup,
  $hash(sans("crsData")) = sans("canonicalCRS")^(\#)$ (`InvalidCRSDatum` otherwise;
  see the fan-out section for the rationale). The exclusion check below runs
  against this canonical CRS.
+ State transitions into $stFanoutProgress$ with updated accumulator. For a $stClosed$ input:
  #transition-arrow("partialFanoutStart")
  (the `partialFanoutStart` rule of `_⟶⟨_⟩_`; $stClosed$ steps to $stFanoutProgress$).
  For a $stFanoutProgress$ input:
  #transition-arrow("partialFanoutStep")
  (the `partialFanoutStep` rule of `_⟶⟨_⟩_`; $stFanoutProgress$ steps to itself).
+ The new accumulator $eta'$ (from the output datum) is not the G1 generator — all elements have _not_ yet been removed (use $stFinalPartialFanout$ for the last batch):
  $ eta' != G_1 $
+ No minting or burning $txMint = emptyset$.
+ Transaction is posted after contestation deadline $txValidityMin > tfinal$.
+ Parameters $cid$, $hydraKeys$, $tfinal$, and $adaO$ are preserved in the output $stFanoutProgress$ datum.
+ Value is conserved: head input value equals head output value plus all distributed outputs:
  $ val_(sans("head"))^(sans("in")) = val_(sans("head"))^(sans("out")) plus.o plus.o.big_(i=1)^(m) val(o_i) $
+ The new accumulator $eta'$ from the output datum correctly represents the remaining UTxOs after removing $S = {o_1, dots.h, o_m}$. The output datum value serves as the exclusion witness:
  $ accVerifyExclude(eta, S, eta') = mtrue $

An intermediate step's conditions form the `PartialFanoutValid` bundle.


#figure(partialFanoutTx-diagram, caption: [$mtxPartialFanout$ transaction spending the $stFanoutProgress$ head output, distributing outputs $o_1 dots.h o_m$, and producing a new $stFanoutProgress$ head output with updated accumulator $eta'$.]) <fig:partialFanoutTx>

=== Final Partial Fan-Out Transaction <sec:final-partial-fanout-tx>

Once all UTxOs except the last batch have been distributed via $stPartialFanout$ steps,
the final step burns all head tokens and distributes the remaining UTxOs.
A final partial fanout transaction (see @fig:finalPartialFanoutTx) consists of:
- one input spending from $nuHead$ in state $stFanoutProgress$, and
- outputs $o_1 dots.h o_m$ distributing the remaining UTxOs (no continuing head output).

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("finalPartialFanout"), m, pi, sans("crsRef"))$, where $m$ is the number of UTxO outputs, $pi$ is the KZG membership witness, and $sans("crsRef")$ is the CRS reference.
The validator checks:
+ $m > 0$ (prevents fund theft via a zero-output proof bypass).
+ The CRS reference input carries the canonical trusted setup,
  $hash(sans("crsData")) = sans("canonicalCRS")^(\#)$ (`InvalidCRSDatum` otherwise;
  see the fan-out section for the rationale).
+ State is advanced from $stFanoutProgress$ to terminal state $stFinal$:
  #transition-arrow("finalPartialFanout")
  (the `finalPartialFanout` rule of `_⟶⟨_⟩_`; $stFanoutProgress$ steps to the terminal state).
+ All head tokens are burnt
  $|{cid |-> dot.c |-> -1} in txMint| = n + 1$.
+ Transaction is posted after contestation deadline $txValidityMin > tfinal$.
+ The $m$ distributed outputs are verified as members of the accumulator $eta$ using the membership witness $pi$:
  $ accVerify(eta, {o_1, dots.h, o_m}, pi) = mtrue $
+ Value is conserved:
  $ val_(sans("head"))^(sans("in")) = plus.o.big_(i=1)^(m) val(o_i) plus.o val_(sans("burned")) plus.o adaO $

The final step's conditions form the `FinalPartialFanoutValid` bundle.


#figure(finalPartialFanoutTx-diagram, caption: [$mtxFinalPartialFanout$ transaction spending the $stFanoutProgress$ head output, distributing the final batch of UTxOs $o_1 dots.h o_m$, and burning all head tokens to reach $stFinal$.]) <fig:finalPartialFanoutTx>

The $muHead(seed)$ minting policy governs the burning of tokens via
redeemer $sans("burn")$ that:
+ All tokens in $txMint$ need to be of negative quantity
  $forall {cid |-> dot.c |-> q} in txMint : q < 0$.
