```
module Hydra.Protocol.OffChain where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.Preliminaries
open import Hydra.Protocol.Setup
open import Hydra.Protocol.OnChain using (outputRef-eqᵇ)
open import Hydra.Protocol.OffChainReference using (snapshotCapacityRef; decommitCapacityRef; pendingTxFitsRef)
import Hydra.Protocol.OffChainReference as ReqTxRef
-- Extra list/nat helpers for the off-chain handler model (the deposit registry + tick).
open import Data.List using (map; _++_)
open import Data.List.Properties using (length-map)
open import Data.Nat using (_<?_)
open import Relation.Binary.PropositionalEquality using (sym; trans)
```

#import "/template.typ": *
#import "/macros.typ": *

= Off-Chain Protocol <sec:offchain>

This section describes the actual Coordinated Hydra Head protocol, an even more
simplified version of the original publication~@hydrahead20. See the
protocol overview in @sec:overview for an introduction and notable
changes to the original protocol. While the on-chain part already describes the
full life-cycle of a Hydra head on-chain, this section completes the picture by
defining how the protocol behaves off-chain and notably the relationship between
on- and off-chain semantics. Participants of the protocol are also called Hydra
head members, parties or simply protocol actors. The protocol is specified as a
reactive system that processes three kinds of inputs:

+ On-chain protocol transactions as introduced in
  @sec:on-chain, which are posted to the mainchain and can be
  observed by all actors:
  - $mono("initialTx")$: opens a head
  - $mono("depositTx")$: some UTxO was deposited to be incremented
  - $mono("recoverTx")$: deposited UTxO was recovered
  - $mono("incrementTx")$: adds UTxO to an open head
  - $mono("decrementTx")$: removes UTxO from an open head
  - $mono("closeTx")$: closes a head
  - $mono("contestTx")$: contests a closed head
  - $mono("fanoutTx")$: distributes all UTxOs from a closed head in one step
  - $mono("partialFanoutTx")$: distributes a subset of UTxOs, transitioning to $stFanoutProgress$ (intermediate)
  - $mono("finalPartialFanoutTx")$: distributes the last subset and burns tokens (terminal from $stFanoutProgress$)

  Also, a special input when time advanced on chain may be used:
  - $mono("tick")$: time advanced on chain

+ Off-chain network messages sent between protocol actors (parties):
  - $hpRT$: to request a transaction to be included in the next snapshot
  - $hpRD$: to request exclusion of UTxO via a decommit transaction
  - $hpRS$: to request a snapshot to be created & signed by every head member
  - $hpAS$: to acknowledge a snapshot by replying with their signatures
+ Commands issued by the participants themselves or on behalf of end-users and clients
  - $hpInit$: to open a head
  - $hpClose$: to request closure of an open head

// TODO: define states and e.g. that newTx not possible when closed? state diagram?

The behavior is fully specified in @fig:off-chain-prot, while the
following paragraphs introduce notation, explain variables and walk-through the
protocol flow.

The Haskell interpreter receives transaction application, snapshot signing and
signature verification as functions in one configuration. Their returned results
feed the protocol rules below; the configuration does not select different
completion, waiting or rejection semantics.

== Assumptions

On top of the statements of the protocol setup in @sec:setup, the
off-chain protocol logic relies on these assumptions:
#todo[move/merge with protocol setup?]
- Every network message received from a specific party is checked for
  authentication. An implementation of the specification needs to find a
  suitable means of authentication, either on the communication channel or
  for individual messages. Unauthenticated messages must be dropped.
- The head protocol gets correctly (and with completeness) notified about
  observed transactions on-chain belonging to the respective head
  instance.
- All inputs are processed to completion, i.e. run-to-completion
  semantics and no preemption.
- Inputs are deduplicated. That is, any two identical inputs must not lead
  to multiple invocations of the handling semantics.
- Given the specification, inputs may pile up forever and implementations
  need to consider these situations (i.e. potential for DoS). A valid
  reaction to this would be to just drop these inputs. Note that, from a
  security standpoint, these situations are identical to a
  non-collaborative peer and closing the head is also a possible reaction.
- The lifecycle of a Hydra head on-chain does not cross (hard fork)
  protocol update boundaries. Note that these inputs are announced in
  advance hence it should be possible for implementations to react in such
  a way as to expedite closing of the head before such a protocol update.
  This further assumes that the contestation period parameter is picked
  accordingly.

== Notation
- $U applytx tx$ applies transaction $tx$ to the UTxO set $U$, yielding the
  updated set, or $bot$ if $tx$ does not apply (a conflict). It is kept
  abstract as part of the off-chain trust base; see the `applyTxs` laws below.
- #kw("on") _event_ specifies how the protocol reacts on a given input
  _event_. Further information may be available from the constituents of
  _event_ and origin of the input.
- #kw("require") $p$ means that boolean expression $p in tyBool$ must be satisfied
  for the further execution of a routine, while discontinued on $not p$. A
  conservative protocol actor could interpret this as a reason to close
  the head.
- #kw("wait") $p$ is a non-blocking wait for boolean predicate $p in tyBool$
  to be satisfied. On $not p$, the execution of the routine is stopped,
  queued, and reactivated at latest when $p$ is satisfied.
- #kw("multicast") _msg_ means that a message _msg_ is (channel-) authenticated
  and sent to all participants of this head, including the sender.
- #kw("postTx") $tx$ has a party create transaction $tx$, potentially from
  some data, and submit it on-chain. See @sec:on-chain for
  individual transaction details.
- #kw("output") _event_ signals an observation of _event_, which is used in the
  security definition and proofs of @sec:security. This
  keyword can be ignored when implementing the protocol.

== Variables

Besides parameters agreed in the protocol setup (see @sec:setup), a
party's local state consists of the following variables:

- $hatv$: Last seen open state version.
- $hats$: Sequence number of latest seen snapshot.
- $hatSigma in (tyNatural times tyBytes)^*$: Accumulator of
  signatures of the latest seen snapshot, indexed by parties.
- $hatmL$: UTxO set representing the local ledger state resulting from
  applying $hatmT$ to $macron(S).U$ to validate requested transactions.
- $hatmT in mT^*$: List of transactions applied locally and pending
  inclusion in a snapshot (if this party is the next leader).
- $tx_alpha in cal(T)$: Pending deposit transaction#footnote[In
  fact this would only need to be a transaction id to look up the
  corresponding deposit in $mc(D)$] to be used for incrementing the head
  state.
- $tx_omega in cal(T)$: Pending decrement transaction, whose outputs are to be
  withdrawn from the head.
- $mc(D)$: Set of deposit objects tracking deposit transactions with their
  status.
- $macron(mc(S))$: Snapshot object of the latest confirmed snapshot which
  contains:
  #align(center)[
    #table(
      columns: 2,
      align: left,
      $macron(mc(S)).v$, [snapshot version],
      $macron(mc(S)).s$, [snapshot number],
      $macron(mc(S)).mT$, [list of transactions relating this snapshot to the previous],
      $macron(mc(S)).U$, [snapshotted UTxO set],
      $macron(mc(S)).U_alpha$, [pending UTxO to increment],
      $macron(mc(S)).txOutRef_alpha$, [deposit the pending UTxO to increment comes from],
      $macron(mc(S)).U_omega$, [pending UTxO to decrement],
      $macron(mc(S)).(eta')^(\#)$, [hash of the accumulator commitment over the snapshotted UTxO set],
    )
  ]

where constructor $"snObj"(v, n, T, U, U_alpha, U_omega)$ initializes a
new snapshot object with $macron(cal(S)).(eta')^(\#) = bot$.
The Agda `Snapshot` record additionally carries the head id $cid$ (the first
signed component of the message
$cid || v || s || (eta')^(\#) || delta^(\#) || kappa^(\#)$), the decommit- and
commit-output-set hashes $delta^(\#)$/$kappa^(\#)$ (the node's
`decommitOutputsHash`/`commitOutputsHash`, bound into the signed message), the
deposit reference $macron(mc(S)).txOutRef_alpha$ the pending increment comes
from (bound into $kappa^(\#)$, node `depositTxId`) and
the aggregate-signature slot $macron(mc(S)).sigma$ ($bot$ until confirmed),
which the figure leaves implicit. \
\
Additionally, deposit objects are created using
$"depositObj"(U, t_("created"), t_("deadline"), "status")$
where status can be $"Inactive"$, $"Active"$, or $"Expired"$.

A pending commit is not part of the snapshotted UTxO set until its increment
has settled on-chain: $underline(tx)_(sans("req"))$ is applied to
$U_(sans("active"))$ *without* $U_alpha$ in the $hpRS$ handler below, so a
requested transaction cannot spend it. Were $U_alpha$ spendable there, such a
transaction would consume it and its value would be accounted for twice — once
through that transaction's outputs in $U$ and once again through $U_alpha$ —
leaving the head under-backed. Nor may a deposit name an output reference the
active set already holds or one a requested transaction creates: a deposit
datum is unauthenticated, so any L1 actor can fund a deposit naming an existing
output with a copy of its content, and absorbing it would add the deposit's
value to the head without growing the UTxO set (the union is by reference),
or delete that output from the ledger should the deposit expire before its
increment. Such a request is rejected, and a leader never makes one: a fresh
deposit is picked only if it is disjoint from $macron(mc(S)).U$ and from the
outputs of the transactions it is requested with, while the pending commit
still settling — which every later request at this version must carry — is
kept while transactions that cannot accompany it are excluded from the request.
If the first pending transaction is blocked, an empty snapshot can prune it,
allowing later independent transactions to proceed before the increment settles.

=== Snapshot capacity

The deployed accumulator setup contains 32768 powers and supports at most 32767
output references in either fanout set. Write
$sans("fits")(U,C,D)$ for $|U union D| <= 32767 and |U union C| <= 32767$,
where $U$ is active UTxO, $C$ a pending commit, and $D$ pending decommit outputs.
These are alternative future fanouts, not a union of all three sets. Unions are
by output reference, so equal output values at different references count twice.
An absent incremental action contributes the empty set.

The implementation supplies this bound through public rule configuration:
$sans("fits")_L(U,C,D)$ requires both counts to be at most $L$.
Cardano runtime wiring fixes $L=32767$; selection, pruning, follower requests,
decommit admission, and side-load validation use the same configured bound.
The API rejects oversized side-loads before queueing, using the Cardano default,
and the core repeats the non-forcing check. This is an implementation parameter;
it adds no negotiated setting or protocol transition.

Optimistic transaction acceptance does not enforce this bound. The leader's
$sans("requestable")(T,tx_alpha,tx_omega)$ selects the longest prefix of its
bounded candidate list that avoids re-creating references held by the settled
confirmed ledger or settling commit and satisfies $sans("fits")$ after every transaction.
Its starting active ledger is the settled confirmed UTxO, with a requested
decommit applied and its outputs excluded. A later merge cannot repair an
overflowing earlier prefix. The node considers at most 4000 transactions per
request; this is a construction policy, not a limit imposed on received requests.
These prefix checks reserve already-settling incremental actions. A fresh deposit
is chosen after selecting the ordinary prefix, and must fit alongside its final
UTxO; it does not impose another check on that prefix's intermediate states.
Otherwise the deposit is deferred. The figure's $sans("depositFits")(D)$ checks
the selected ledger and $D.U$. Write $sans("outputs")(bot)=emptyset$.
A settling commit stays in the request. Pending transactions
justify either a non-empty request or an empty pruning snapshot; a settling
incremental action alone does not repeatedly trigger empty snapshots.

Let $sans("settled")(macron(mc(S)),hatv)$ include the confirmed snapshot's
pending commit only when its increment has advanced the version. Before
admitting or broadcasting a decommit, the node first checks local ledger
applicability, then requires non-empty outputs and capacity after arithmetic
application to *both* the optimistic and settled confirmed ledgers, before
excluding the decommit's outputs. Confirmed-space checking is conservative:
an unconfirmed merge may lie outside the next request's transaction prefix.
Arithmetic application removes named inputs that exist and adds outputs; it
does not require the decommit to be ledger-applicable against confirmed state.
Both client decommit submission and the received $hpRD$ handler use these guards.

The executable reference also checks a small-set cycle with capacity three.
The split spends reference 1 into 4 and 5; the independent merge spends 2 and 3
into 6. Selection stops before the split. Pruning skips it and keeps the merge,
which is then requestable. These equalities cover that finite example, not
distributed liveness. Haskell tests exercise the supplied small bound directly
and bridge it to the deployed one with untouched background outputs. They also
run the actual request/sign/confirm sequence including the retained request
marker and checkpoint recovery.

```
capacity-configured-boundary :
  ReqTxRef.capacityFitsRef 2 (1 ∷ []) (2 ∷ []) (3 ∷ []) ≡ true
capacity-configured-boundary = refl

capacity-configured-overflow :
  ReqTxRef.capacityFitsRef 1 (1 ∷ []) (2 ∷ []) (3 ∷ []) ≡ false
capacity-configured-overflow = refl

capacity-stops-at-split :
  ReqTxRef.capacityPrefixRef 3 (1 ∷ 2 ∷ 3 ∷ []) []
    ((1 ∷ []) ∷ (2 ∷ 3 ∷ []) ∷ []) ((4 ∷ 5 ∷ []) ∷ (6 ∷ []) ∷ []) ≡ 0
capacity-stops-at-split = refl

capacity-pruning-keeps-merge :
  ReqTxRef.capacityPruneRef 3 (1 ∷ 2 ∷ 3 ∷ []) [] []
    ((1 ∷ []) ∷ (2 ∷ 3 ∷ []) ∷ []) ((4 ∷ 5 ∷ []) ∷ (6 ∷ []) ∷ []) ≡ (2 ∷ [])
capacity-pruning-keeps-merge = refl

capacity-merge-is-requestable :
  ReqTxRef.capacityPrefixRef 3 (1 ∷ 2 ∷ 3 ∷ []) []
    ((2 ∷ 3 ∷ []) ∷ []) ((6 ∷ []) ∷ []) ≡ 1
capacity-merge-is-requestable = refl
```

A settling commit — $macron(mc(S)).U_alpha != bot$ at the current version — is
dropped, by a request claiming no deposit, only once its increment can no
longer land: $sans("unclaimable")(tx_alpha)$ holds when the deposit was spent
on-chain (recovered, so $tx_alpha in.not mc(D)$) or the chain time passed its
deadline — which bounds the validity of any transaction claiming it — by more
than $Tdeposit$, the protocol's settlement margin against rollbacks (a deposit
stops being claimed that long before its deadline for the same reason). Merely
$sans("Expired")$ is too early — that status is set $Tdeposit$ ahead of the
deadline — so a settling commit whose deposit counts as $sans("Expired")$ is
still carried on.

A settling decommit — $macron(mc(S)).U_omega != bot$ at the current version —
is never dropped: every request at that version must carry a decommit with the
same outputs, as the leader does until it observes the $mtxDecrement$, which
bumps the version. Nothing bounds how long that transaction may take to land —
it is posted again with every snapshot confirmed carrying the decommit — and a
request without it would leave the decommitted outputs in no confirmed snapshot
once it lands, while a deposit claimed in their place would race it for the
version bump; either way the head could no longer be closed and fanned out from
a snapshot its honest parties hold.

Transactions may not re-create an output reference the ledger holds either:
unique on L1, output references are not on L2, where a deposit names arbitrary
ones and enters the ledger with its increment. A member can compute the id of a
transaction before submitting it, fund a deposit naming its outputs and submit
the transaction once the deposit is absorbed; the union by reference would drop
one of the two entries while the head keeps both values. The $hpRT$ handler
refuses such a transaction (also one naming an output a deposit still in play
names: one not $sans("Expired")$, or the one settling), and $hpRS$ requires
$sans("outputs")(underline(tx)_(sans("req"))) inter U_(sans("active")) = emptyset$. Dropping it earlier would let a landing increment add the
deposit's value to the head but not to the ledger, leaving the head over-backed
and impossible to fan out. The $hpRS$ handler waits for $sans("unclaimable")$
before signing such a request, since the leader may have observed the recovery
or the chain time first; that wait is not bounded by the message's retry budget,
as it resolves by itself once this party's chain time passes the margin.

In Agda, a UTxO set is a finite map from output references to outputs, and a
party's local state and the protocol messages are captured by dedicated record
types.

```
UTxO : Set
UTxO = OutputRef ⇀ Output

-- §6 deposit objects (the 𝒟 registry). A deposit moves Inactive → Active → Expired over time as
-- chain `tick`s arrive (see `depositStatusAt` and the `_observes_↝_` chain-event relation below).
data DepositStatus : Set where
  Inactive Active Expired : DepositStatus

record DepositObj : Set where      -- §6 depositObj(U, t_created, t_deadline, status)
  constructor depositObj
  field
    utxoD    : UTxO                 -- the deposited UTxO U
    created  : ℕ                    -- t_created
    deadline : ℕ                    -- t_deadline
    status   : DepositStatus

record Snapshot : Set where        -- the confirmed snapshot object S̄
  field
    cid     : ℍ                    -- S̄.cid (head currency id; the FIRST signed component, cid‖v‖s‖η#‖η̂#‖δ#‖κ#)
    version : ℕ                    -- S̄.v
    number  : ℕ                    -- S̄.s
    txs     : List Data            -- S̄.T
    utxo    : UTxO                 -- S̄.U (descriptive, see the note after this record)
    utxoInc : UTxO                 -- S̄.U_α (pending increment; a κ# argument, see `signHonest`)
    depRef  : Maybe Data           -- S̄.txOutRef_α: the deposit the pending increment comes from
                                   -- (node `Snapshot.depositTxId`). An argument of κ# via the
                                   -- `signHonest` digest premise (@sec:security), which is what
                                   -- makes the signature authorize that one deposit and no other
                                   -- recording the same UTxO.
    utxoDec : UTxO                 -- S̄.U_ω (pending decrement; the δ# argument, see `signHonest`)
    etaHash : ℍ                    -- S̄.(η')# (hash of the commitment to the outputs the head owes at this
                                   -- snapshot's version, node `accumulator`; signed 4th component)
    appliedEtaHash : ℍ             -- S̄.(η̂')# (hash of the commitment to the outputs the head owes once the
                                   -- pending increment/decrement has been applied on chain, node
                                   -- `appliedAccumulator`; signed 5th component; equals etaHash when
                                   -- nothing is pending)
    decHash : ℍ                    -- S̄.δ# (decommit-output-set hash, node `decommitOutputsHash`; signed 6th component)
    comHash : ℍ                    -- S̄.κ# (commit-output-set hash, node `commitOutputsHash`; signed 7th component)
    sig     : Maybe AggSig         -- S̄.σ, mirroring the §6 snapshot object (descriptive: the proofs
                                   -- read signing status from the system's `sigs`/`aggSigOf`, never
                                   -- from here, so this field marks nothing on its own)

record LocalState : Set where      -- a party's local state (besides setup params)
  field
    params             : HeadParameters
    seenVersion        : ℕ          -- v̂
    seenNumber         : ℕ          -- ŝ
    seenSigs           : List (ℕ × PartySig) -- Σ̂ (individual signatures, indexed by party)
    localLedger        : UTxO       -- L̂
    pending            : List Data  -- T̂ (txs pending a snapshot)
    confirmed          : Snapshot   -- S̄
    currentDepositTxId : Maybe Data -- tx_α (pending deposit tx-id, the key into 𝒟; ⊥ if none — node's currentDepositTxId)
    pendingDecrement   : Maybe Data -- tx_ω (pending decrement, ⊥ if none)
    deposits           : List (Data × DepositObj)  -- 𝒟 (registry keyed by deposit tx-id tx_α)

data Message : Set where           -- network messages of the coordinated head (§6)
  reqTx  : (tx : Data)                          → Message  -- hpRT
  reqDec : (tx : Data)                          → Message  -- hpRD
  -- hpRS (full payload). NB the payload order here (deposit txα before decommit txω) follows
  -- the §6 figure; the Haskell `ReqSn` record (Hydra.Network.Message) lists the same fields by
  -- NAME as txReq↔transactionIds, txα↔depositTxId, txω↔decommitTx, with decommit before deposit
  -- - match them by name, not position (the deposit and decommit slots are easy to confuse).
  reqSn  : (v s : ℕ) (txReq txα txω : Data)     → Message
  ackSn  : (s : ℕ) (σ : PartySig)               → Message  -- hpAS (σⱼ, an individual signature)
```

The local-ledger application $applytx$ and the UTxO-map operations the handlers
use are kept abstract, as the off-chain trust base: `applyTxs` with its nil and
compositionality laws (the two ledger laws the @sec:security proofs consume), the
joint-applicability predicate `Applicable`, and the map operations recording how
$hatmL$ changes.

```
-- Ledger application: apply a transaction list to a UTxO set; `nothing` = ⊥ (conflict). Kept abstract.
-- `applyTxs-nil` is the (trivial) ledger law that applying no transactions never conflicts.
-- `applyTxs-compose` is ledger COMPOSITIONALITY: applying `txs₁` then `txs₂` equals applying their
-- concatenation (the one ledger law, alongside nil, that lets the §7 honest-signer applicability guard
-- be DERIVED against U₀). The off-chain handler arms below also use `applyTxs`/`Applicable`,
-- which is why they live here; @sec:security re-exports them via its `open import` of this module.
postulate
  applyTxs         : UTxO → List Data → Maybe UTxO
  applyTxs-nil     : ∀ U → applyTxs U [] ≡ just U
  applyTxs-compose : ∀ {U U′} txs₁ txs₂ → applyTxs U txs₁ ≡ just U′
                   → applyTxs U (txs₁ ++ txs₂) ≡ applyTxs U′ txs₂

-- A transaction list is jointly applicable to U when applying it does not conflict (≠ ⊥).
Applicable : UTxO → List Data → Set
Applicable U txs = ¬ (applyTxs U txs ≡ nothing)

-- UTxO-map operations used by the local-ledger (L̂) updates of the handlers below. Kept abstract (same
-- trust family as `applyTxs`): the handler arms only need them to RECORD how L̂ changes (∅ at head open,
-- ∪ on increment, ∖ outputs(tx) on decommit), faithfully to the §6 figure; the §7 invariants do not
-- reason about ledger CONTENTS, so no laws are required here.
infixl 6 _∪ᵘ_ _∖ᵘ_
postulate
  εᵘ      : UTxO                -- the empty UTxO map (L̂ ← ∅ when the head opens)
  _∪ᵘ_    : UTxO → UTxO → UTxO  -- UTxO-map union (L̂ ∪ U when an increment is observed)
  _∖ᵘ_    : UTxO → UTxO → UTxO  -- UTxO-map difference (… ∖ outputs(tx) when a decommit is requested)
  outputs : Data → UTxO         -- the outputs a transaction produces, as a UTxO map
  -- Arithmetic update: remove named inputs that exist and add outputs, without
  -- ledger validation. The confirmed decommit-capacity check uses this even
  -- when the transaction spends an unconfirmed local transaction's output.
  applyUTxO : UTxO → Data → UTxO

-- Count distinct output REFERENCES, not distinct output values. The list model
-- can represent a set with duplicate entries, so count each key only once.
utxoRefs : UTxO → List OutputRef
utxoRefs U = map proj₁ (setToList (proj₁ U))

refOccurs : OutputRef → List OutputRef → Bool
refOccurs r [] = false
refOccurs r (x ∷ xs) = if outputRef-eqᵇ r x then true else refOccurs r xs

countRefs : List OutputRef → ℕ
countRefs [] = 0
countRefs (r ∷ rs) = if refOccurs r rs then countRefs rs else suc (countRefs rs)

utxoCount : UTxO → ℕ
utxoCount U = countRefs (utxoRefs U)

refsDisjoint : List OutputRef → List OutputRef → Bool
refsDisjoint [] _ = true
refsDisjoint (r ∷ rs) other = if refOccurs r other then false else refsDisjoint rs other

SnapshotFits : UTxO → UTxO → UTxO → Set
SnapshotFits U C D = snapshotCapacityRef (utxoCount (U ∪ᵘ D)) (utxoCount (U ∪ᵘ C)) ≡ true

settledConfirmed : LocalState → UTxO
settledConfirmed st =
  if ⌊ Snapshot.version S <? LocalState.seenVersion st ⌋
  then Snapshot.utxo S ∪ᵘ Snapshot.utxoInc S else Snapshot.utxo S
  where S = LocalState.confirmed st

DecommitFits : LocalState → Data → UTxO → Set
DecommitFits st tx localAfter =
  decommitCapacityRef (utxoCount (outputs tx)) (utxoCount localAfter)
    (utxoCount (applyUTxO (settledConfirmed st) tx)) ≡ true

-- Operational post-snapshot pruning. Keep a transaction only if it applies,
-- avoids the carried commit's output references and fits both possible fanouts.
-- Skipping a transaction leaves U unchanged and still considers later merges.
prunePending : UTxO → UTxO → UTxO → List Data → UTxO × List Data
prunePending U C D [] = U , []
prunePending U C D (tx ∷ txs) with applyTxs U (tx ∷ [])
... | nothing = prunePending U C D txs
... | just U' with pendingTxFitsRef true
  (refsDisjoint (utxoRefs (outputs tx)) (utxoRefs (U ∪ᵘ C)))
  (utxoCount (U' ∪ᵘ D)) (utxoCount (U' ∪ᵘ C))
...   | true = let result = prunePending U' C D txs in proj₁ result , tx ∷ proj₂ result
...   | false = prunePending U C D txs
```

== Protocol flow

=== Initializing the head

#dparagraph[$hpInit$.]#h(1em) Before a head can be initialized, all parties need
to exchange and agree on protocol parameters during the protocol setup phase
(see @sec:setup), so we can assume the public Cardano keys
$cardanoKeys^("setup")$, Hydra keys $hydraKeysAgg^("setup")$, as well as the
contestation period $Tcontest^("setup")$ are available. One of the clients then
can start head initialization using the $hpInit$ command, which will result in
an $mtxInit$ transaction being posted. Not strictly a protocol parameter, but
the deposit period $Tdeposit$ would also be made available from
configuration.\
\
#dparagraph[$mono("initialTx")$.]#h(1em) All parties will receive this $mtxInit$
transaction and validate announced parameters against the pre-agreed $italic("setup")$
parameters, as well as the structure of the transaction and the minting policy
used. This is a vital step to ensure the initialized Head is valid, which cannot
be checked completely on-chain (see also @sec:init-tx).
Importantly, parties must verify that the transaction mints the correct tokens
(one state thread token and one participation token per head member). This also
helps in to distinguish $mtxInit$ transactions from $mtxIncrement$ or
$mtxDecrement$ transactions, which may produce similar outputs without
minting.

Since the $mtxInit$ transaction directly opens the head with an empty UTxO set,
the parties initialize their local state upon observing it: the local ledger
state $hatmL = emptyset$, the seen transaction set $hatmT = emptyset$, the
seen head state version $hatv = 0$, and the snapshot number $hats = 0$. No
deposit transaction $tx_(alpha) = bot$ and no decrement transaction
$tx_(omega) = bot$ are pending, and the last confirmed snapshot is
initialized accordingly
$macron(mc(S)) <- Sno(0, 0, [], emptyset, emptyset, emptyset)$.

=== Processing transactions off-chain

Transactions are announced and captured in so-called snapshots. Parties generate
snapshots in a strictly sequential round-robin manner. The party responsible for
issuing the #ith($i$) snapshot is the _leader_ of the #ith($i$) snapshot.
Leader selection is round-robin per the $hydraKeys$ from the protocol setup.
While the frequency of snapshots in the general Head protocol~@hydrahead20
was configurable, the Coordinated Head protocol does specify a snapshot to be
created after each transaction.\
\
#dparagraph[$hpRT$.]#h(1em) Upon receiving request $(hpRT,tx)$, the transaction
is ignored if its identifier already names a locally applied transaction with a
retained registry entry; otherwise it is applied to the _local_ ledger state $hatmL applytx tx$. If not
applicable yet, the protocol does #kw("wait") to retry later or eventually marks
this transaction as invalid (see assumption about events piling up). After
applying and if there is no current snapshot in flight ($hats = macron(mc(S)).s$)
and the receiving party $party_(i)$ is the next snapshot leader, a message to
request snapshot signatures $hpRS$ is sent. \
\
#dparagraph[Bounded reqTx execution model.]#h(1em)
`OffChainReference.reqTxOutcomeRef` records the terminal result and ordered
committed-change and effect tags of one attempt. Its domain has no deposits or
decommit, no previously pending local transactions, fresh transaction output
names, and a local ledger equal to the settled confirmed ledger. Ledger
applicability is an explicit primitive answer; the model computes leader
eligibility from the party count, next
snapshot number, and local party index. A successful transaction is therefore
requestable in this domain. Payloads, collision checks, and retry scheduling are
outside this projection.

The received-transaction change survives a wait. Exhausting the retry budget
records an invalid transaction and finishes normally; it is not a rejection.
A successful attempt records application before deciding and emitting a snapshot
request, and emits no request when another snapshot is in flight. The relation
below uses the extracted computation directly, so these outcome rules have one
Agda definition. Differential tests bind this bounded result to the Haskell
handler; these lemmas do not establish equivalence of the complete node or a
refinement of the broader `_handles_↝_` relation.

```
-- The bounded duplicate policy protects validated registry payloads. This is
-- a one-attempt lemma, not a theorem about arbitrary registry histories.
reqTx-applied-is-idempotent : ∀ applicable ttl inFlight m nextSn partyIndex →
  ReqTxRef.reqTxKnownOutcomeRef true applicable ttl inFlight m nextSn partyIndex ≡
    ReqTxRef.reqTxOutcomeᶜ ReqTxRef.reqTxFinishedᶜ [] []
reqTx-applied-is-idempotent _ _ _ _ _ _ = refl

ReqTxAttempt : Bool → ℕ → Bool → ℕ → ℕ → ℕ → ReqTxRef.ReqTxOutcomeᶜ → Set
ReqTxAttempt applicable ttl inFlight m nextSn partyIndex result =
  ReqTxRef.reqTxOutcomeRef applicable ttl inFlight m nextSn partyIndex ≡ result

reqTx-wait-retains-received : ∀ ttl inFlight m nextSn partyIndex →
  ReqTxAttempt false (suc ttl) inFlight m nextSn partyIndex
    (ReqTxRef.reqTxOutcomeᶜ ReqTxRef.reqTxWaitingᶜ (ReqTxRef.reqTxReceivedᶜ ∷ []) [])
reqTx-wait-retains-received _ _ _ _ _ = refl

reqTx-timeout-finishes : ∀ inFlight m nextSn partyIndex →
  ReqTxAttempt false zero inFlight m nextSn partyIndex
    (ReqTxRef.reqTxOutcomeᶜ ReqTxRef.reqTxFinishedᶜ
      (ReqTxRef.reqTxReceivedᶜ ∷ ReqTxRef.reqTxInvalidᶜ ∷ []) [])
reqTx-timeout-finishes _ _ _ _ = refl

reqTx-inflight-does-not-propose : ∀ ttl m nextSn partyIndex →
  ReqTxAttempt true ttl true m nextSn partyIndex
    (ReqTxRef.reqTxOutcomeᶜ ReqTxRef.reqTxFinishedᶜ
      (ReqTxRef.reqTxReceivedᶜ ∷ ReqTxRef.reqTxAppliedᶜ ∷ []) [])
reqTx-inflight-does-not-propose _ _ _ _ = refl
```

#dparagraph[$hpRD$.]#h(1em) Upon receiving request $(hpRD,tx_omega)$, the
transaction is checked against the _local_ ledger state and if it is not
applicable yet or another deposit or decommit is pending still, the protocol does
#kw("wait") to retry later or eventually marks the decommit as invalid. After
applying $tx$, its outputs are removed from _local_ ledger state $hatmL$
so that they are not available any more and the decommit transaction is kept in
the local state ($tx_omega$). If there is no current snapshot in flight
($hats = macron(mc(S)).s$) and the receiving party $party_(i)$ is the next
snapshot leader, it requests useful snapshot work. A new decommit is included
only when all its spending, reference and collateral inputs are present in the
confirmed base (including a settled increment). Otherwise the ordinary
transactions confirm its dependencies first. A decommit already carried by the
same-version confirmed snapshot is carried again without reapplication. \
\
#dparagraph[$mono("depositTx")$.]#h(1em) Upon observing a deposit transaction,
each party records the deposit index by deposit transaction id to their local
deposit registry $cal(D)$ with status $"Inactive"$. The deposit
contains deposited UTxO $U$, creation time $t_("created")$, and deadline
$t_("deadline")$. A transaction is only recognised as a deposit if its inline
datum decodes as $datumDeposit$ and its declared commits total exactly the
value locked at $nuDeposit$ (the node's deposit observation,
`observeDepositTx`, drops anything else) — the observation-time gate that
discharges the off-chain obligation of @sec:deposit-tx: a malformed deposit
never enters $cal(D)$, so no honest party ever signs a snapshot claiming
it. \
\
#dparagraph[$mono("recoverTx")$.]#h(1em) Upon observing a recover transaction,
each party drops the corresponding entry from its deposit registry
$cal(D)$. \
\
#dparagraph[$mono("tick")$.]#h(1em) Whenever time advances (on-chain) to point
$t$, parties update the status of deposits in $cal(D)$ accordingly using
the configured deposit period $Tdeposit$:
- $sans("Expired")$ when deadline passed (or too soon): $t > t_("deadline") - Tdeposit$
- $sans("Active")$ when deposit settled enough: $t > t_("created") + Tactivate$
When deposits become $sans("Active")$ and no other deposit / decommit is
pending, and the party is the next snapshot leader, it may request a new
snapshot including the deposit transaction $tx_(alpha)$. \
\

The `tick` deposit-status transition is the function `depositStatusAt`,
the off-chain twin of the extracted reference's
`depositStatusRef`.

```
-- ── Chain observations (§6.4 `from chain`): the deposit lifecycle ────────────────────────────────
-- The §6 `tick` deposit-status transition for one deposit, as a function of the expiry period
-- T_deposit, the activation period T_activate and the current time t (the off-chain twin of
-- `OffChainReference.depositStatusRef`):
--   t > deadline − T_deposit ⇒ Expired;  else t > created + T_activate ⇒ Active;  else Inactive.
-- The two periods are distinct: the node keys expiry off `depositPeriod` and activation off
-- `depositActivation`, which are separately configurable, so collapsing them into one would put
-- the model's activation boundary an arbitrary distance from the real node's.
depositStatusAt : ℕ → ℕ → ℕ → DepositObj → DepositStatus
depositStatusAt Tdep Tact t D =
  if ⌊ (DepositObj.deadline D ∸ Tdep) <? t ⌋ then Expired
  else if ⌊ (DepositObj.created D + Tact) <? t ⌋ then Active
  else Inactive
```
#dparagraph[$hpRS$.]#h(1em) Upon receiving request
$(hpRS,v,s,underline(tx)_(sans("req")), tx_alpha, tx_omega)$#footnote[Snapshot
requests with only transaction identifiers and output references are possible
if all parties keep an index of previously seen transactions and their
identifiers.] from party $party_j$, the receiving $party_i$ #kw("require")s that
only a deposit or decommit may be pending, that $s$ is the next snapshot number
and that party $party_j$ is responsible for leading its creation. Party
$party_i$ may have to #kw("wait") until the previous snapshot is confirmed
($macron(cal(S)).s = hat(s)$) and until $v$ refers to the current open state
version. The version check is a #kw("wait"), not a #kw("require"): a follower
can receive the request for a bumped version before its own chain handler has
observed the transaction that bumped it, so a mismatch parks the request until
the versions agree instead of aborting the routine.
Furthermore, the protocol validates the snapshot request by:
+ Start from the last confirmed snapshot's UTxO set, folding in its pending
  commit if that commit's increment has since settled on-chain
  ($hatv > macron(mc(S)).v$)
+ If a decommit is requested: verify the transaction is applicable to the
  last confirmed UTxO set and update the active utxo set with it
+ If a decommit is requested: verify it produces at least one output,
  since $mtxDecrement$ materializes the decommit outputs and requires one
+ If a deposit is requested: verify the corresponding deposit is
  $"Active"$, record it as $U_alpha$ and verify that none of the output
  references it names is held by the active utxo set or created by the
  requested transactions; the deposit is not added to that set, since a pending
  commit only joins it once its increment has settled on-chain, so the
  requested transactions cannot spend it
+ If we are on the same version as the last snapshot, any requested
  decommit or deposit must match the last snapshot, and a decommit the last
  snapshot is still settling must be requested again: unlike a settling
  commit, it is never dropped (see below).
+ Verify all requested transactions $underline(tx)_(sans("req"))$ are
  applicable to the active UTxO set
+ Require $sans("fits")(U,U_alpha,sans("outputs")(tx_omega))$ for the resulting
  active UTxO $U$, before computing an accumulator commitment or signing.
Only then, $party_i$ increments their seen-snapshot counter $hats$, resets the
signature accumulator $hatSigma$, and computes the UTxO set of the new local
snapshot as
$U <- U_(sans("active")) applytx underline(tx)_(sans("req"))$. Then,
                #kw("require") $sans("fits")(U,U_alpha,sans("outputs")(tx_omega))$ \\\\
$party_i$ creates a signature $msSig_i$ using their signing key
$hydraSigningKey$ on a message comprised by the $cid$, the new snapshot number
$hats$, the new $eta$ resulting from canonically combining $U$ (see
@sec:close-tx for details), and either $eta_(alpha)$ or
$eta_(omega)$ derived from deposited $U_(alpha)$ or decommit transaction
$tx_(omega)$ respectively. Note that $eta_(alpha)$ covers both $U_(alpha)$ and
the deposit it comes from, identified by the pending deposit transaction
$tx_alpha$ (recorded in the snapshot as $macron(mc(S)).txOutRef_alpha$, and
bound on-chain as $txId(txOutRef_(sans("deposit")))$, see @sec:increment-tx: a
deposit is always the first output of its transaction, so its transaction
identifies it). The resulting signature therefore authorizes that one deposit
and no other recording the same UTxO. The signature is sent to all head members via
message $(hpAS,hats,msSig_i)$. Finally, the local ledger state $hatmL$ and
pending transaction set $hatmT$ get pruned by re-applying all locally pending
transactions $hatmT$ to the just requested snapshot's UTxO set iteratively.
A transaction is kept only if it applies without re-creating a held output,
avoids the carried commit's output references and fits both fanout alternatives.
Skipping a transaction leaves the running ledger unchanged and still considers
later transactions, so an independent merge can survive a capacity rejection.
The Agda `prunePending` function models this filtering; `signHonest` separately
requires `SnapshotFits`. After this rebuild,
the spending inputs of the locally pending decommit are removed again. This
reservation survives while the requested snapshot is unconfirmed, even when
that snapshot does not carry the decommit. A signed candidate alone does not
authorise releasing the reservation. An accepted request also retains its decommit
transaction in $T_(sans("all"))$ if it is not already held locally. This payload
is separate from the local reservation: `ReqSn` can arrive before `ReqDec`, or
carry a different decommit. While the candidate is collecting signatures, a
duplicate `ReqTx` cannot overwrite or evict this payload. \
\
#dparagraph[$hpAS$.]#h(1em) Upon receiving acknowledgment $(hpAS,s,msSig_j)$, all
participants #kw("require") that it is from an expected snapshot (either the last seen
$hats$ or + 1), potentially #kw("wait") for the corresponding $hpRS$ such that
$hats = s$ and #kw("require") that the signature is not yet included in $hatSigma$.
They store the received signature in the signature accumulator $hatSigma$, and
if the signature from each party has been collected, $party_i$ aggregates the
multisignature $msCSig$ and #kw("require") it to be valid (constructing the signed
message as in $hpRS$). If everything is fine, the snapshot can be considered
confirmed by creating the snapshot object
$macron(mc(S)) <- Sno(hatv, hats, hatmT, hatmU, U_(alpha), sans("outputs")(tx_(omega)))$
and storing the multi-signature $msCSig$ in it for later reference. In case
there is a pending decommit, any participant can now submit a $mtxDecrement$
transaction by providing the just confirmed snapshot with its accumulator
commitment $eta'$ for the updated UTxO set. If, however, there
was a pending deposit, any participant can now submit an $mono("incrementTx")$
by providing the confirmed snapshot with its accumulator commitment $eta'$
for the updated UTxO set. Lastly, if $party_i$ is the next snapshot
leader and there are transactions to snapshot in $hatmT$ or a newly eligible
decommit, a corresponding $hpRS$ is distributed. An already-carried decommit
alone does not start another round. \
\

For request selection, $sans("requestDecommit")(hatv, macron(mc(S)), tx_omega)$
is $tx_omega$ when it is already carried at that version or all its required
inputs are in the settled confirmed base; otherwise it is $bot$.
$sans("newDecommit")(hatv, macron(mc(S)), tx_omega)$ additionally excludes a
carried decommit. Neither test replaces ledger validation in $hpRS$.
Define $sans("reconcileReservation")$ as follows. When a snapshot is confirmed or adopted, reconstruct the optimistic ledger from
its settled base and the retained ordinary transactions, before removing any
local reservation. Keep the locally recorded $tx_omega$ if this snapshot carries
it at the current version. Otherwise keep it only if every required input remains
in that reconstructed ledger and no different decommit is authorised. If these
conditions fail, clear $tx_omega$, restore that reconstructed ledger (including
any still-unspent reserved inputs), and report `DecommitInvalid`. This step runs
on confirmation, not on signing a candidate. Required inputs include spending,
reference and collateral inputs; retaining a dependency in the ordinary sequence
therefore preserves a still-possible decommit. The next proposal uses the updated
reservation, not the pre-confirmation value. When the certificate authorises a
decommit at the current version, transfer its matching retained transaction into
$tx_omega$ and remove that registry entry. Prefer a matching local reservation;
otherwise find the payload retained from `ReqSn`. Identity here is the non-empty
output UTxO, as in the receiver's carry check; Cardano output references include
the transaction body hash. If settlement already advanced the version, remove
the retained payload without reviving the decommit.

A request at the same version must carry the authorised decommit and no deposit.
If an older checkpoint has the certificate but lacks its transaction, request
selection waits for settlement; it cannot emit a request that omits the decommit
or retain a marker for that request. The persistence encoding is unchanged:
`TransactionReceived` retains the accepted payload before `SnapshotRequested`,
and `SnapshotConfirmed` transfers it to the decommit slot.

A current-version confirmed snapshot with decommit outputs blocks a new decommit
even at a party that never received its `ReqDec`. Settlement advances the version
and retires this authority. The bounded operational reference below includes
these admission and confirmation refinements; the `_handles_↝_` relation's
`ackSn-confirm` projects the certificate update and does not model local conflict
notifications or optimistic-ledger reconstruction.
Ticks and settlement observations also request newly eligible work when this
party leads and no round is in flight.

The network-message handlers above are transcribed as the relation
`_handles_↝_`: one constructor per handler arm, each carrying
its `require`/`wait` guards from @fig:off-chain-prot as premises (the reqTx applicability guard, the
reqDec no-in-flight guards, the ackSn no-double-sign and all-signed guards, and
the reqSn version/number guards). These are normative: the machine-checked
results of @sec:security-theorems consume exactly these premises. The figure
remains the authoritative statement of the full off-chain behaviour; the
relations formalise exactly the arms those results consume - the four network
handlers here and the deposit/tick/increment/decrement/initialTx chain
observations below - while the close/contest/fanout observations and the
leader-multicast effects are not modelled at this level.

```
-- Handling a network message updates a party's local state (spec §6.4); together with `_observes_↝_`
-- (chain events) below it transcribes the §6.4 handlers, the figure (`Protocol flow`) being the rendered
-- authority. Each arm carries its §6 `require`/`wait` guards as premises (normative):
-- reqTx the applicability guard `wait L̂ ∘ tx ≠ ⊥` (de-abstracted over `applyTxs`) + ledger update;
-- reqDec its no-in-flight + applicability guard (`U_α = ∅ ∧ tx_ω = ⊥ ∧ L̂ ∘ tx ≠ ⊥`) + ledger update;
-- reqSn-sign the version/number guards (§7);
-- ackSn-collect the no-double-sign guard (`(j,·) ∉ Σ̂`); ackSn-confirm the n-of-n all-signed guard. The
-- ackSn-confirm multisignature VERIFY is the operational realisation of all-signed and is formalised at
-- the §7 `confirm` step (`AggVerified`), not duplicated here; the leader-snapshot multicast EFFECTS are
-- likewise deferred to the §7 system wiring.
data _handles_↝_ : LocalState → Message → LocalState → Set where
  -- on (reqTx, tx): `wait L̂ ∘ tx ≠ ⊥` (tx applies to the local ledger), then `L̂ ← L̂ ∘ tx` and
  -- `T̂ ← T̂ ∪ {tx}`. The applicability guard is de-abstracted as the witness `applyTxs L̂ [tx] ≡ just L'`,
  -- which also supplies the updated ledger L'. (The leader-snapshot multicast is a deferred effect.)
  reqTx-pending : ∀ {st tx L'}
    → applyTxs (LocalState.localLedger st) (tx ∷ []) ≡ just L'
    → st handles (reqTx tx) ↝ record st { localLedger = L' ; pending = LocalState.pending st ++ (tx ∷ []) }

  -- on (reqDec, tx): the §6 `wait U_α = ∅ ∧ tx_ω = ⊥ ∧ L̂ ∘ tx ≠ ⊥` guard (no commit/decommit in flight,
  -- and tx applies), all three de-abstracted as premises; then `L̂ ← L̂ ∘ tx ∖ outputs(tx)` and tx_ω ← tx.
  --
  -- NB this rule reads `U_α = ∅` as "no deposit tx-id is recorded at all", which is the clean case and
  -- a SUFFICIENT condition, not a characterisation. The implementation additionally proceeds when a
  -- tx-id IS recorded but names nothing the head can still settle, because tx_α is cleared on neither
  -- expiry nor recovery: an Expired deposit stays in 𝒟 so it can still be recovered, and a recovered
  -- one only leaves 𝒟. Treating either as a commit in flight would block every later decommit on
  -- something no increment can ever settle. `Hydra.Protocol.OffChainReference.reqDecEligibleRef` models
  -- that refinement (its `PendingCommitᶜ` separates the four cases) and the off-chain differential
  -- binds it to the node.
  --
  -- Consequence worth stating plainly: `NoBothInFlight` below is therefore STRONGER than what the
  -- implementation guarantees, since a recorded-but-unsettleable tx_α can coexist with a pending tx_ω.
  -- Restating it over settleable commits means weakening a §7 safety invariant and adding a freshness
  -- premise to `deposit-add`, which is deliberately left as its own change rather than folded in here.
  reqDec-pending : ∀ {st tx L'}
    → LocalState.currentDepositTxId st ≡ nothing         -- U_α = ∅ (no commit in flight)
    → LocalState.pendingDecrement st ≡ nothing       -- no local reservation
    → (¬ (Snapshot.version (LocalState.confirmed st) ≡ LocalState.seenVersion st)) ⊎ (Snapshot.utxoDec (LocalState.confirmed st) ≡ εᵘ) -- no unsettled decommit certificate
    → applyTxs (LocalState.localLedger st) (tx ∷ []) ≡ just L'   -- L̂ ∘ tx ≠ ⊥ (applicability, witnessed)
    → DecommitFits st tx L'                         -- non-empty outputs; local AND confirmed capacity
    → st handles (reqDec tx) ↝ record st { localLedger = L' ∖ᵘ outputs tx ; pendingDecrement = just tx }

  -- on (ackSn, s, σ) before the round completes: record sender j's signature in Σ̂; S̄ unchanged. The §6
  -- `require (j,·) ∉ Σ̂` guard (sender j has not already signed this round) is de-abstracted as the
  -- premise, so Σ̂ never double-counts a party.
  ackSn-collect : ∀ {st s σ j}
    → ¬ (j ∈ˡ map proj₁ (LocalState.seenSigs st))   -- (j,·) ∉ Σ̂ (sender has not signed yet)
    → st handles (ackSn s σ) ↝ record st { seenSigs = (j , σ) ∷ LocalState.seenSigs st }

  -- on (ackSn, s, σ) completing the round: every party has signed, so the seen snapshot becomes the new
  -- confirmed S̄ (txs = T̂, number = ŝ). The §6 `if ∀ k ∈ [1..n] : (k,·) ∈ Σ̂` all-signed guard is
  -- de-abstracted as the `allSigned` premise: the n-of-n condition (every party index < n is recorded
  -- in Σ̂), the SEMANTIC content the §7 `Certified` predicate captures (no tx confirmed unless every
  -- honest party signed it). The figure's operational `require msVfy(…, msComb Σ̂)` over the COMBINED
  -- multisignature is the implementation of that n-of-n condition (via ms-unforgeability); it is
  -- formalised precisely at the §7 `confirm` step (`AggVerified`/`msVfy`), so it is not duplicated here.
  ackSn-confirm : ∀ {st s σ snap}
    → (∀ {k} → k < HeadParameters.n (LocalState.params st) → k ∈ˡ map proj₁ (LocalState.seenSigs st))  -- ∀k:(k,·)∈Σ̂
    → Snapshot.txs snap ≡ LocalState.pending st          -- S̄'.T = T̂
    → Snapshot.number snap ≡ LocalState.seenNumber st    -- S̄'.s = ŝ
    → (LocalState.seenVersion st ≡ Snapshot.version snap) ⊎ (LocalState.seenVersion st ≡ suc (Snapshot.version snap))  -- v̂ = S̄'.v or S̄'.v+1: a snapshot is confirmed at the current or one-prior version (supports `VersionDiscipline`; the v̂ = S̄'.v+1 case is a snapshot signed before its increment/decrement version bump was observed)
    → st handles (ackSn s σ) ↝ record st { confirmed = snap }

  -- on (reqSn, v, s, …): the honest snapshot-leader's request triggers a party to sign (§6.4
  -- onOpenNetworkReqSn). The `require` guards captured here: v ≡ v̂ (version matches) and s ≡ s̄+1
  -- (the requested number is one above the party's CONFIRMED snapshot - combined with the §7
  -- `signHonest` no-in-flight precondition ŝ = s̄, this is the node's `requireReqSn` s = ŝ+1 /
  -- `waitNoSnapshotInFlight`). Signing advances the last-seen number ŝ ← s, the local mutation that
  -- makes one-signature-per-round (`sigDedup`) DERIVABLE. The requested-tx applicability and the
  -- resulting snapshot's tx set are handled at the §7 signing step (they need U₀ / the ledger laws).
  reqSn-sign : ∀ {st v s txReq txα txω}
    → v ≡ LocalState.seenVersion st
    → s ≡ suc (Snapshot.number (LocalState.confirmed st))
    → st handles (reqSn v s txReq txα txω) ↝ record st { seenNumber = s }
```
#dparagraph[$mono("decrementTx")$.]#h(1em) Upon observing the $mtxDecrement$
transaction, which removed outputs $U$ from the head, the corresponding pending
decrement transaction is cleared and the observed version $v$ is used for future
snapshots by setting $hatv <- v$. Note that the version of the open head state
is incremented on each $mtxDecrement$ transaction as described in
@sec:decrement-tx. \
\
#dparagraph[$mono("incrementTx")$.]#h(1em) Upon observing the $mtxIncrement$
transaction, which added outputs $U$ to the head, the local ledger state
$hatmL$ is extended with the newly added UTxO while the pending increment
state $U_(alpha)$ is cleared. Also the observed version $v$ is used for future
snapshots by setting $hatv = v$. Note that the version of the open head state
is incremented on each $mtxIncrement$ transaction as described in
@sec:increment-tx. \
\
#dparagraph[Adopting a settled snapshot.]#h(1em) The redeemer of an
$mtxIncrement$ or $mtxDecrement$ transaction carries the number $s$ and the
multi-signature $xi$ of the snapshot it settles. A party may see such a
transaction while it is still collecting signatures for that very snapshot
($s = hats = macron(mc(S)).s + 1$): another party can complete the
multi-signature locally and post the transaction without ever sending its own
$hpAS$. If $xi$ verifies under $hydraKeysAgg$ over the message of the seen
snapshot, the party adopts the seen snapshot as confirmed with $xi$ as its
certificate, exactly as if the last $hpAS$ had arrived (routine $sans("adopt")$
in @fig:off-chain-prot). This check is against
the party's own seen snapshot, so a transaction settling any other snapshot
leaves $macron(mc(S))$ unchanged.

Chain observations are transcribed likewise: `ChainEvent` enumerates the
modelled chain observations and `_observes_↝_` gives one constructor per
handler of the figure - the deposit lifecycle above, the version-bumping
increment/decrement observations (plain, and adopting the settled snapshot),
and the head-opening `initialTx` of the Initializing-the-head paragraph.

```
-- Chain events a party observes: the deposit lifecycle (deposit/recover/tick), the version-bumping
-- increment/decrement observations, and the head-opening initialTx. Close/contest/fanout
-- observations are not modelled (see the scope note above). Increment and decrement carry the
-- number s and the aggregate multisignature ξ of the snapshot they settle, from their redeemer.
data ChainEvent : Set where
  depositTx : (txα : Data) (U : UTxO) (created deadline : ℕ) → ChainEvent  -- on (depositTx, …)
  recoverTx : (txα : Data)                                   → ChainEvent  -- on (recoverTx, txα)
  tick      : (t : ℕ)                                        → ChainEvent  -- on (tick, t)
  incrementTx : (U : UTxO) (v s : ℕ) (ξ : AggSig)            → ChainEvent  -- on (incrementTx, U, v, s, ξ)
  decrementTx : (U : UTxO) (v s : ℕ) (ξ : AggSig)            → ChainEvent  -- on (decrementTx, U, v, s, ξ)
  initialTx   :                                                ChainEvent  -- on (initialTx, …): head opens

-- The message a snapshot's aggregate multisignature is verified against, cid‖v‖s‖η#‖η̂#‖δ#‖κ#: the
-- same concatenation as §7's `msgOf`, needed here already for the adopting observations below.
signedMessage : Snapshot → ℍ
signedMessage snap =
  Snapshot.cid snap ‖ Snapshot.version snap ‖ Snapshot.number snap ‖ Snapshot.etaHash snap
    ‖ Snapshot.appliedEtaHash snap ‖ Snapshot.decHash snap ‖ Snapshot.comHash snap

-- The seen snapshot is the one in flight (ŝ = S̄.s + 1), `snap` is it (its txs are T̂ and its number
-- ŝ), the observed transaction settled exactly it (s = ŝ), and ξ verifies over it under the head's
-- aggregate key. Then the transaction proves `snap` fully signed, whether or not every AckSn arrived.
SettlesSeen : LocalState → ℕ → AggSig → Snapshot → Set
SettlesSeen st s ξ snap =
    (LocalState.seenNumber st ≡ suc (Snapshot.number (LocalState.confirmed st)))
  × (s ≡ LocalState.seenNumber st)
  × (Snapshot.number snap ≡ s)
  × (Snapshot.txs snap ≡ LocalState.pending st)
  × (msVfy (HeadParameters.aggregateKey (LocalState.params st)) (signedMessage snap) ξ ≡ true)

-- Observing a chain event updates a party's deposit registry 𝒟 (and, for `tick`, every entry's
-- status). `recover` removes a WITNESSED occurrence, so no decidable Data-equality is assumed.
-- Mirrors the figure's deposit / recover / tick handlers; the leader re-trigger `multicast` effect is
-- modelled when these events are lifted into the §7 system (as the existing network arms are).
data _observes_↝_ : LocalState → ChainEvent → LocalState → Set where
  deposit-add : ∀ {st txα U c d}
    → st observes (depositTx txα U c d)
        ↝ record st { deposits = (txα , depositObj U c d Inactive) ∷ LocalState.deposits st }

  recover-del : ∀ {st txα D before after}
    → LocalState.deposits st ≡ before ++ ((txα , D) ∷ after)
    → st observes (recoverTx txα) ↝ record st { deposits = before ++ after }

  tick-update : ∀ {st t}
    → st observes (tick t)
        ↝ record st
            { deposits =
                map (λ kD → proj₁ kD ,
                       record (proj₂ kD)
                         { status = depositStatusAt (HeadParameters.depositPeriod (LocalState.params st))
                                                        (HeadParameters.depositActivation (LocalState.params st))
                                                        t (proj₂ kD) })
                    (LocalState.deposits st) }

  -- §6 chain observations that bump the open-state version (increment/decrement). Increment adds the
  -- committed UTxO to the local ledger (`L̂ ← L̂ ∪ U`); decrement leaves L̂ (its outputs left earlier, at
  -- the decommit request). Each bumps v̂ and clears the matching in-flight pending (tx_α / tx_ω).
  -- NB the seen number ŝ is PRESERVED, not reset to S̄.s: an unconditional `ŝ ← S̄.s` would
  -- drop ŝ below an in-flight signature's number and break `signNumBound` (§7). The §6 figure, this
  -- model and the node agree: the seen snapshot is carried across the version bump (the node keeps an
  -- in-flight `SeenSnapshot`, a no-op on ŝ given the invariant ŝ ∈ {s̄, s̄+1}).
  increment-obs : ∀ {st U v s ξ}
    → v ≡ suc (Snapshot.version (LocalState.confirmed st))   -- authorize-then-bump: v = S̄.v + 1 (the increment is signed at the confirmed snapshot's version); supports `VersionDiscipline`
    → st observes (incrementTx U v s ξ)
        ↝ record st { seenVersion = v ; currentDepositTxId = nothing ; localLedger = LocalState.localLedger st ∪ᵘ U }

  decrement-obs : ∀ {st U v s ξ}
    → v ≡ suc (Snapshot.version (LocalState.confirmed st))   -- as increment: v = S̄.v + 1 (authorize-then-bump)
    → st observes (decrementTx U v s ξ)
        ↝ record st { seenVersion = v ; pendingDecrement = nothing }

  -- The same observations settling the snapshot still in flight: besides the plain effect, the seen
  -- snapshot becomes S̄ with the observed ξ, as if the last AckSn had arrived. The version is the one
  -- after that snapshot's own settlement (v = snap.v + 1), which keeps `VersionDiscipline`. The §7
  -- system lifts this as the plain observation plus a `confirm` of `snap` (its aggregate verifies).
  increment-adopt : ∀ {st U v s ξ snap}
    → v ≡ suc (Snapshot.version snap)
    → SettlesSeen st s ξ snap
    → st observes (incrementTx U v s ξ)
        ↝ record st { seenVersion = v ; currentDepositTxId = nothing ; localLedger = LocalState.localLedger st ∪ᵘ U
                    ; confirmed = snap }

  decrement-adopt : ∀ {st U v s ξ snap}
    → v ≡ suc (Snapshot.version snap)
    → SettlesSeen st s ξ snap
    → st observes (decrementTx U v s ξ)
        ↝ record st { seenVersion = v ; pendingDecrement = nothing ; confirmed = snap }

  -- on (initialTx, …): the head opens with a genesis confirmed snapshot (number/version 0, no txs) and
  -- an empty local ledger (`L̂ ← ∅`); the §6 setup `require`s stay abstracted (genesis is given here,
  -- like ackSn-confirm). The deposit registry 𝒟 is NOT reset: the figure leaves it untouched and the
  -- node carries `pendingDeposits` through head-open unchanged.
  initialTx-obs : ∀ {st genesis}
    → Snapshot.version genesis ≡ 0
    → Snapshot.number  genesis ≡ 0
    → Snapshot.txs     genesis ≡ []
    → st observes initialTx
        ↝ record st { seenVersion = 0 ; seenNumber = 0 ; pending = [] ; confirmed = genesis
                    ; currentDepositTxId = nothing ; pendingDecrement = nothing ; localLedger = εᵘ }
```

```
-- A tick only recomputes statuses: it never adds or drops a deposit (the registry length is preserved).
tick-preserves-deposits-length : ∀ {st t st'}
  → st observes (tick t) ↝ st'
  → length (LocalState.deposits st') ≡ length (LocalState.deposits st)
tick-preserves-deposits-length {st = st} tick-update = length-map _ (LocalState.deposits st)

-- The de-abstracted reqDec guard is load-bearing: reqDec fires ONLY from a no-decommit-in-flight state
-- (tx_ω = ⊥) and results in exactly the requested decommit (so it never silently overwrites an
-- in-flight one). Deleting the `pendingDecrement ≡ nothing` premise on `reqDec-pending` makes the first
-- conjunct underivable (an adversarial check that the guard carries weight).
reqDec-starts-decommit : ∀ {st tx st'}
  → st handles (reqDec tx) ↝ st'
  → (LocalState.pendingDecrement st ≡ nothing) × (LocalState.pendingDecrement st' ≡ just tx)
reqDec-starts-decommit (reqDec-pending _ q _ _ _) = q , refl

-- The de-abstracted reqTx guard is load-bearing too: reqTx fires only for transactions that APPLY to
-- the local ledger (the §6 `wait L̂ ∘ tx ≠ ⊥`). Deleting the `applyTxs … ≡ just L'` witness premise on
-- `reqTx-pending` makes this underivable.
reqTx-applicable : ∀ {st tx st'}
  → st handles (reqTx tx) ↝ st'
  → Applicable (LocalState.localLedger st) (tx ∷ [])
reqTx-applicable (reqTx-pending eq) p with trans (sym eq) p
... | ()

-- The ackSn guards are load-bearing too. ackSn-collect records a signature only for a sender that is
-- NOT already in Σ̂ (so Σ̂ never double-counts a party):
ackSn-collect-fresh : ∀ {st s σ j}
  → st handles (ackSn s σ) ↝ record st { seenSigs = (j , σ) ∷ LocalState.seenSigs st }
  → ¬ (j ∈ˡ map proj₁ (LocalState.seenSigs st))
ackSn-collect-fresh (ackSn-collect p) = p

-- ackSn-confirm adopts a snapshot only once EVERY party (index < n) has signed (the n-of-n condition
-- the §7 `Certified` predicate captures):
ackSn-confirm-allSigned : ∀ {st s σ snap}
  → st handles (ackSn s σ) ↝ record st { confirmed = snap }
  → ∀ {k} → k < HeadParameters.n (LocalState.params st) → k ∈ˡ map proj₁ (LocalState.seenSigs st)
ackSn-confirm-allSigned (ackSn-confirm allSigned _ _ _) = allSigned
```

=== Closing the head

#dparagraph[$hpClose$.]#h(1em) In order to close a head, a client issues the
$hpClose$ input which uses the latest confirmed snapshot $macron(mc(S))$ to
construct the accumulator commitment $eta'$ for the closing UTxO state
and the certificate $xi$ using the corresponding multi-signature. With these, the $mtxClose$ transaction
can be constructed and posted. See @sec:close-tx for details about this
transaction. \
\
#dparagraph[$mono("closeTx")\/mono("contestTx")$.]#h(1em) When a party observes
the head getting closed or contested, the $eta$-state extracted from the
$mtxClose$ or $mtxContest$ transaction represents the latest head status that
has been aggregated on-chain so far (by a sequence of $mtxClose$ and
$mtxContest$ transactions). If the last confirmed (off-chain) snapshot is newer
than the observed (on-chain) snapshot number $s_(c)$, an updated accumulator
commitment $eta'$ and certificate $xi$ are constructed and posted in a $mtxContest$ transaction (see
@sec:contest-tx).

Like increments and decrements, both transactions carry the multi-signature
$xi$ of the snapshot they use, and a head can be closed or contested with the
snapshot a party is still collecting signatures for. The party adopts it as
confirmed under the same condition as for an increment ($s_c = hats = macron(mc(S)).s + 1$
and $xi$ verifies over the seen snapshot's message), so that it can fan out the
head. A party keeps its seen snapshot while the head is closed: a head closed
with an older snapshot can still be contested with the seen one later. This
adoption is not part of the handler relation `_observes_↝_` above, which does
not model the closed head, but the adopted snapshot's aggregate verifies, so it
is an instance of the `confirm` step of @sec:security.

#dparagraph[$mono("fanoutTx")$.]#h(1em) Upon observing a $mtxFanout$
transaction, all UTxOs are distributed and the head transitions to $stFinal$ state.

#dparagraph[$mono("partialFanoutTx")$.]#h(1em) When a head needs to be finalized,
the node first attempts to post a single $mtxFanout$ transaction distributing all UTxOs at once.
If the full UTxO set does not fit within the transaction size or script execution budget, the node
falls back to partial fanout: it performs a binary search over batch sizes to find the largest
batch that fits within protocol limits, minimising the total number of fanout steps. Concretely, for
a remaining set of $N$ UTxOs, it searches $[1, N-1]$ using ceiling-division midpoints (biasing
toward larger batches) and selects the largest $n$ for which the transaction satisfies both the size
limit and the script execution budget.

A batch of size $n < N$ is posted as an intermediate $mtxPartialFanout$ transaction, which
distributes $n$ UTxOs, updates the accumulator commitment to cover only the remaining $N - n$
UTxOs, and transitions the head to the $stFanoutProgress$ state. The procedure repeats: after
each $mtxPartialFanout$ observation the node again attempts to finalize all remaining UTxOs in
one step; if they still do not fit, another binary search determines the next batch size.
$mtxFinalPartialFanout$ is used for the last batch when all remaining UTxOs fit in a single
transaction, burning all head tokens and transitioning to $stFinal$.

== Machine-checked handler invariants <sec:offchain-theorems>

Two `require` disciplines of @fig:off-chain-prot — commit/decommit
_exclusivity_ (at most one of a pending deposit $tx_alpha$ or a pending
decommit $tx_omega$ in flight, the ReqSn handler's
#kw("require") $tx_omega = bot or tx_alpha = bot$) and the _version
discipline_ (the seen open-state version $hatv$ is the confirmed snapshot's
version or exactly one above) — are not merely transcribed as handler guards
but proved to be maintained by every off-chain step. A step of the handler
model is a network-message handling or a chain observation (`_⟶ᴴ_`), and
`Reachableᴴ` closes it reflexively-transitively from a given start state.

```
-- One off-chain step: handle a network message OR observe a chain event.
data _⟶ᴴ_ : LocalState → LocalState → Set where
  hstep : ∀ {st m st'} → st handles m  ↝ st' → st ⟶ᴴ st'
  ostep : ∀ {st e st'} → st observes e ↝ st' → st ⟶ᴴ st'

data Reachableᴴ (init : LocalState) : LocalState → Set where
  base : Reachableᴴ init init
  step : ∀ {st st'} → Reachableᴴ init st → st ⟶ᴴ st' → Reachableᴴ init st'
```

#invariant(name: "Handler-model safety disciplines")[
  Along any off-chain run from a state satisfying them (such as the state
  produced by observing `initialTx`, where both pending slots are empty): (i) at
  most one of a pending commit ($tx_alpha$) or a pending decommit ($tx_omega$)
  is ever in flight, the figure's `require` $tx_omega = bot or tx_alpha = bot$
  discipline (`NoBothInFlight`); and (ii) the seen open-state version $hatv$ is
  the confirmed snapshot's version or exactly one above (`VersionDiscipline`).
  The system-level versions over multi-party adversarial executions,
  `noBothInFlightˢ` and `versionDisciplineˢ`, are stated with the
  machine-checked results of @sec:security-theorems (@agda-appendix).
] <inv:handler-disciplines>

`NoBothInFlight` is the deposit/decommit exclusivity. Its preservation rests on
reqDec's de-abstracted `U_α = ∅ ∧ tx_ω = ⊥` guard, which re-establishes the
invariant; the increment, decrement and initialTx observations clear a pending
slot; every other step leaves both slots untouched. Deleting the
`pendingDecrement ≡ nothing` premise on `reqDec-pending` makes the proof fail,
which is the adversarial check that the guard carries weight.

```agda
NoBothInFlight : LocalState → Set
NoBothInFlight st = (LocalState.currentDepositTxId st ≡ nothing) ⊎ (LocalState.pendingDecrement st ≡ nothing)
```

```
noBothInFlight-step : ∀ {st st'} → st ⟶ᴴ st' → NoBothInFlight st → NoBothInFlight st'
```

```
noBothInFlight-step (hstep (reqTx-pending _))     inv = inv
noBothInFlight-step (hstep (reqDec-pending p _ _ _ _)) _  = inj₁ p
noBothInFlight-step (hstep (ackSn-collect _))     inv = inv
noBothInFlight-step (hstep (ackSn-confirm _ _ _ _)) inv = inv
noBothInFlight-step (hstep (reqSn-sign _ _))      inv = inv
noBothInFlight-step (ostep deposit-add)           inv = inv
noBothInFlight-step (ostep (recover-del _))       inv = inv
noBothInFlight-step (ostep tick-update)           inv = inv
noBothInFlight-step (ostep (increment-obs _))      _  = inj₁ refl
noBothInFlight-step (ostep (decrement-obs _))      _  = inj₂ refl
noBothInFlight-step (ostep (increment-adopt _ _))  _  = inj₁ refl
noBothInFlight-step (ostep (decrement-adopt _ _))  _  = inj₂ refl
noBothInFlight-step (ostep (initialTx-obs _ _ _))  _  = inj₁ refl
```

Hence the discipline holds throughout any off-chain run whose start state
satisfies it.

```agda
noBothInFlight-reachable : ∀ {init st} → NoBothInFlight init → Reachableᴴ init st → NoBothInFlight st
```

```
noBothInFlight-reachable inv base       = inv
noBothInFlight-reachable inv (step r s) = noBothInFlight-step s (noBothInFlight-reachable inv r)
```

`VersionDiscipline` makes the close/contest version reuse well-formed: the node
reuses the open-state version $hatv$ when posting a contest, which is meaningful
only if the confirmed snapshot is at the current or the immediately-prior
version (it then posts the $hatv$ or $hatv - 1$ signature). Preservation rests
on the on-chain authorize-then-bump rule, captured as the increment/decrement
observations' premise that a version bump is signed at the confirmed snapshot's
version (so $hatv$ never runs more than one version ahead), while `ackSn-confirm`
re-establishes the discipline from its own version premise, and the adopting
increment/decrement observations from theirs (the observed version is one above
the adopted snapshot's). This turns an
otherwise unchecked runtime assumption of the implementation into a theorem of
the off-chain state machine.

```agda
VersionDiscipline : LocalState → Set
VersionDiscipline st =
    (LocalState.seenVersion st ≡ Snapshot.version (LocalState.confirmed st))
  ⊎ (LocalState.seenVersion st ≡ suc (Snapshot.version (LocalState.confirmed st)))
```

```
versionDiscipline-step : ∀ {st st'} → st ⟶ᴴ st' → VersionDiscipline st → VersionDiscipline st'
```

```
versionDiscipline-step (hstep (reqTx-pending _))       inv = inv
versionDiscipline-step (hstep (reqDec-pending _ _ _ _ _))  inv = inv
versionDiscipline-step (hstep (ackSn-collect _))       inv = inv
versionDiscipline-step (hstep (ackSn-confirm _ _ _ d)) _   = d
versionDiscipline-step (hstep (reqSn-sign _ _))        inv = inv
versionDiscipline-step (ostep deposit-add)             inv = inv
versionDiscipline-step (ostep (recover-del _))         inv = inv
versionDiscipline-step (ostep tick-update)             inv = inv
versionDiscipline-step (ostep (increment-obs e))       _   = inj₂ e
versionDiscipline-step (ostep (decrement-obs e))       _   = inj₂ e
versionDiscipline-step (ostep (increment-adopt e _))   _   = inj₂ e
versionDiscipline-step (ostep (decrement-adopt e _))   _   = inj₂ e
versionDiscipline-step (ostep (initialTx-obs eq _ _))  _   = inj₁ (sym eq)
```

```agda
versionDiscipline-reachable : ∀ {init st} → VersionDiscipline init → Reachableᴴ init st → VersionDiscipline st
```

```
versionDiscipline-reachable inv base       = inv
versionDiscipline-reachable inv (step r s) = versionDiscipline-step s (versionDiscipline-reachable inv r)
```

== Rollbacks and protocol changes <sec:rollbacks>
#todo[Explain why rollbacks are no problem to increment/decrement]
#todo[Write about contestation deadline vs. rollbacks]

The overall life-cycle of the Head protocol is driven by on-chain inputs (see
introduction of @sec:offchain) which stem from observing
transactions on the mainchain. Most blockchains, however, do only provide
_eventual_ consistency. The consensus algorithm ensures a consistent view
of the history of blocks and transactions between all parties, but this
so-called _finality_ is only achieved after some time and the local view of
the blockchain history may change until that point.

On Cardano with it's Ouroboros consensus algorithm, this means that any local
view of the mainchain may not be the longest chain and a node may switch to a
longer chain, onto another fork. This other version of the history may not
include what was previously observed and hence, any tracking state needs to be
updated to this "new reality". Practically, this means that an observer of the
blockchain sees a _rollback_ followed by rollforwards.

For the Head protocol, this means that chain events like $mono("closeTx")$ may
be observed a second time. Hence, it is crucial, that the local state of the
Hydra protocol is kept in sync and also rolled back accordingly to be able to
observe and react to these events the right way, e.g. correctly contesting this
$mono("closeTx")$ if need be.

The rollback handling can be specified fully orthogonal on top of the nominal
protocol behavior, if the chain provides strictly monotonically increasing
points $p$ on each chain event via a new or wrapped $mono("rollforward")$
event and $mono("rollback")$ event with the point to which a rollback happened:\
\
#dparagraph[$mono("rollforward")$.]#h(1em) On every chain event that is paired or
wrapped in a rollforward event $(mono("rollback"),p)$ with point $p$, protocol
participants store their head state indexed by this point in a history
$Omega$ of states $Delta <- (hatv, hats, hatmU, hatSigma, hatmL, hatmT, macron(mc(S)))$ and $Omega' = (p, Delta) union Omega$. \
\
#dparagraph[$mono("rollback")$.]#h(1em) On a rollback
$(mono("rollback"),p_(italic("rb")))$ to point $p_(italic("rb"))$, the corresponding head state
$Delta$ need to be retrieved from $Omega$, with the maximal point
$p <= p_(italic("rb"))$, and all entries in $Omega$ with $p > p_(italic("rb"))$ get removed. \
\
This will essentially reset the local head state to the right point and allow
the protocol to progress through the life-cycle normally. Most stages of the
life-cycle are unproblematic if they are rolled back, as long as the protocol
logic behaves as in the nominal case.

Since the head is directly opened by the $mtxInit$ transaction, every rollback
of on-chain state is effectively a rollback "past open". However, because the
head opens with an empty UTxO set and funds are only added via deposits, the
critical concern becomes rollbacks of deposit transactions. The deposit
settlement period $Tdeposit$ (see @sec:offchain) ensures that a
deposit is only considered $sans("Active")$ — and thus eligible for
incrementing into the head — after sufficient time has elapsed since its
creation on-chain. This means that by the time a deposit is incremented, its
on-chain transaction is sufficiently settled and unlikely to be rolled back,
preventing inconsistency between the on-chain and off-chain state.

#figure(
  {
    set text(size: 6.5pt)
    set par(justify: false, leading: 0.5em)
    show math.equation: set text(size: 6.5pt)
    align(left)[
      #algobox("Coordinated Hydra Head")[
        #grid(
          columns: (1fr, 1fr),
          column-gutter: 1em,
          [
            #proc([#kw("on") $(hpInit)$ #kw("from") client])[
              $n <- |hydraKeys^("setup")|$ \
              $hydraKeysAgg <- msCombVK(hydraKeys^("setup"))$ \
              $cardanoKeys <- cardanoKeys^("setup")$ \
              $Tcontest <- Tcontest^("setup")$ \
              $Tdeposit <- Tdeposit^("setup")$ \
              #kw("postTx") $(mtxInit, nop, hydraKeysAgg, cardanoKeys, Tcontest)$
            ]
          ],
          [
            #proc([#kw("on") $(gcChainInitial, cid, seed, nop, hydraKeysAgg, cardanoKeys^(\#), Tcontest)$ #kw("from") chain])[
              #kw("require") $hydraKeysAgg = msCombVK(hydraKeys^("setup"))$ \
              #kw("require") $cardanoKeys^(\#) = [hash(k) | forall k in cardanoKeys^("setup")]$ \
              #kw("require") $Tcontest = Tcontest^("setup")$ \
              #kw("require") $cid = hash(muHead(seed))$ \
              $hatmL <- emptyset$ \
              $macron(mc(S)) <- Sno(0, 0, [], emptyset, emptyset, emptyset)$ \
              $hatv, hats <- 0$ \
              $hatmT <- emptyset$ \
              $tx_omega <- bot$ \
              $tx_alpha <- bot$
            ]
          ],
        )

        #line(length: 100%, stroke: 0.4pt + luma(120))
        #v(0.2em)

        #grid(
          columns: (1fr, 1fr),
          column-gutter: 1em,
          [
            #proc([#kw("on") $(hpRT, tx)$ #kw("from") $party_j$])[
              #kw("wait") $hatmL applytx tx != bot and sans("outputs")(tx) inter (hatmL union union.big_(D in mc(D), D.sans("status") != sans("Expired") or D = mc(D)[macron(mc(S)).tx_alpha]) D.U) = emptyset$
              #nst[
                $hatmL <- hatmL applytx tx$ \
                $hatmT <- hatmT union {tx}$ \
                #kw("if") $hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i$
                #nst[
                  #kw("if") $tx_alpha = bot and tx_omega = bot and macron(mc(S)).U_alpha = emptyset$
                  #nst[
                    $tx_alpha <-$ oldest $D in mc(D)$ with $D.sans("status") = sans("Active") and D.U inter macron(mc(S)).U = emptyset and D.U inter sans("outputs")(hatmT) = emptyset and sans("depositFits")(D)$
                  ]
                  #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, sans("requestable")(hatmT, tx_alpha, sans("requestDecommit")(hatv, macron(mc(S)), tx_omega)), tx_alpha, sans("requestDecommit")(hatv, macron(mc(S)), tx_omega))$
                ]
              ]
            ]

            #proc([#kw("on") $(hpRD, tx)$ #kw("from") $party_j$])[
              #kw("wait") $U_alpha = emptyset and tx_omega = bot and (macron(mc(S)).v != hatv or macron(mc(S)).U_omega = bot) and hatmL applytx tx != bot$
              #nst[
                #kw("require") $sans("outputs")(tx) != emptyset and |hatmL applytx tx| <= 32767 and |sans("applyUTxO")(sans("settled")(macron(mc(S)),hatv),tx)| <= 32767$ \

                $hatmL <- hatmL applytx tx without sans("outputs")(tx)$ \
                $tx_omega <- tx$ \
                #kw("if") $hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i$
                #nst[
                  #kw("if") $tx_alpha != bot$ #text(size: 0.8em)[(a settling commit is carried on; the decommit waits for it)]
                  #nst[
                    #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, sans("requestable")(hatmT, tx_alpha, bot), tx_alpha, bot)$
                  ]
                  #kw("else if") $hatmT != emptyset or sans("newDecommit")(hatv, macron(mc(S)), tx_omega)$
                  #nst[
                    #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, sans("requestable")(hatmT, bot, sans("requestDecommit")(hatv, macron(mc(S)), tx_omega)), bot, sans("requestDecommit")(hatv, macron(mc(S)), tx_omega))$
                  ]
                ]
              ]
            ]

            #proc([#kw("on") $(hpRS, v, s, underline(tx)_(sans("req")), tx_alpha, tx_omega)$ #kw("from") $party_j$])[
              #kw("require") $s = hats + 1 and hpLdr(s) = j$ \
              #kw("wait") $hats = macron(mc(S)).s and v = hatv$
              #nst[
                #kw("require") $tx_omega = bot or tx_alpha = bot$ \
                $U_(sans("active")) <- macron(mc(S)).U$ \
                #kw("if") $hatv > macron(mc(S)).v and macron(mc(S)).U_alpha != bot$
                #nst[
                  $U_(sans("active")) <- U_(sans("active")) union macron(mc(S)).U_alpha$
                ]
                #kw("if") $v = macron(mc(S)).v and macron(mc(S)).U_omega != bot$
                #nst[
                  #kw("require") $tx_omega != bot and macron(mc(S)).U_omega = sans("outputs")(tx_omega)$ #text(size: 0.8em)[(a settling decommit is carried by every request at this version)]
                ]
                #kw("else if") $tx_omega != bot$
                #nst[
                  #kw("require") $U_(sans("active")) applytx tx_omega != bot$ \
                  $U_(sans("active")) <- U_(sans("active")) applytx tx_omega without sans("outputs")(tx_omega)$
                ]
                #kw("if") $tx_alpha = bot and v = macron(mc(S)).v and macron(mc(S)).U_alpha != bot$
                #nst[
                  #kw("wait") $sans("unclaimable")(macron(mc(S)).tx_alpha)$ #text(size: 0.8em)[(a settling commit is dropped only once its increment can no longer land)]
                ]
                #kw("if") $tx_alpha != bot$
                #nst[
                  $mc(D) <- mc(D)[tx_alpha]$ \
                  #kw("wait") $mc(D).sans("status") != sans("Inactive")$
                  #nst[
                    #kw("if") $v = macron(mc(S)).v and macron(mc(S)).U_alpha != bot$
                    #nst[
                      #kw("require") $tx_alpha = macron(mc(S)).tx_alpha and macron(mc(S)).U_alpha = mc(D).U$ #text(size: 0.8em)[(carried on whatever its status)] \
                      $U_alpha <- macron(mc(S)).U_alpha$
                    ]
                    #kw("else")
                    #nst[
                      #kw("require") $mc(D).sans("status") = sans("Active")$ \
                      $U_alpha <- mc(D).U$
                    ]
                    #kw("require") $U_alpha inter U_(sans("active")) = emptyset and U_alpha inter sans("outputs")(underline(tx)_(sans("req"))) = emptyset$
                  ]
                ]
                #kw("require") $sans("outputs")(underline(tx)_(sans("req"))) inter U_(sans("active")) = emptyset$ #text(size: 0.8em)[(no output reference is re-created)] \
                #kw("require") $U_(sans("active")) applytx underline(tx)_(sans("req")) != bot$ \
                $U <- U_(sans("active")) applytx underline(tx)_(sans("req"))$ \
                #kw("require") $sans("fits")(U,U_alpha,sans("outputs")(tx_omega))$ \
                $hats <- s$ \
                $eta' <- accUTxO(U)$ \
                $(eta')^(\#) <- hash(eta')$ \
                $delta^(\#) <- hash(sans("outputs")(tx_omega))$ #text(size: 0.8em)[(the hash of the empty set if $tx_omega = bot$)] \
                $kappa^(\#) <- hash(hash(U_alpha) || tx_alpha)$ #text(size: 0.8em)[(over the empty set and $bot$ if no deposit is claimed)] \
                $msSig_i <- msSign(hydraSigningKey, (cid || v || hats || (eta')^(\#) || delta^(\#) || kappa^(\#)))$ \
                $hatSigma <- emptyset$ \
                #kw("multicast") $(hpAS, hats, msSig_i)$ \
                $forall tx in underline(tx)_(sans("req")):$ #kw("output") $(hpSeen, tx)$ \
                $hatmL <- U$ \
                $X <- hatmT$ \
                $hatmT <- emptyset$ \
                #kw("for") $tx in X : hatmL applytx tx != bot and sans("outputs")(tx) inter U_alpha = emptyset and sans("fits")(hatmL applytx tx,U_alpha,sans("outputs")(tx_omega))$
                #nst[
                  $hatmT <- hatmT union {tx}$ \
                  $hatmL <- hatmL applytx tx$
                ]
                $hatmL <- hatmL without sans("spendingInputs")(sans("locallyPendingDecommit"))$
              ]
            ]

            #proc([$sans("adopt")(s, xi)$ #text(size: 0.8em)[(called by the chain handlers)]])[
              #kw("if") $s = hats = macron(mc(S)).s + 1$
              #nst[
                $eta' <- accUTxO(hatmU)$ \
                $(eta')^(\#) <- hash(eta')$ \
                $delta^(\#) <- hash(U_omega)$ \
                $kappa^(\#) <- hash(hash(U_alpha) || tx_alpha)$ \
                #kw("if") $msVfy(hydraKeysAgg, (cid || hatv || hats || (eta')^(\#) || delta^(\#) || kappa^(\#)), xi)$
                #nst[
                  $macron(mc(S)) <- Sno(hatv, hats, hatmT, hatmU, U_alpha, U_omega)$ \
                  $macron(mc(S)).sigma <- xi$ \
                  $sans("reconcileReservation")()$
                ]
              ]
            ]
          ],
          [
            #proc([#kw("on") $(hpAS, s, msSig_j)$ #kw("from") $party_j$])[
              #kw("require") $s in {hats, hats + 1}$ \
              #kw("wait") $hats = s$
              #nst[
                #kw("require") $(j, dot.c) in.not hatSigma$ \
                $hatSigma[j] <- sigma_j$ \
                #kw("if") $forall k in [1..n] : (k, dot.c) in hatSigma$
                #nst[
                  $msCSig <- msComb(hydraKeys^("setup"), hatSigma)$ \
                  $eta' <- accUTxO(hatmU)$ \
                  $(eta')^(\#) <- hash(eta')$ \
                  $delta^(\#) <- hash(U_omega)$ \
                  $kappa^(\#) <- hash(hash(U_alpha) || tx_alpha)$ \
                  #kw("require") $msVfy(hydraKeysAgg, (cid || hatv || hats || (eta')^(\#) || delta^(\#) || kappa^(\#)), msCSig)$ \
                  $macron(mc(S)) <- Sno(hatv, hats, hatmT, hatmU, U_alpha, U_omega)$ \
                  $macron(mc(S)).sigma <- msCSig$ \
                  $sans("reconcileReservation")()$ \
                  $forall tx in mT_(sans("req")) :$ #kw("output") $(hpConf, tx)$ \
                  #kw("if") $macron(S).U_omega != bot$
                  #nst[
                    #kw("postTx") $(mtxDecrement, hatv, hats, (eta')^(\#), macron(mc(S)).sigma)$
                  ]
                  #kw("if") $macron(S).U_alpha != bot$
                  #nst[
                    #kw("postTx") $(mtxIncrement, hatv, hats, (eta')^(\#), macron(mc(S)).sigma)$
                  ]
                  #kw("if") $hpLdr(s + 1) = i and (hatmT != emptyset or sans("newDecommit")(hatv, macron(mc(S)), tx_omega))$
                  #nst[
                    #kw("if") $tx_alpha = bot and tx_omega = bot and macron(mc(S)).U_alpha = emptyset$
                    #nst[
                      $tx_alpha <-$ oldest $D in mc(D)$ with $D.sans("status") = sans("Active") and D.U inter macron(mc(S)).U = emptyset and D.U inter sans("outputs")(hatmT) = emptyset and sans("depositFits")(D)$
                    ]
                    #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, sans("requestable")(hatmT, tx_alpha, sans("requestDecommit")(hatv, macron(mc(S)), tx_omega)), tx_alpha, sans("requestDecommit")(hatv, macron(mc(S)), tx_omega))$
                  ]
                ]
              ]
            ]

            #proc([#kw("on") $(mtxDeposit, tx_alpha, U, t_(sans("created")), t_(sans("deadline")))$ #kw("from") chain])[
              $mc(D) <- mc(D) union (tx_alpha, sans("depositObj")(U, t_(sans("created")), t_(sans("deadline")), sans("Inactive")))$
            ]

            #proc([#kw("on") $(mtxRecover, tx_alpha)$ #kw("from") chain])[
              $mc(D) <- mc(D) without (tx_alpha, dot.c)$
            ]

            #proc([#kw("on") $(mtxDecrement, U, v, s, xi)$ #kw("from") chain])[
              $sans("adopt")(s, xi)$ \
              $hatv <- v$ \
              $tx_omega <- bot$ \
              #kw("if") $hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i and (hatmT != emptyset or sans("newDecommit")(hatv, macron(mc(S)), tx_omega))$
              #nst[
                #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, sans("requestable")(hatmT, tx_alpha, bot), tx_alpha, bot)$
              ]
            ]

            #proc([#kw("on") $(mtxIncrement, U, v, s, xi)$ #kw("from") chain])[
              $sans("adopt")(s, xi)$ \
              $hatv <- v$ \
              $tx_alpha <- bot$ \
              $hatmL <- hatmL union U$ \
              #kw("if") $hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i and (hatmT != emptyset or sans("newDecommit")(hatv, macron(mc(S)), tx_omega))$
              #nst[
                #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, sans("requestable")(hatmT, bot, sans("requestDecommit")(hatv, macron(mc(S)), tx_omega)), bot, sans("requestDecommit")(hatv, macron(mc(S)), tx_omega))$
              ]
            ]

            #proc([#kw("on") $(sans("tick"), t)$ #kw("from") chain])[
              #kw("for") $D in mc(D)$
              #nst[
                #kw("if") $t > D.sans("deadline") - Tdeposit$
                #nst[
                  $D.sans("status") <- sans("Expired")$
                ]
                #kw("else if") $t > D.sans("created") + Tdeposit$
                #nst[
                  $D.sans("status") <- sans("Active")$ \
                  #kw("if") $tx_alpha = bot and tx_omega = bot$
                  #nst[
                    $tx_alpha <- D$
                  ]
                ]
              ]
              #kw("if") $sans("newDecommit")(hatv, macron(mc(S)), tx_omega) and hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i$
              #nst[
                #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, sans("requestable")(hatmT, bot, tx_omega), bot, tx_omega)$
              ]
              #kw("if") $exists D in mc(D) : D.sans("status") = sans("Active")$
              #nst[
                #kw("if") $tx_alpha != bot and tx_omega = bot and hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i$
                #nst[
                  #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, sans("requestable")(hatmT, tx_alpha, bot), tx_alpha, bot)$
                ]
              ]
            ]
          ],
        )

        #line(length: 100%, stroke: 0.4pt + luma(120))
        #v(0.2em)

        #grid(
          columns: (1fr, 1fr),
          column-gutter: 1em,
          [
            #proc([#kw("on") $(hpClose)$ #kw("from") client])[
              $(eta')^(\#) <- macron(mc(S)).(eta')^(\#)$ \
              $xi <- macron(mc(S)).sigma$ \
              #kw("postTx") $(mtxClose, hatv, macron(mc(S)).v, macron(mc(S)).s, (eta')^(\#), xi)$
            ]
          ],
          [
            #proc([#kw("on") $(gcChainClose, s_c, (eta')^(\#), xi) or (gcChainContest, s_c, (eta')^(\#), xi)$ #kw("from") chain])[
              $sans("adopt")(s_c, xi)$ \
              #kw("if") $macron(mc(S)).s > s_c$
              #nst[
                $(eta')^(\#) <- macron(mc(S)).(eta')^(\#)$ \
                $xi <- macron(mc(S)).sigma$ \
                #kw("postTx") $(mtxContest, hatv, macron(mc(S)).v, macron(mc(S)).s, (eta')^(\#), xi)$
              ]
            ]
          ],
        )
      ]
    ]
  },
  caption: [Head-protocol machine for the _coordinated head_ from the perspective of party $party_i$.],
) <fig:off-chain-prot>

== Operational snapshot-round refinement

The node executes `reqTx`, `reqSn`, `ackSn`, network `reqDec` and client
`Recover` as typed crem phases whose actions are inspectable programs. Ledger application,
signing and multisignature verification are primitives. Snapshot construction,
incremental-action selection, transaction pruning and effect ordering remain
protocol rules. Signing keys are interpreter configuration, never trace data.
The runtime supplies public rule parameters and primitive implementations to the
pure entry point. Each interpreter configuration supplies transaction application,
signing and verification; debugger recording uses the same verifier, while replay
consumes its retained answer. Concrete Cardano helpers maintain the two accumulator
deltas, canonical snapshot encoding and certificate assembly in party order.
These boundaries preserve the existing bytes, query order and decisions.
Completion commits the ordered changes and releases effects; waiting retains
only the ordered change prefix; rejection discards both buffers. In particular,
a failed multisignature check discards the preceding tentative signature record.

Each input starts a fresh workflow; its phase is not persisted or resumed on
retry. The workflow exposes instructions to the existing primitive interpreter
and terminal semantics. Independent Agda projections check the bounded outcomes;
interpreter and replay tests check execution and recovery consistency. Crem constrains phase edges
and payload types; it does not prove the guards or the primitive replies. Its
static topology omits decisions inside actions and early wait/reject exits;
concrete instruction traces retain those decisions and exits.

The extended Crem API selects the next typed phase inside a Program
after a primitive reply. `reqTx` branches directly to deposit checks or retry;
client decommit and `reqDec` branch to output checks or their existing failure
path. `ackSn` branches directly to confirmation with a verified snapshot or
rejects. Settlement verifies and optionally adopts its candidate in one action,
after recording finalization, then enters the next-request phase.
Queries, rejection, invalidation and adoption instructions keep their order.
The independent Agda outcome projection states concrete wait,
decommit-precedence, settlement and invalid-final-acknowledgement equations pin
these boundaries; they do not prove equivalence of the Haskell implementations.
Every workflow and state owner has optional phase descriptions. These inspect
constructors without evaluating retained payloads and make no strictness claim.
The live owner composition drops rendering metadata from its inspectable
definition; this does not change its allowed transitions.

Live snapshot admission and trusted event recovery use separate entry points.
They share storage updates, but importing a retained request marker does not add
an edge from collecting signatures to sending a request in the live topology.
A live candidate requires this Head, the current version, the next confirmed
number, and no candidate already collecting signatures. A local request marker
must name that same number.

Only the owner issues a candidate handle. It binds the Head, snapshot version,
number, canonical signing bytes and ordered parties. Signature recording and
confirmation consume that handle; a generally verified certificate is sufficient
only for the separate adoption operation. Owner refusal rejects the entire
Program attempt, including any staged signature. Restoration derives signing
bytes from snapshot contents instead of trusting an in-memory byte cache.
Within a runtime, the getter and machine share that restored candidate. The
composition carries the typed snapshot state through its output, so signature
updates do not re-encode the same snapshot. Confirmation retires the candidate;
accepting another round issues a new one. A persisted checkpoint restores a fresh
candidate. This changes evaluation and allocation, not the protocol transitions.
Settlement can advance the working version without replacing a collecting
candidate, so its late final acknowledgement remains admissible.

The independent owner projection excludes cryptographic and ledger correctness:
it treats canonical signing contents as an abstract identity and signature
validity as a premise. It checks the live edge relation and complete candidate
identity; it does not model heap sharing or allocation. Haskell tests exercise
real signatures, stale and foreign handles,
cache inconsistencies and event/checkpoint recovery. These checks do not make
raw retained events safe to import from untrusted sources.

Each accepted partial `ackSn` records one signature event. Old event logs can
contain two identical records; replay remains idempotent and accepts both forms.
An acknowledgement for the last confirmed snapshot is ignored only after its
number guard succeeds. Rejected final verification discards the current input's
staged signature. The abstract transition relation describes one signature map
assignment, as does the ordered outcome reference.

`OffChainReference.agda` independently computes bounded ordered snapshot outcome
values with named phase, change, effect, wait and rejection constructors, including number/leader rejection, predecessor/version/missing-transaction
waits, ledger rejection, early and duplicate acknowledgements, verification
failure, confirmation and the next request. This projection excludes payloads,
pruning contents, output collisions, accumulator overflow, incremental actions
and scheduler behavior. `reqDecOutcomeRef` separately checks selected-commit,
existing-decommit, ledger and nonempty-output guard priority, retry expiry,
recording and request selection for decommits whose inputs are confirmed.
`carriedDecommitPayloadRef` separately selects a matching reservation or registry
entry using bounded output-reference sets. `carriedDecommitRequestRef` checks the
same-version carry obligation. Neither models transaction witnesses, ledger
validity, registry key integrity or serialization; Haskell sequence tests cover
reordering, invalid signatures, hostile duplicates and event/checkpoint recovery.
Deferred work and the decommit-only wake-up are checked by the separate readiness
projection. `recoverOutcomeRef` checks registration before
the finalized-increment guard. These projections exclude deposit selection,
ledger/cryptographic payloads and chain deadline validation. The Haskell tests
also compare the complete ordered outcomes and serialized states. Neither these
comparisons nor the bounded explorer establish an unbounded protocol proof.

The development scheduler uses the node's retry eligibility: unsynced inputs
park, unresolved-commit snapshot requests retain their budget, other network
inputs decrement a positive budget, and exhausted inputs drop. Delivery order
and delay are explored only within explicit limits.

A focused retry-window search receives an initial message prefix, varies each
party's retry budget independently, then drains remaining messages. It finds the
finite-expiry progress witness within the default limits. This is a restricted
schedule family, not exhaustive exploration of all delivery orders or a model of
the node queue's wall-clock timing.

Separate Haskell sequences exercise three parties through deposit activation,
signed confirmation, increment, spending the deposit, rollback and re-observation.
They vary the rollback boundary, confirmation timing and re-observation order,
and check serialized events and checkpoints after every transition. Chain inputs
are assumed valid; these checks do not validate Cardano settlement transactions
or extend the Agda model to rollback, crash recovery or general liveness.

A fixed three-party Haskell sequence preserves the historical decommit failure:
`reqDec` accepts a decommit against the optimistic local ledger, but `reqSn`
applies it before ordinary transactions against the confirmed ledger. The old
leader therefore requests a decommit before its dependency exists. Its snapshot
rebase also restores the reserved input. The corrected sequence preserves the
reservation, confirms the dependency in snapshot 2, and starts a decommit-only
snapshot 3. The regression checks serialized events and checkpoints at every prefix.
It is a deterministic SimpleTx witness, not a general progress proof.

The development checks independently require exact registry payload equality for
locally pending transactions, reconstruct the local UTxO from the current
snapshot and pending sequence, and check for stranded work when an honest
scenario has no queued deliveries or retries. Random scheduled prefixes check
safety; fair delivery before retry exhaustion checks confirmation of dependent
work. None is a proof of chain progress or Byzantine liveness.

The implemented duplicate policy ignores a transaction identifier already in the
local sequence and registry before staging receipt or consulting the ledger.
It prevents an invalid same-ID witness variant replacing a validated payload and
a duplicate timeout deleting it. `reqTxKnownOutcomeRef` models this guard; the
lemma above establishes its no-change outcome independently of the primitive
answer. The original one-attempt projection remains for fresh IDs. This does not
repair a registry already corrupted by historical events.

Finite retry expiry still drops a missing dependency; progress then requires
client re-submission. The normal-interpreter witness and recovery test make this
policy explicit. An invalid early acknowledgement is still retained until the
aggregate check and blocks replacement from that party. Rejection is safe, but
there is no automatic recovery or Byzantine progress guarantee; closing the head
remains the protocol exit. Altering this acknowledgement policy requires a
separate protocol decision. Regression tests preserve its ordered events.

The instruction language, evaluator and trace vocabulary are separate modules.
A rule's public environment excludes signing keys. All programs use the same
interpreter configuration with functions for each primitive.

=== Retained-event machines

The Haskell implementation applies retained `StateChanged` events through
three sequential owners: tracked deposits, head lifecycle, and the node clock.
The open lifecycle state contains composed snapshot, transaction and commit owners.
Their ports carry owned projections and settlement context, never a mutable
NodeState. The deposit registry supplies a deposit value before consuming it.
The input rules above still choose events and effects. A rejected input commits
no events; a waiting input commits its retained prefix. Event replay performs
neither admission checks nor signature verification.

`Snapshots.Replay` enumerates every retained event. Returning `Nothing` there
means that the coordinated owners have no update; the other owners
still process the event. This also covers acknowledgements and confirmations
without an explicit snapshot when no candidate is collecting signatures.
`DepositRecorded` and `DepositExpired` update the
registry. Activation can select a deposit, but does not make its outputs
spendable. Recovery consumes the registry entry and clears a matching selection.
`CommitFinalized` consumes the entry and, at a fresh version, adds the deposit
value to the local UTxO. Repeated settlement must not restore outputs already
spent on L2. Expiry does not erase a selected or signed claim: an increment may
still land before the deposit is recovered. `CommitApproved` and
`DecommitApproved` are notifications after `SnapshotConfirmed`; they do not
apply the incremental change again. `DecommitInvalid` likewise reports a
rejection or a reservation already released by snapshot confirmation.

The lifecycle projection distinguishes Idle, Open, Closed, ReadyToFanout,
PendingFanout and FanoutProgress. Init opens the head directly in this branch.
ReadyToFanout records that a notification was sent; contestation can extend the
deadline without resetting that flag. PendingFanout represents optimistic
fanout with no distributed output observed. Reverting it restores Closed with
the notification flag set. Rollback updates chain state and tracked deposit
history; it does not undo off-chain transactions or snapshot versions.

An open head separately tracks a selected deposit and a pending decommit. Raw
persisted events can represent both, so the replay machine includes that state;
the mutual-exclusion guards above remain necessary. A deposit's recorded,
active, expired and consumed phases are distinct from its selection for a
snapshot. Re-observation resets its record slot; rollback can remove the record
or undo its consumption while retaining its stored activation status.

Snapshot coordination has no request seen, idle after a snapshot, request sent,
and collecting signatures phases. Outside Open the coordination component is
inactive, although a closed head can adopt the signed snapshot of a contest.
Transaction receipt and partial acknowledgement do not create new coordination
phases. A fresh settlement preserves a snapshot collecting signatures and resets
other coordination phases to the last confirmed number. A repeated settlement
preserves the phase. Retry queues and waiting inputs are scheduler state.

`OffChainReference.agda` defines independent lifecycle, coordination and tracked
deposit phase functions, plus the pending-action projection. Differential tests
compare these abstractions with event application. Separate tests check complete
states across continuous execution, reconstruction and CBOR recovery. The finite phase graphs permit raw replay
transitions that admission guards exclude. They do not enumerate ledger
contents, signature maps, versions, scheduler states or arbitrary history, and
the comparison is not a proof of the complete node. Checkpoints restore all
components together; foreign-head filtering precedes their execution.

=== Complete input workflow

Every Haskell input now enters a crem workflow with `Program` actions. Admission
checks client accumulator bounds before echoing any input, then applies the
synchronisation gate. Chain inputs remain admissible while catching up; client
inputs report rejection and network inputs wait. Typed phases carry the Idle,
Open, Closed or FanoutProgress state into dispatch. Multi-step transaction,
snapshot, decommit and settlement rules have their own phases; small actions
compose directly as programs. The interpreter supplies primitives and terminal
semantics, not protocol decisions.

Tick handling records time, updates synchronisation, changes deposit statuses,
and finally selects head-specific work. Settlement records finalization before
attempting snapshot adoption. Adoption requires a matching number, the seen
snapshot version plus one, the matching incremental action, and valid signatures.
A failed verification still retains finalization. Rollback reposts retained
settlements only strictly before their observed slot; it preserves L2 state.
Closing and contesting may adopt an unconfirmed signed snapshot. Side-loading
checks snapshot number and L1-relevant fields before signature verification;
failed requirements report to the client without staging changes.

The independent reference additionally projects input admission, client decommit
failure priority, settlement adoption, the strict repost boundary, and fanout
continuation mode. Haskell tests compare these projections with ordered outcomes and check
serialized replay at each input prefix. These checks do not prove the routing table,
fanout multiset arithmetic, chain validity, termination or distributed progress.

A two-head recovery test checks that a recovery observation consumes a registered
deposit from a previous head while another head is active. `DepositRecovered` is
node-level and is explicitly exempt from the retained-event head-id filter.

=== Composed snapshot and transaction state

ReqTx, ReqDec, ReqSn, AckSn, settlement and side-loading stage their core changes
through live crem transitions. Event recovery shares the storage updates without
repeating live snapshot admission. One component owns the candidate snapshot,
signature map and confirmed snapshot; another owns the transaction registry,
local transaction sequence, local UTxO and pending decommit. Crem's sequential
composition connects them with a typed rebase command. A third component owns
deposit selection and the commit phase relative to the confirmed certificate.
The snapshot component
supplies the snapshot and retained transactions; only the transaction component
chooses how to reconstruct its UTxO. No component receives a NodeState.

Live confirmation requires an opaque result from successful multisignature
verification of an owner-issued candidate handle. The handle binds the Head,
version, number, canonical signing contents and ordered parties. Confirmation
checks that it still identifies the owned candidate. Adoption uses separate
certificate evidence and checks the Head and parties. A refusal rejects the
Program, discarding staged changes and effects. Restoring a candidate derives its
signing bytes from the snapshot rather than trusting a supplied cache. This still
depends on the primitive implementation; it is not a proof of cryptography in
the type system. Replay trusts retained events
and skips verification. The snapshot owner also handles explicit adoption,
clearing, settlement retention and version changes. The transaction owner checks
whether a deposit fits, applies settled outputs and releases reservations. The
commit owner applies selection and recovery updates. A repeated settlement does
not release a newer reservation; rollback only updates the chain context.
Lifecycle owns closed-state snapshot adoption. Checkpoint restoration rebuilds
all owners from the unchanged persisted representation.

Applying one retained outcome shares a runtime across its event list, including
wait prefixes. A checkpoint in that list resets every component. Execution after
each input still resumes from NodeState; no continuation is persisted. Tests
compare batched application with event-by-event application and serialized replay, including checkpoints and foreign-head events.

The conversion preserves Complete, Wait and Reject semantics and event ordering,
except that a partial acknowledgement records its signature once. State and event
encodings remain unchanged; replay accepts older duplicate signature events
idempotently. Two decommit corrections accompany the implementation: rebase
removes pending spending inputs after rebuilding, and request selection defers
new decommits until their required inputs are confirmed. A newly eligible
decommit can start a round without ordinary transactions; a carried one cannot.

OffChainReference.agda independently states reservation exclusion, confirmed
input availability, reservation-preserving rebase and the decommit-only wake-up
condition. Tests compare the actual owner and request selection against these
functions over subsets of three output identities. Cardano input-availability
tests cover spending, reference and collateral references separately. The Agda
projection excludes scripts, time validity, settlement history and Cardano
ledger validation.
Checkpoint restoration retains its old representation and does not repair
historically inconsistent state by itself.

The reference also projects the version and reservation after decrement
observations. Bounded tests cover fresh, equal and older versions with and without
a pending reservation. Complete Haskell sequence tests cover deposit activation,
settlement before acknowledgement, reobservation, rollback, explicit adoption,
clearing and recovery, comparing uninterrupted continuations with reconstruction
and CBOR round trips at every prefix. The natural-number projection does not
model settlement payload identity; the sequence tests cover that separately.


=== Decommit authority and confirmation disposition

The transaction owner distinguishes `NoDecommit`, `DecommitPending` and
`AwaitingDecommitSettlement` in `DecommitPhase`. These describe local intent and current-version authority,
not merely whether a transaction field is populated. Recording a local decommit
enters `DecommitPending`; confirmation carrying it enters `AwaitingDecommitSettlement`;
a conflicting confirmation releases it to `NoDecommit`. Adoption can enter
`AwaitingDecommitSettlement` without a local transaction. Fresh decrement observation
returns to `NoDecommit`; repeated observations and rollback do not reinstate old
work. `AwaitingDecommitSettlement` cannot transition directly to `DecommitPending`.
Crem checks that missing edge; guards additionally prevent replacing an operation
within a phase, since Crem permits identity transitions.

`OffChainReference.agda` independently defines this transition relation and proves
that the replacement edge is impossible. Its `retainReservationRef` checks carried
authority or surviving required inputs, `decommitAdmissionRef` accounts for both
local intent and confirmed authority, and `verificationContextRef` compares Head
identity and ordered lists of parties. Haskell tests compare these predicates over
bounded sets and real signature verification. The reference abstracts keys and
UTxO names; it does not prove cryptography, ledger validity, crash safety or
progress under arbitrary delivery. Separate integration schedules check 24
orders of competing ordinary and decommit spends, with serialized replay at
every delivery. Persisted constructors and their encodings are unchanged.

=== Commit selection and authority

`CommitPhase` distinguishes `NoCommit`, `CommitSelected` and
`AwaitingCommitSettlement`. A selected deposit is local intent, not spendable
funds. A current-version confirmed snapshot containing a commit takes precedence
over that selection when determining the phase. Adoption can therefore enter
`AwaitingCommitSettlement` without a selected deposit, including legacy
certificates without a deposit identifier. Clearing local state or recovering
the selected deposit does not erase the certificate.

A fresh increment advances the version, adds the deposited outputs once and
clears selection. If it arrives before acknowledgement, subsequent confirmation
of the older snapshot does not restore its settlement phase. Reobservation
does not restore outputs spent in the meantime or clear another selection.
Expiry only changes the deposit registry; recovery clears a matching selection;
rollback does not undo the signed certificate or the off-chain version.

A pending commit can be dropped after recovery or after its deadline plus the
rollback margin. Until a replacement snapshot confirms, its old certificate
remains visible as `AwaitingCommitSettlement`. A replacement certificate can
retire the claim while leaving another selection, so the commit graph permits
`AwaitingCommitSettlement` to `CommitSelected`. All three phase pairs can occur:
the graph alone does not prevent replacement within a phase or premature
dropping. `settlingCommitAtPeriod`, ReqSn and ReqDec enforce eligibility, carry
obligations and mutual exclusion. These guards also prevent selecting both a
commit and a decommit in a proposed snapshot. The phase is not a substitute for
the registry and time checks.

`OffChainReference.agda` defines this commit transition relation and the phase
projection from selection, version and signed claim. Bounded tests compare it
with the live owner, including missing deposit identifiers. Command sequences
compare live continuations, retained-event replay and serialized reconstruction
after every transition. They cover both settlement orders, adoption, clearing,
replacement, retained selections and spendability after repeated settlement.
Replay sequences cover expiry, recovery and rollback of a signed claim; ReqDec
tests check the settlement-window boundary without a local selection. These
checks supplement the raw selected-deposit/pending-decommit projection, which
does not describe signed authority.
