
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
`decommitOutputsHash`/`commitOutputsHash`, bound into the signed message) and
the aggregate-signature slot $macron(mc(S)).sigma$ ($bot$ until confirmed),
which the figure leaves implicit. \
\
Additionally, deposit objects are created using
$"depositObj"(U, t_("created"), t_("deadline"), "status")$
where status can be $"Inactive"$, $"Active"$, or $"Expired"$.

In Agda, a UTxO set is a finite map from output references to outputs, and a
party's local state and the protocol messages are captured by dedicated record
types.


The local-ledger application $applytx$ and the UTxO-map operations the handlers
use are kept abstract, as the off-chain trust base: `applyTxs` with its nil and
compositionality laws (the two ledger laws the @sec:security proofs consume), the
joint-applicability predicate `Applicable`, and the map operations recording how
$hatmL$ changes.


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
is applied to the _local_ ledger state $hatmL applytx tx$. If not
applicable yet, the protocol does #kw("wait") to retry later or eventually marks
this transaction as invalid (see assumption about events piling up). After
applying and if there is no current snapshot in flight ($hats = macron(mc(S)).s$)
and the receiving party $party_(i)$ is the next snapshot leader, a message to
request snapshot signatures $hpRS$ is sent. \
\
#dparagraph[$hpRD$.]#h(1em) Upon receiving request $(hpRD,tx_omega)$, the
transaction is checked against the _local_ ledger state and if it is not
applicable yet or another deposit or decommit is pending still, the protocol does
#kw("wait") to retry later or eventually marks the decommit as invalid. After
applying $tx$, its outputs are removed from _local_ ledger state $hatmL$
so that they are not available any more and the decommit transaction is kept in
the local state ($tx_omega$). If there is no current snapshot in flight
($hats = macron(mc(S)).s$) and the receiving party $party_(i)$ is the next
snapshot leader, a message to request snapshot signatures $hpRS$ containing the
decrement transaction $tx_omega$ is sent. \
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
- $sans("Active")$ when deposit settled enough: $t > t_("created") + Tdeposit$
When deposits become $sans("Active")$ and no other deposit / decommit is
pending, and the party is the next snapshot leader, it may request a new
snapshot including the deposit transaction $tx_(alpha)$. \
\

The `tick` deposit-status transition is the function `depositStatusAt`,
the off-chain twin of the extracted reference's
`depositStatusRef`.

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
+ If a decommit is requested: verify the transaction is applicable to the
  last confirmed UTxO set and update the active utxo set with it
+ If a decommit is requested: verify it produces at least one output,
  since $mtxDecrement$ materializes the decommit outputs and requires one
+ If a deposit is requested: verify the corresponding deposit is
  $"Active"$ and update the active utxo set with it
+ If we are on the same version as the last snapshot, any requested
  decommit or deposit must match the last snapshot.
+ Verify all requested transactions $underline(tx)_(sans("req"))$ are
  applicable to the active UTxO set
Only then, $party_i$ increments their seen-snapshot counter $hats$, resets the
signature accumulator $hatSigma$, and computes the UTxO set of the new local
snapshot as
$U <- U_(sans("active")) applytx underline(tx)_(sans("req"))$. Then,
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
transactions $hatmT$ to the just requested snapshot's UTxO set iteratively and
ultimately yielding a "pruned" version of $hatmT$ and $hatmL$. \
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
leader and there are already transactions to snapshot in $hatmT$, a
corresponding $hpRS$ is distributed. \
\

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
@sec:increment-tx

Chain observations are transcribed likewise: `ChainEvent` enumerates the
modelled chain observations and `_observes_↝_` gives one constructor per
handler of the figure - the deposit lifecycle above, the version-bumping
increment/decrement observations, and the head-opening `initialTx` of the
Initializing-the-head paragraph.



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




Hence the discipline holds throughout any off-chain run whose start state
satisfies it.



`VersionDiscipline` makes the close/contest version reuse well-formed: the node
reuses the open-state version $hatv$ when posting a contest, which is meaningful
only if the confirmed snapshot is at the current or the immediately-prior
version (it then posts the $hatv$ or $hatv - 1$ signature). Preservation rests
on the on-chain authorize-then-bump rule, captured as the increment/decrement
observations' premise that a version bump is signed at the confirmed snapshot's
version (so $hatv$ never runs more than one version ahead), while `ackSn-confirm`
re-establishes the discipline from its own version premise. This turns an
otherwise unchecked runtime assumption of the implementation into a theorem of
the off-chain state machine.






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
              #kw("wait") $hatmL applytx tx != bot$
              #nst[
                $hatmL <- hatmL applytx tx$ \
                $hatmT <- hatmT union {tx}$ \
                #kw("if") $hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i$
                #nst[
                  #kw("if") $tx_alpha = bot and tx_omega = bot and macron(mc(S)).U_alpha = emptyset$
                  #nst[
                    $tx_alpha <-$ oldest $D in mc(D)$ with $D.sans("status") = sans("Active")$
                  ]
                  #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, hatmT, tx_alpha, tx_omega)$
                ]
              ]
            ]

            #proc([#kw("on") $(hpRD, tx)$ #kw("from") $party_j$])[
              #kw("wait") $U_alpha = emptyset and tx_omega = bot and hatmL applytx tx != bot$
              #nst[
                $hatmL <- hatmL applytx tx without sans("outputs")(tx)$ \
                $tx_omega <- tx$ \
                #kw("if") $hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i$
                #nst[
                  #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, hatmT, bot, tx_omega)$
                ]
              ]
            ]

            #proc([#kw("on") $(hpRS, v, s, underline(tx)_(sans("req")), tx_alpha, tx_omega)$ #kw("from") $party_j$])[
              #kw("require") $s = hats + 1 and hpLdr(s) = j$ \
              #kw("wait") $hats = macron(mc(S)).s and v = hatv$
              #nst[
                #kw("require") $tx_omega = bot or tx_alpha = bot$ \
                #kw("if") $tx_omega != bot$
                #nst[
                  #kw("if") $v = macron(mc(S)).v and macron(mc(S)).U_omega != bot$
                  #nst[
                    #kw("require") $macron(mc(S)).U_omega = sans("outputs")(tx_omega)$
                  ]
                  #kw("else")
                  #nst[
                    #kw("require") $macron(mc(S)).U applytx tx_omega != bot$ \
                    $U_(sans("active")) <- macron(mc(S)).U applytx tx_omega without sans("outputs")(tx_omega)$
                  ]
                ]
                #kw("if") $tx_alpha != bot$
                #nst[
                  $mc(D) <- mc(D)[tx_alpha]$ \
                  #kw("require") $mc(D).sans("status") != sans("Expired")$ \
                  #kw("wait") $mc(D).sans("status") = sans("Active")$
                  #nst[
                    #kw("if") $v = macron(mc(S)).v and macron(mc(S)).U_alpha != bot$
                    #nst[
                      #kw("require") $macron(mc(S)).U_alpha = mc(D).U$
                    ]
                    #kw("else")
                    #nst[
                      $U_alpha <- mc(D).U$ \
                      $U_(sans("active")) <- U_(sans("active")) union U_alpha$
                    ]
                  ]
                ]
                #kw("require") $U_(sans("active")) applytx underline(tx)_(sans("req")) != bot$ \
                $U <- U_(sans("active")) applytx underline(tx)_(sans("req"))$ \
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
                #kw("for") $tx in X : hatmL applytx tx != bot$
                #nst[
                  $hatmT <- hatmT union {tx}$ \
                  $hatmL <- hatmL applytx tx$
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
                  $forall tx in mT_(sans("req")) :$ #kw("output") $(hpConf, tx)$ \
                  #kw("if") $macron(S).U_omega != bot$
                  #nst[
                    #kw("postTx") $(mtxDecrement, hatv, hats, (eta')^(\#), macron(mc(S)).sigma)$
                  ]
                  #kw("if") $macron(S).U_alpha != bot$
                  #nst[
                    #kw("postTx") $(mtxIncrement, hatv, hats, (eta')^(\#), macron(mc(S)).sigma)$
                  ]
                  #kw("if") $hpLdr(s + 1) = i and hatmT != emptyset$
                  #nst[
                    #kw("if") $tx_alpha = bot and tx_omega = bot and macron(mc(S)).U_alpha = emptyset$
                    #nst[
                      $tx_alpha <-$ oldest $D in mc(D)$ with $D.sans("status") = sans("Active")$
                    ]
                    #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, hatmT, tx_alpha, tx_omega)$
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

            #proc([#kw("on") $(mtxDecrement, U, v)$ #kw("from") chain])[
              $hatv <- v$ \
              $tx_omega <- bot$ \
              #kw("if") $hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i and hatmT != emptyset$
              #nst[
                #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, hatmT, tx_alpha, bot)$
              ]
            ]

            #proc([#kw("on") $(mtxIncrement, U, v)$ #kw("from") chain])[
              $hatv <- v$ \
              $tx_alpha <- bot$ \
              $hatmL <- hatmL union U$ \
              #kw("if") $hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i and hatmT != emptyset$
              #nst[
                #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, hatmT, bot, bot)$
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
              #kw("if") $exists D in mc(D) : D.sans("status") = sans("Active")$
              #nst[
                #kw("if") $tx_alpha != bot and tx_omega = bot and hats = macron(mc(S)).s and hpLdr(macron(mc(S)).s + 1) = i$
                #nst[
                  #kw("multicast") $(hpRS, hatv, macron(mc(S)).s + 1, hatmT, tx_alpha, bot)$
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
            #proc([#kw("on") $(gcChainClose, (eta')^(\#)) or (gcChainContest, s_c, (eta')^(\#))$ #kw("from") chain])[
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
