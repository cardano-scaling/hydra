```
module Hydra.Protocol.Overview where
```

#import "/template.typ": *
#import "/macros.typ": *
#import "/diagrams.typ": head-fsm, deposit-fsm

= Protocol Overview <sec:overview>

The Hydra Head protocol provides functionality to lock a set of UTxOs on a
blockchain, referred to as the _mainchain_, and evolve it inside a
so-called off-chain _head_, independently of the mainchain. At any point,
the head can be closed with the effect that the locked set of UTxOs on the
mainchain is replaced by the latest set of UTxOs inside the head. The protocol
guarantees full wealth preservation: no generation of funds can happen off-chain
(inside a head) and no responsive honest party involved in a head can ever lose
any funds other than by consenting to give them away. In exchange for decreased
liveness guarantees (stop any time), it can essentially proceed at network speed
under good conditions, thereby reducing latency and increasing throughput. At
the same time, the head protocol provides the same capabilities as the mainchain
by reusing the same ledger model and transaction formats - making the protocol
"isomorphic".

== Opening the head

To create a head-protocol instance, any party may take the role of an
_initiator_ and ask other parties, the _head members_, to participate
in the head by exchanging public keys and agreeing on other protocol parameters.
These public keys are used for both, the authentication of head-related on-chain
transactions that are restricted to head members (e.g., a non-member is not
allowed to close the head) and for signing off-chain transactions in the head.

The initiator then establishes and directly opens the head by submitting an
_init_ transaction to the mainchain that contains the Hydra protocol
parameters and mints special _participation tokens (PT)_ identifying the
head members. The _init_ transaction initializes a state machine (see
@fig:head-protocol-states) that manages the life-cycle of UTxOs in the
head. The state machine comprises the three states: $stOpen$, $stClosed$, and
$stFinal$. A _state thread token (ST)_ minted in _init_ marks the
head output and ensures contract continuity~@eutxo.

#figure(
  stack(spacing: 1.2em, deposit-fsm, head-fsm),
  caption: [Possible state transitions of a Hydra head. The lower (main) state
    machine is rendered from the same transition data that `check-refs.sh`
    verifies against the Agda `_⟶⟨_⟩_` relation in @sec:on-chain.],
) <fig:head-protocol-states>

The head is opened with an empty UTxO set. Participants can then add funds to the
head by creating _deposit_ transactions on the mainchain, which are
subsequently incorporated into the head via _increment_ transactions (see
@sec:deposit-tx and @sec:increment-tx). Once the $stOpen$
state is confirmed, the head members start running the off-chain head protocol,
which evolves the UTxO set independently of the mainchain.


== The Coordinated Head protocol

The actual Head protocol starts once the head is open on-chain, with an initially
empty UTxO set.

The protocol processes off-chain transactions by distributing them between participants,
while each party maintains their view of the local UTxO state. That is, the current
set of UTxOs evolved from the empty initial state by applying transactions as they
are received from the other parties.

To confirm transactions and allow for distribution of the resulting UTxO set
without needing the whole transaction history, snapshots are created by the
protocol participants. The initial snapshot $U_0$ corresponds to the initial
(empty) UTxO set, while snapshots thereafter $Uset_1, Uset_2, dots.h$ are created with
monotonically increasing snapshot numbers.

For this, the next snapshot leader (round-robin) requests his view of a new confirmed state to be
signed by all participants as a new snapshot. The leader does not need to send his local state,
but only indicate, by hashes, the set of transactions to be included in order to
obtain the to-be-snapshotted UTxO set.

The other participants sign the snapshot as soon as they have (also) seen the
transactions that are to be processed on top of its preceding snapshot: a
party's local state is always ahead of the latest confirmed snapshot.

Signatures are broadcast and aggregated by each party. When all signature parts
of the multi-signature are received and verified, a snapshot is considered
confirmed. As a consequence, a participant can safely delete (if wished) all
transactions that have been processed into it as the snapshot's multi-signature
is now evidence that this state once existed during the head evolution.

=== Updating an open head

Besides processing "normal" transactions, participants can also request to
withdraw some UTxO they can spend from the Head and make it available on main
chain via a $mtxDecrement$~@sec:decrement-tx transaction - the overall
process is also called "decommit".

To add UTxO to an open head, anyone may create a deposit of one or more UTxO
using a $mtxDeposit$~@sec:deposit-tx transaction. The head participants
will observe this deposit and, once settled, request off-chain agreement to
include the deposited UTxO in the form of a snapshot. With that agreement, an
$mtxIncrement$~@sec:increment-tx transaction can be created and used to
update the head state on the mainchain. A deadline is associated with the
deposit, which ensures that the UTxO is locked up long enough to be safely
consumed into the head without risk of double spending. Should a deposit have
not been picked up in time, a $mtxRecover$~@sec:recover-tx transaction
allows anyone to unlock the original UTxO.

== Closing the head

The head protocol is designed to allow any head member at any point in time to
produce, without interaction, a certificate to close the head. This certificate
is created from the latest confirmed, multi-signed snapshot. Using this
certificate, the head member may "force close" the head by advancing the
mainchain state machine to the $stClosed$ state.

Once in $stClosed$, the state machine grants parties a contestation period,
during which parties may contest the closure by posting the certificate of a
newer snapshot on-chain in a contest transaction. Contesting leads back to the
state $stClosed$ and each party can contest at most once. After the
contestation period has elapsed, the state machine may proceed to the $stFinal$
state. The state machine enforces that the outputs of the transaction leading to
$stFinal$ correspond exactly to the latest UTxO set seen during the
contestation period.

== Differences
// TODO More details on what was in orig paper?
In the Coordinated Head protocol, off-chain consensus is simplified by not
having transactions confirmed concurrently to the snapshots (and to each other)
but having the snapshot leader propose, in their snapshot, a set of transactions
for explicit confirmation. The parties' views of confirmed transactions thus
progress in sync with each other (once per confirmed snapshot), thus simplifying
the close/contest procedure on the mainchain. Also, there is no need for
conflict resolution as in Appendix~B of~@hydrahead20. In summary, the
differences to the original Head protocol in~@hydrahead20 are:

- No hanging transactions due to 'coordination'.
- No acknowledgement nor confirmation of transactions.
- No confirmation message for snapshots (two-round local confirmation).
- Allow for deposits and decommits while head is open.
- No initialization phase: head opens directly with empty UTxO, funds added via deposits.
