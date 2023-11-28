# Protocol Overview {#sec:overview}

The Hydra Head protocol provides functionality to lock a set of UTxOs on
a blockchain, referred to as the *mainchain*, and evolve it inside a
so-called off-chain *head*, independently of the mainchain. At any
point, the head can be closed with the effect that the locked set of
UTxOs on the mainchain is replaced by the latest set of UTxOs inside the
head. The protocol guarantees full wealth preservation: no generation of
funds can happen off-chain (inside a head) and no responsive honest
party involved in a head can ever lose any funds other than by
consenting to give them away. In exchange for decreased liveness
guarantees (stop any time), it can essentially proceed at network speed
under good conditions, thereby reducing latency and increasing
throughput. At the same time, the head protocol provides the same
capabilities as the mainchain by reusing the same ledger model and
transaction formats - making the protocol "isomorphic".

## Opening the head

To create a head-protocol instance, any party may take the role of an
*initiator* and ask other parties, the *head members*, to participate in
the head by exchanging public keys and agreeing on other protocol
parameters. These public keys are used for both, the authentication of
head-related on-chain transactions that are restricted to head members
(e.g., a non-member is not allowed to close the head) and for signing
off-chain transactions in the head.

The initiator then establishes the head by submitting an *initial*
transaction to the mainchain that contains the Hydra protocol parameters
and mints special *participation tokens (PT)* identifying the head
members. The *initial* transaction also initializes a state machine (see
Fig. [1](#fig:SM_states_basic)) that manages the "transfer" of UTxOs
into the head and back. The state machine comprises the four states:
$\stInitial$, $\stOpen$, $\stClosed$, and $\stFinal$. A *state thread
token (ST)* minted in *initial* marks the head output and ensures
contract continuity [@eutxo].

```{#fig:SM_states_basic .tikz
caption="Mainchain state diagram of the Hydra protocol."
}
\begin{tikzpicture}[>=stealth,auto,node distance=2.8cm, initial text=$\mathsf{init}$, every
    state/.style={text width=10mm, text height=2mm, align=center}]
    \node[state, initial] (initial) {$\mathsf{initial}$};
    \node[state] (open) [above right of=initial] {$\mathsf{open}$};
    \node[state] (closed) [right of=open] {$\mathsf{closed}$};
    \node[state] (final) [below right of=closed] {$\mathsf{final}$};

    \path[->] (initial) edge [bend left=20] node {$\mathsf{collect}$} (open);
    \path[->] (open) edge [bend left=20] node {$\mathsf{close}$} (closed);
    \path[->] (closed) edge [bend left=20] node {$\mathsf{fanout}$} (final);
    \path[->] (closed) edge [loop above] node {$\mathsf{contest}$} (closed);
    \path[->] (initial) edge node {$\mathsf{abort}$} (final);
\end{tikzpicture}
```

Once the initial transaction appears on the mainchain, establishing the
initial state $\stInitial$, each head member can attach a transaction,
which locks (on the mainchain) the UTxOs that the party wants to commit
to the head, or deliberately acknowledges to commit nothing.

The commit transactions are subsequently collected by the transaction
causing a transition from $\stInitial$ to $\stOpen$. Once the $\stOpen$
state is confirmed, the head members start running the off-chain head
protocol, which evolves the initial UTxO set (the union over all UTxOs
committed by all head members) independently of the mainchain. For the
case where some head members fail to post a transaction, the head can be
aborted by going directly from $\stInitial$ to $\stFinal$.

## The Coordinated Head protocol

The actual Head protocol starts after the initialization phase with an
initial set of UTxOs that is identical to the UTxOs locked on-chain via
the and transactions.

The protocol processes off-chain transactions by distributing them
between participants, while each party maintains their view of the local
UTxO state. That is, the current set of UTxOs evolved from the initial
UTxO set by applying transactions as they are received from the other
parties.

To confirm transactions and allow for an on-chain decommit of the
resulting UTxO set without needing the whole transaction history,
snapshots are created by the protocol participants. The initial snapshot
$U_{0}$ corresponds to the initial UTxO set, while snapshots thereafter
$\Uset_1,\Uset_2,\ldots$ are created with monotonically increasing
snapshot numbers.

For this, the next snapshot leader (round-robin) requests his view of a
new confirmed state to be signed by all participants as a new snapshot.
The leader does not need to send his local state, but only indicate, by
hashes, the set of transactions to be included in order to obtain the
to-be-snapshotted UTxO set.

The other participants sign the snapshot as soon as they have (also)
seen the transactions that are to be processed on top of its preceding
snapshot: a party's local state is always ahead of the latest confirmed
snapshot.

Signatures are broadcast and aggregated by each party. When all
signature parts of the multi-signature are received and verified, a
snapshot is considered confirmed. As a consequence, a participant can
safely delete (if wished) all transactions that have been processed into
it as the snapshot's multi-signature is now evidence that this state
once existed during the head evolution.

## Closing the head

The head protocol is designed to allow any head member at any point in
time to produce, without interaction, a certificate to close the head.
This certificate is created from the latest confirmed snapshot,
specifically from its snapshot number and the respective multisignature.
Using this certificate, the head member may "force close" the head by
advancing the mainchain state machine to the $\stClosed$ state.

Once in $\stClosed$, the state machine grants parties a contestation
period, during which parties may contest the closure by posting the
certificate of a newer snapshot on-chain in a contest transaction.
Contesting leads back to the state $\stClosed$ and each party can
contest at most once. After the contestation period has elapsed, the
state machine may proceed to the $\stFinal$ state. The state machine
enforces that the outputs of the transaction leading to $\stFinal$
correspond exactly to the latest UTxO set seen during the contestation
period.

## Differences

In the Coordinated Head protocol, off-chain consensus is simplified by
not having transactions confirmed concurrently to the snapshots (and to
each other) but having the snapshot leader propose, in their snapshot, a
set of transactions for explicit confirmation. The parties' views of
confirmed transactions thus progress in sync with each other (once per
confirmed snapshot), thus simplifying the close/contest procedure on the
mainchain. Also, there is no need for conflict resolution as in
Appendix B of [@hydrahead20]. In summary, the differences to the
original Head protocol in [@hydrahead20] are:

-   No hanging transactions due to 'coordination'.

-   No acknowledgement nor confirmation of transactions.

-   No confirmation message for snapshots (two-round local
    confirmation).


<!-- TODO: Remove mermaid example -->

```{#fig:mermaid-example .mermaid caption="A simple flowchart."}
graph TD;
    A-->B;
    A-->C;
```
