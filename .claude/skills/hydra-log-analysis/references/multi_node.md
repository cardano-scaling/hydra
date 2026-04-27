# Multi-Node (Cluster) Failure Patterns

Patterns that are only diagnosable with logs from two or more nodes of the same Head. Use after running `scripts/cluster_diagnose.sh`. For each pattern, **re-read the relevant code in `hydra-node/src/Hydra/HeadLogic.hs` before concluding** — the catalog is a starting hypothesis, not a verdict.

---

## ACKSN_GAP_CLUSTER — quorum AckSn not reached cluster-wide {#acksn-gap-cluster}

**Signature**: `SnapshotRequested(sn=N)` appears at one or more nodes, but the merged timeline shows fewer than `n_nodes` distinct nodes emitting `AckSn(sn=N)`, and no node ever emits `SnapshotConfirmed(sn=N)`.

**Mechanism candidates** (verify each by code inspection):
- A peer received `ReqSn(N)` but `RequireFailed` rejected it locally — check that peer's outcome.error in the same window
- The peer never received `ReqSn(N)` — see REQSN_NOT_DELIVERED
- Network partition: peer received but its `AckSn` effect never reached the leader — check leader's input stream for AckSn from that peer
- Peer crashed or was paused — its log will simply end before AckSn

**Code paths to verify**:
- `onOpenNetworkReqSn` in `hydra-node/src/Hydra/HeadLogic.hs` — peer's ReqSn handling
- `onOpenNetworkAckSn` — leader's AckSn aggregation
- The quorum check (look for the function that checks `length signatures == length parties`)

**Confidence rule**: do not conclude "peer X is at fault" unless the merged timeline shows either (a) peer X's log emits a RequireFailed for sn=N, or (b) peer X's log shows no NetworkInput for ReqSn(N) at all, or (c) peer X's log ends before sn=N. Anything else is speculation.

---

## REQSN_NOT_DELIVERED — leader broadcast ReqSn but peers didn't receive it {#reqsn-not-delivered}

**Signature**: leader emits `ReqSn(sn=N)` as an effect, but one or more peers' logs show no `NetworkInput { message = ReqSn(N) }` within a reasonable window (seconds, not minutes).

**Mechanism candidates**:
- etcd / network layer dropped the message
- Peer was offline at the time of broadcast (its log will reflect this — gap in input stream)
- Receiver-side filter rejected it before logging (rare; check `Hydra/Network/...`)

**Code paths to verify**:
- `Hydra/Network/Etcd.hs` — broadcast and delivery
- `Hydra/Node/InputQueue.hs` — input queue ingestion (could it have been coalesced?)

**Confidence rule**: a peer not logging a NetworkInput for ReqSn within ~5 seconds of the leader's effect is strong evidence. Within 5 seconds is ambiguous (timestamps across nodes can drift slightly).

---

## STATE_DIVERGENCE — node state-change streams diverge {#state-divergence}

**Signature**: each node's `stateChanges[].tag` stream, taken in order, is identical up to some index — then differs at index K. Diagnostic output names the index and per-node tag at that position.

**Mechanism candidates**:
- One node missed an input the others received (combine with REQSN_NOT_DELIVERED check)
- One node observed a chain event the others did not (chain-follower lag — combine with CHAIN_SKEW)
- A bug in `aggregateNodeState` produced inconsistent state from the same input — this is the *most serious* possibility, and worth opening an issue for

**Code paths to verify**:
- `aggregateNodeState` in `hydra-node/src/Hydra/HeadLogic.hs` — must be deterministic given the same inputs
- The specific event tags at the divergence point — look for nondeterminism in their handlers

**Confidence rule**: divergence does not by itself indicate a bug. Verify that the divergent nodes received the *same* inputs. If inputs differ, the state divergence is a downstream symptom, not the cause.

---

## CHAIN_SKEW — same chain event observed at very different times {#chain-skew}

**Signature**: the same chain-derived event (e.g. `DepositRecorded(txId=X)`, `CommitFinalized`, `DecommitFinalized`) appears in multiple nodes' logs but with timestamps differing by more than ~5 seconds.

**Mechanism candidates**:
- One node's chain follower is lagging (cardano-node sync issue, network slow)
- The nodes are connected to different cardano-nodes (acceptable in some setups)
- Clock skew between hosts (rare in containerized setups; check `chronyd`/`ntp` if suspected)

**Code paths to verify**: not in `HeadLogic.hs` — this is a chain layer concern. Look at `Hydra/Chain/Direct/...` if needed.

**Confidence rule**: chain skew alone does not cause snapshot failures unless one node is *so* far behind that it hasn't seen a deposit-activation event the others are acting on. Cross-reference with the snapshot timeline before flagging this as a root cause.

---

## When the cluster diagnose returns no findings

This is a real and useful outcome. Possible explanations:
1. The Head is operating normally; the user's hypothesis of "snapshots stopped" may be incorrect — check `find_boundary.sh` against current expected sn.
2. The failure is single-node only — fall back to `snapshot_diagnose.sh` per node and `failure_patterns.md`.
3. The failure is in a layer this skill does not analyze (chain, network transport, persistence) — say so explicitly.

**Do not** synthesize a finding by combining weak signals from multiple checks. If each check individually returned no firm result, the report is "no firm cluster-level diagnosis."
