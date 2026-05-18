# Dynamic Head Participants — manual demo

A walkthrough of the [issue #1813](https://github.com/cardano-scaling/hydra/issues/1813) join
flow on the local `process-compose` devnet.

## Prerequisites

- A clean checkout of this branch.
- `nix run .#demo` works on your machine (cardano-node, hydra-node and the
  hydra-tui get built on first run).

## The flow

1. **Start the devnet.**

   ```bash
   nix run .#demo
   ```

   This brings up `cardano-node`, seeds funds for alice / bob / carol, and
   starts `hydra-node-alice` + `hydra-node-bob`. `hydra-tui-alice` and
   `hydra-tui-bob` are also defined; carol's `hydra-node` and `hydra-tui`
   are wired up but `disabled = true`, so they don't auto-start.

   > **process-compose foreground note.** Both TUIs are
   > `is_foreground = true`, which in process-compose means *launch on
   > demand* — they only start when you bring them to the foreground in
   > process-compose's UI (press `F-keys` to cycle, then `F4` / `Enter` to
   > start the highlighted process). If you only ever interact with
   > alice's TUI, **bob's TUI never starts** and bob's hydra-node will
   > have zero API subscribers. The protocol still runs end-to-end (look
   > for `ParametersChanged` and `OnUpdateParametersTx` in
   > `devnet/bob-logs.txt`), but you won't see `JoinFinalized` /
   > `HeadIsOpen` lines in bob's log because those are emitted only when
   > something is connected to the WS API.

2. **Open a 2-party head.** In alice's TUI:
   - Press `i` to `Init` the head.
   - Both TUIs should soon display `HeadIsOpen` with parties `[alice, bob]`.

3. **Invite carol.** From a separate terminal (in the repo root):

   ```bash
   bash demo/invite-carol.sh
   ```

   The script reads carol's hydra and cardano keys from disk, computes her
   `OnChainId`, and `POST`s to alice's `/participants` endpoint. Alice and
   bob multi-sign a snapshot whose `parameterUpdate` is `AddParty carol`,
   the leader posts a single `UpdateParametersTx` to L1, and once it's
   observed both nodes emit `JoinFinalized`.

   The script returns `"OK"` from the POST when the L1 transaction is
   observed; you can also tail `devnet/alice-logs.txt` and
   `devnet/bob-logs.txt` for `JoinRecorded` / `JoinApproved` /
   `JoinFinalized`.

4. **Start carol's node.** Once `invite-carol.sh` returns `OK`, carol
   is both an on-chain party and a known L2 mesh member (alice and bob's
   hydra-nodes called `etcdctl member add` for her host as part of
   processing `JoinFinalized`). Start her processes:

   ```bash
   process-compose process start hydra-node-carol
   process-compose process start hydra-tui-carol
   ```

   Her hydra-node is pre-configured with `--join-existing-cluster` and
   `--start-chain-from 0`, so her etcd joins alice and bob's existing
   raft cluster (rather than bootstrapping a new one) and her chain
   handler replays from genesis to observe the historical `InitTx` and
   `UpdateParametersTx` that admitted her.

   You can confirm carol's etcd joined the mesh:

   ```bash
   # alice and bob each observe carol's etcd peer (port 5003):
   grep PeerConnected devnet/alice-logs.txt devnet/bob-logs.txt | grep 5003
   # carol observes alice (5001) and bob (5002):
   grep PeerConnected devnet/carol-logs.txt | grep -E '500[12]'
   ```

   > **About the grep style.** `nix run .#demo` writes each hydra-node
   > line through `process-compose`, which wraps it in its own JSON
   > envelope. The inner JSON ends up double-escaped (`\"PeerConnected\"`
   > rather than `"PeerConnected"` in the file), so a literal
   > `grep '"PeerConnected"'` returns nothing. Either grep for the bare
   > word (`grep PeerConnected`) and pipe through `grep <port>`, or
   > use `grep -F` and the escaped form, or filter through `jq` after
   > extracting the inner `message` field. Same for every other event
   > below.

5. **Sync carol's snapshot.** Her local snapshot is still the empty
   `InitialSnapshot` (number 0) but alice and bob have already
   confirmed the `AddParty` snapshot (number 1). The next `ReqSn` from
   alice/bob would be rejected by carol as `ReqSnNumberInvalid` until
   her local state catches up. Use the existing snapshot-side-load
   feature: fetch alice's latest confirmed snapshot and inject it on
   carol's WebSocket (e.g. via `websocat`):

   ```bash
   ALICE_CONFIRMED=$(curl -sS http://127.0.0.1:4001/snapshot)
   echo "{\"tag\":\"SideLoadSnapshot\",\"snapshot\":$ALICE_CONFIRMED}" \
     | websocat ws://127.0.0.1:4003
   ```

   Carol's hydra-node verifies the multi-signature against the
   pre-update parties (the side-load path recognises an `AddParty`
   `parameterUpdate` and filters out the joiner from the expected
   signer set). She emits `SnapshotSideLoaded`, and from this point
   onward she signs every subsequent snapshot — verifiable by watching
   for a `SnapshotConfirmed` event with three multi-signatures.

## What you should see

- Alice's TUI shows `HeadIsOpen` → `JoinFinalized` for carol → eventually
  `SnapshotConfirmed` with three signatures.
- `devnet/alice-logs.txt` contains `JoinFinalized` (one occurrence) and
  `devnet/bob-logs.txt` contains the equivalent `ParametersChanged`
  state-change (also one occurrence).
- A new `UpdateParametersTx` on L1, observable with
  `cardano-cli query utxo --whole-utxo` — the head output now carries
  three participation tokens (alice, bob, carol) plus the `HydraHeadV2`
  state token.

## Things that look like errors but aren't

- **`BadInputsUTxO` / `PostingFailed` in process-compose's process log
  for `hydra-node-alice` or `hydra-node-bob`.** When a snapshot is fully
  signed, *every* party posts the resulting L1 transaction (the
  `IncrementTx` after a commit, the `UpdateParametersTx` after a leave or
  join). The first party's submission wins; the others get
  `BadInputsUTxO` because their input was already consumed and the
  failure is enqueued as a `PostTxError` chain event. Both losing nodes
  then observe the winning transaction via chain-sync and the head state
  stays consistent. These messages are noisy but harmless. You can see
  the protocol's net L1 activity from `PostedTx` events: `grep -c
  PostedTx devnet/*-logs.txt` should sum to exactly *3* across both
  files after step 3 (`InitTx` + `IncrementTx` + `UpdateParametersTx`).

- **`JoinFinalized` missing from `devnet/bob-logs.txt`.** Almost always
  this is the "process-compose foreground note" from step 1 — bob's TUI
  was never brought to the foreground in process-compose, so bob's
  hydra-node has zero WebSocket subscribers and the API-output trace
  never fires. Confirm with `grep -c ParametersChanged
  devnet/bob-logs.txt`: it should be `1`. If that's `1` the protocol
  worked; only the visible event in bob's log is missing.
