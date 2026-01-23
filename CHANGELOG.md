# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## [1.3.0] - UNRELEASED

- **BREAKING** A Hydra node will now start rejecting both network and client inputs once its view of the chain has been out of sync for more than 50% of the configured `--contestation-period`, based on **system wall-clock time**.
  - Added `NodeUnsynced` and `NodeSynced` state events and server outputs.
  - Added `RejectedInput` client message.
  - The `Checkpoint` event, and consequently the `EventLogRotated` server output, now carry the different `NodeState` variants: `NodeInSync` or `NodeCatchingUp`.
  - `Greetings` message now also contains the hydra-node synced status to the chain backend.
  - See [Issue #2286](https://github.com/cardano-scaling/hydra/issues/2286) and [PR #2290](https://github.com/cardano-scaling/hydra/pull/2290).
- **BREAKING** Changed `defaultContestationPeriod` from 10 minutes to 12 hours
  (43200 seconds) to align with Cardano's safe zone for mainnet safety. See
  [#2389](https://github.com/cardano-scaling/hydra/issues/2389).
- Fix the `cabalOnly` development environment to allow for `cabal build hydra-node` without `haskell.nix`.
- Add `--unsynced-period` CLI option to configure when the node considers itself
  out of sync with the chain. Defaults to half the contestation period.
- Support non-encoded DELETE `/commit/:tx-id` requests [#2445](https://github.com/cardano-scaling/hydra/pull/2445)
- **BREAKING** Reduced the time required to synchronize the node with the chain after long periods of inactivity (without head transitions).
  - When restarting, the node now resumes from the last seen tick while catching up with the chain, instead of from the last seen head transition.
  - The chain backend now uses a non-empty list of starting points as a prefix to find a valid intersection when reconnecting to the chain.
  - `TickObserved` event schema has changed: the `chainSlot` field has been replaced with `chainPoint`
  - `Greetings` message now also contains a new field `currentSlot` that indicates the last known slot while catching up.
  - See [Issue #2206](https://github.com/cardano-scaling/hydra/issues/2206) and [PR #2407](https://github.com/cardano-scaling/hydra/pull/2407).
- Bounded the number of transactions that will be approved per snapshot
  [#2444](https://github.com/cardano-scaling/hydra/pull/2444).
- Buffer and batch logging so it's faster [#2452](https://github.com/cardano-scaling/hydra/pull/2452)

## [1.2.0] - 2025.11.28

- There is a new `SafeClose` client command which prevents closing the Head in case there are non-ADA assets in the confirmed snapshot UTxO [#2330](https://github.com/cardano-scaling/hydra/issues/2330).

- Add support for Blockfrost to the `hydra-tui` [#2032](https://github.com/cardano-scaling/hydra/issues/2032).

- `hydra-node` has a new endpoint `GET /head-initialization` which serves the timestamp of the last Head initialization.

- Tested with `cardano-node 10.5.3` and `cardano-cli 10.11.0.0`.

## [1.1.0] - 2025-10-28

- **BREAKING** Partial assets depositing works a bit differently now so you should consult our [API reference](https://hydra.family/head-protocol/api-reference).
  These changes also enable more complex scenarios when depositing and gives more flexibility to our users
  since hydra-node is capable of balancing the deposit tx outputs now. [#2180](https://github.com/cardano-scaling/hydra/issues/2180)

## [1.0.0] - 2025-10-08

- Add `--blockfrost-query-timeout` and `--blockfrost-retry-timeout` to configure
  Blockfrost API timeouts.
  See the [corresponding issue](https://github.com/cardano-scaling/hydra/issues/2058)
  and [PR](https://github.com/cardano-scaling/hydra/pull/2267) for more details.

- Fix bug where TUI would have out-of-date head status information
  after restarting on a rotated node (due to event rotation).

- Hydra API server responds with the correct `Content-Type` header `application-json`.

- Add `Environment` to `Greetings` message, enabling clients to access runtime settings.

- Bugfix for incorrect logic around fanning out with decommit/commit in progress

- Hydra node now rejects requests for incremental commits if provided UTxO is below the limit.

- Add API endpoint `POST /transaction` to submit transaction to the head.

- Improve HTTP API status codes for side-effecting endpoints to reflect operation outcome:
  - `POST /snapshot`: 200 on successful side-load, 400 on validation failure, 202 on timeout
  - `POST /decommit`: 200 on finalize, 400 on invalid/failed, 202 on timeout
  - `DELETE /commits/:txid`: 200 on recovered, 400 on failed, 202 on timeout
  - See [Issue #1911](https://github.com/cardano-scaling/hydra/issues/1911) and [PR #2124](https://github.com/cardano-scaling/hydra/pull/2124).

- Tested with `cardano-node 10.4.1` and `cardano-cli 10.8.0.0`.

- **BREAKING** Rename `--script-info` option to `--hydra-script-catalogue` in the `hydra-node` CLI.

- Label threads, queues and vars.

- **BREAKING** Enable handling client recover in all head states.
  - See [Issue #1812](https://github.com/cardano-scaling/hydra/issues/1812) and [PR #2217](https://github.com/cardano-scaling/hydra/pull/2217).
    > This enables clients (e.g. the TUI) to fully recover after event-log rotation.
  - The Checkpoint event, and consequently the EventLogRotated server output, now carry the full NodeState instead of just the HeadState.

- Optimistic approach to statefile corruption by just ignoring invalid JSON
  lines [#2253](https://github.com/cardano-scaling/hydra/issues/2253)

- Select next deposit for ReqSn using FIFO, based on deposit created.
  - See [#2263](https://github.com/cardano-scaling/hydra/issues/2263)

## [0.22.4] - 2025-08-05

- Fix API not correctly handling event log rotation. This was evident in not
  being able to use `/commit` although the head is initializing or outdated
  information in the `Greetings` message.

- Ignore snapshot signatures of already confirmed snapshots. This was previously
  resulting in the node waiting for the accompanying snapshot request and
  occurred when running heads with mirror nodes.

- Fix an internal persistent queue blocking after restart when it reached
  capacity.

- Timeout and retry broadcast of network messages after 3 seconds in case the
  `etcd` grpc server is not responsive. This should avoid build-up on the
  outbound persistent queue.

- Handle failing lease keep alive in network component and avoid bursts in
  heartbeating.

- Fix for blocking bug when broadcasting messages via etcd. See:
  https://github.com/cardano-scaling/hydra/issues/2167. This is not a full fix
  but is enough to resolve the problem until we can identify the central cause
  of the issue.

- Fix rotation log id consistency after restart by changing the rotation check to trigger only
when the number of persisted `StateChanged` events exceeds the configured `--persistence-rotate-after` threshold.
  * This also prevents immediate rotation on startup when the threshold is set to 1.
  * `Checkpoint` event ids now match the suffix of their preceding rotated log file and the last `StateChanged` event id within it,
  preserving sequential order and making it easier to identify which rotated log file was used to compute it.

## [0.22.3] - 2025-07-21

- Change behavior of `Hydra.Network.Etcd` to fallback to earliest possible
  revision if `last-known-revision` is missing or too old. This can happen if a
  node is down for a long time and the `etcd` cluster compacted the last known
  revision in the meantime
  [#2136](https://github.com/cardano-scaling/hydra/issues/2136).

- Don't keep around invalid transactions as they could lead to stuck Head.


- Add grafana monitoring to hydra nix and docker demos.

## [0.22.2] - 2025-06-30

* Fix wrong hydra-script-tx-ids in networks.json

## [0.22.1] - 2025-06-27

* Fix for bug where node got stalled at `ReplayingState` [#2089](https://github.com/cardano-scaling/hydra/issues/2089)

## [0.22.0] - 2025-06-17

- Tested with `cardano-node 10.1.4` and `cardano-cli 10.1.1.0`.

- Fix tutorial usage of `cardano-cli` and include download of `etcd`.

- Remove runtime dependency to `etcd` by embedding and shipping it with `hydra-node`.
  - New option `--use-system-etcd` to prefer the system etcd instead of the embedded one.

- Add file-based event log rotation support via optional `--persistence-rotate-after` command line option.

- **BREAKING** Update scripts to plutus 1.45.0.0.

- Hydra will now store etcd cluster information on the filesystem in directories content-addressed
  by the cluster configuration.

- **BREAKING** Fixed observation of deposit transactions:
  - Correctly ignore deposits with deadlines in the past or too soon in the future.
  - Replaced `--deposit-deadline` with `--deposit-period`.
  - To make sure a deposit is picked up ensure that `--deposit-period` is longer than `--contestation-period`.
  - Change persisted events of `hydra-node`.
  - Change to the `ReqSn` message in the Hydra network protocol
  - Added `DepositExpired` for when a deposit was deemed expired.

- Enable blockfrost integration for hydra-node.

- Fix head status in post abort greetings output.

- Add `UDP` and `S3` examples for `EventSource` and `EventSink` implementations to `hydra-node:examples`.

- Switch `hydra-chain-observer` to use `HeadObservation` when reporting observations to `hydra-explorer`.
  - Most observation types got changed quite a lot to match the previously used `OnChainTx`.
  - This introduces `ToJSON` and `FromJSON` instances on all observation types.
  - `CollectComObservation` and `ContestObservation` are made compatible with their `OnChainTx` counterparts.

- Enhanced the error message for `etcd` cluster ID mismatches by including detailed information about
  the expected peers versus peers loaded from the `hydra-node` arguments.

- Add API query (GET /head) to fetch the latest head state by a node and help introspecting the whole internal state.

- Provide `--network` option to hydra-node in order to use pre-published hydra scripts.

## [0.21.0] - 2025-04-28

- New metric for counting the number of active peers: `hydra_head_peers_connected`
- **BREAKING** Switch to using `etcd` internally to establish a reliable L2 network
  - New run-time dependency onto `etcd` binary
  - The peer network options to `hydra-node` (`--peer`) need to match across the Hydra network.
  - `--host` and `--port` have been removed and we now have `--listen` and `--advertise`.
  - `--listen` can be the same as the old `--host:--port` combined, but you may require `--advertise` to set your _public_ IP address and port combination, if you cannot bind to that on the local machine.
  - Adds `NetworkConnected` and `NetworkDisconnected` outputs which are most indicative of whether the L2 network is up or not.
  - Change `PeerConnected` and `PeerDisconnected` to indicate connectivity to `--peer` items and not the remote `node-id`.
  - Change `PeerHandshakeFailure` to a network-level `NetworkVersionMismatch`
  - Log outputs related to the network components changed significantly.
  - Persisted state (write ahead logs) of the network components changed significantly. The `<persistence-dir>/etcd` directory must not be lost or manual action to recover the L2 network (etcd cluster) with counter-parties needs to be taken.
  - To configure the `etcd` instance used internally, you may use `ETCD_` environment variables. For example, to switch auto-compaction to periodic retention of 7 days:
    ```
    ETCD_AUTO_COMPACTION_MODE=periodic
    ETCD_AUTO_COMPACTION_RETENTION=168h
    ```

- Changed default contestation period to 600 seconds and deposit deadline to 3600 seconds.

- Introduce an option to publish hydra scripts using blockfrost.

- Remove checks that rely on hydra-node's local state and trust on-chain data when we observe decrement/recover transactions.

- Fix a bug in increment observation where wrong deposited UTxO was picked up.

- Fix a bug where incremental commits / decommits were not correctly observed after restart of `hydra-node`. This was due to incorrect handling of internal chain state [#1894](https://github.com/cardano-scaling/hydra/pull/1894)

- Fix a bug where decoding `Party` information from chain would crash the node or chain observer.
  - A problematic transaction will now be ignored and not deemed a valid head protocol transaction.
  - An example was if the datum would contain CBOR instead of just hex encoded bytes.

- **BREAKING** Enable multi-party, networked "offline" heads by providing an `--offline-head-seed` option to `hydra-node`.
  - Drop `hydra-node offline` as a sub-command. Use `--offline-head-seed` and `--initial-utxo` options to switch to offline mode.

- Add support for "withdraw zero trick" transactions:
  - Any transaction with a `Rewarding` redeemer for a `Withdrawal` of `0 lovelace`, will be validated as if there would be a corresponding stake `RewardAccount` already registered.
  - No need to register the script's stake address before.

- Stream historical data from disk in the hydra-node API server.

- Record used and free memory when running `bench-e2e` benchmark.

- Submit observations to a `hydra-explorer` via optional `--explorer` option.

- Add API query (GET /snapshot/last-seen) to fetch the latest seen snapshot by a node and help identify non-cooperating peers.

- Bugfix: HeadFannedOut should always display the observed fanned-out UTxO instead of local confirmed snapshot.

- **BREAKING**
  - API Server does **NOT** serve the event history by default any more. Clients need to add a query parameter `?history=yes` in order to obtain the history.
  - Remove `GetUTxO` client input and corresponding `GetUTxOResponse`. There is already a way to query the `UTxO` in the Head with `GET /snapshot/utxo` query.
  - Renamed 'CommitFinalized' field 'theDeposit' to 'depositTxId'.
  - We now store the `time` in `StateEvent` which is a breaking change to our
  persistence loading

- Add a list of [clients](https://hydra.family/head-protocol/unstable/docs/clients) to the docs

- Add API command (POST /snapshot) which allows to adopt the given snapshot as the latest confirmed.
  * add new `SideLoadSnapshot` client input.
  * add new `LocalStateCleared` state changed event.
  * add new `SnapshotSideLoaded` server output.
  * add new `SideLoadSnapshotFailed` logic error.

## [0.20.0] - 2025-02-04

- **BETA** hydra-node now supports incremental commits in beta mode. We would like to test out this feature
  with the community members building on Hydra. This feature means you can commit funds to a Head while it is running.

- There is a new `--deposit-deadline` argument to hydra-node that determines the maximum time for the hydra-node to detect a deposit.
  After this time has passed users can recover a deposit in case it wasn't observed previously.

- **BREAKING** hydra-node accepts multiple `hydra-scripts-tx-id` as a comma-separated list, as the outcome of changes in the Hydra scripts publishing.

- Tested with `cardano-node 10.1.2` and `cardano-cli 10.1.1.0`.

- **BREAKING** Rewrite of the commit script in aiken:
  - This makes `abort` and `collectCom` transactions more efficient and results
    in a new maximum number of head participants being `8`.
  - Changes script hashes in `hydra-plutus`

- **BREAKING** Rewrite of the initial script in aiken and update to Plutus V3:
  - This makes the initial script smaller by 1337 bytes
  - Changes script hashes in `hydra-plutus`

- Fix the bug where commit endpoint drops withdraw redeemers [#1643](https://github.com/cardano-scaling/hydra/issues/1643)

- **BREAKING** Change to `SnapshotConfirmed` and `TxValid` server outputs, as
  well as to persisted `StateEvent` format:
  - Snapshots now contain the full transactions in `confirmed` and field names changed.
  - Persisted `StateChanged` events containing a snapshot changed consequently
    and are not backward compatible.
  - `TxValid` only refers to the transaction by id.
  - Overall this results in transactions still to be submitted once per client,
    but requires signifanctly less book-keeping on the client-side.

- Auto-debug and provide more information on `PlutusFailure` when validating transactions on L2 ledger.

- Bump docusaurus version

- Add blockfrost support to `hydra-chain-observer`, to follow the chain via Blockfrost API.

- Fix `bench-e2e single` benchmarks and only use `--output-directory` to keep
  the whole benchmark state.

- Add `inlineDatumRaw` to transaction outputs on the `hydra-node` API.

- Use different versions of mithril depending on the network (Sanchonet/Preview: Unstable, Mainnet/Preproduction: 2450.0)

- New websocket URL parameter `?address=...` to filter `SnapshotConfirmed`, `TxValid` and `TxInvalid` server outputs by address.

- Updated `hydra-tui` to handle `incremental commits`.

## [0.19.0] - 2024-09-13

- Tested with `cardano-node 9.1.1` and `cardano-cli 9.2.1.0`

- Switch L2 ledger to use the `Conway` era. [#1178](https://github.com/cardano-scaling/hydra/issues/1178)
  - Conway formatted transactions can be submitted to the `hydra-node`, while past eras are still supported (except deprecated features like protocol updates).
  - This includes support for `PlutusV3` scripts, but most of the governance-related features have no meaning in the Hydra L2.

- Added head id information into the Greetings message.

- Adds a manual recipient address entry to `hydra-tui` and fixes event handling. [#1607](https://github.com/cardano-scaling/hydra/pull/1607)

- Add a demo mode to hydra-cluster to facilitate network resilience tests [#1552](https://github.com/cardano-scaling/hydra/pull/1552)

## [0.18.1] - 2024-08-15

- New landing page and updated documentation style. [#1560](https://github.com/cardano-scaling/hydra/pull/1560)

- Fixed pruning of transactions when a snapshot is requested:
  - Before, the `hydra-node` would re-apply transactions in the wrong order which made already "seen" transactions (indicated by `TxValid`) disappear from the local ledger state and also not get snapshotted.
  - Details in [#1559](https://github.com/cardano-scaling/hydra/pull/1559)

- Add TLS support for the API server via `--tls-cert` and `--tls-key` command line options. [#1555](https://github.com/cardano-scaling/hydra/pull/1555)

## [0.18.0] - 2024-08-08

- Tested with `cardano-node 9.1.0` and `cardano-cli 9.2.1.0`.

- Fixed fee calculation of the internal wallet when the network switches to `Conway`. This allows heads to be opened in `Babbage` and closed/finalized in `Conway`. [#1553](https://github.com/cardano-scaling/hydra/pull/1553)

- **BREAKING** Changes to the `hydra-node` API `/commit` endpoint [#1463](https://github.com/cardano-scaling/hydra/pull/1463):
  - Removed the check that prevented committing UTxOs from an internal `hydra-node` wallet.
  - `SpendingNodeUtxoForbidden` error was removed.

- **BREAKING** Add capability to move UTxO out of an open Head to the Cardano main chain: [#1344](https://github.com/cardano-scaling/hydra/pull/1344)
  - Submitting a decommit transaction to `POST /decommit` or as `Decommit` command through websocket, requests removal of this transactions' outputs from the head.
  - When successful, `DecommitApproved` and `DecommitFinalized` indicate that all outputs are made available on the layer one.
  - Invalid transactions are explained through a `DecommitInvalid` server output.

- Change `--start-chain-from` to always use the newer point when also a head state is known. [#1471](https://github.com/cardano-scaling/hydra/pull/1471)

- Moved several pages from "core concepts" into the user manual and developer docs to further improve user journey. [#1486](https://github.com/cardano-scaling/hydra/pull/1486)

- Offline mode of `hydra-node` uses `--node-id` to derive an artificial offline `headId`. [#1551](https://github.com/cardano-scaling/hydra/pull/1551)

## [0.17.0] - 2024-05-20

- **BREAKING** Change `hydra-node` API `/commit` endpoint for committing from scripts [#1380](https://github.com/cardano-scaling/hydra/pull/1380):
  - Instead of the custom `witness` extension of `UTxO`, the endpoint now accepts a _blueprint_ transaction together with the `UTxO` which is spent in this transaction.
  - Usage is still the same for committing "normal" `UTxO` owned by public key addresses.
  - Spending from a script `UTxO` now needs the `blueprintTx` request type, which also unlocks more involved use-cases, where the commit transaction should also satisfy script spending constraints (like additional signers, validity ranges etc.)

- _DEPRECATED_ the `GetUTxO` client input and `GetUTxOResponse` server output. Use `GET /snapshot/utxo` instead.

- Update navigation and re-organized documentation website https://hydra.family [#1440](https://github.com/cardano-scaling/hydra/pull/1440)
  - Updated logos
  - Removed localization as it got outdated and on-demand site translation tools exist.

- Add `GET /snapshot/utxo` API endpoint to query confirmed UTxO set on demand. [#1398](https://github.com/cardano-scaling/hydra/pull/1398)
  - Always responds with the last confirmed UTxO

- Set [CORS](https://fetch.spec.whatwg.org/#http-cors-protocol) headers on `hydra-node` API to allow requests from any origin `*`. [#1434](https://github.com/cardano-scaling/hydra/pull/1434)

- `hydra-node` logs will now report `NetworkEvents` to distinguish between `ConnectivityEvent`s and `ReceivedMessage`s on the network. [#1396](https://github.com/cardano-scaling/hydra/pull/1396)

- Hydra now uses a versioned protocol for handshaking. In the event of a node
  attempting to connect using a different version of the networking protocol, a
  `HandshakeFailure` event will be recorded in the logs and sent as a server
  output on the API. [#1381](https://github.com/cardano-scaling/hydra/pull/1381)

- Make `hydra-cluster --devnet` more configurable
  - Now it is idle by default again and a `--busy` will make it busy respending the same UTxO. [#1420](https://github.com/cardano-scaling/hydra/pull/1420)

## [0.16.0] - 2024-04-03

- Tested with `cardano-node 8.9.0`, `cardano-cli 8.20.3.0` and `mithril 2412.0`.

- **BREAKING** Change to the `hydra-node` logs, monitoring and removal of `log-filter` executable:
  - Renamed the `Event` data types to `Input` and consequently log items like `BeginEvent` to `BeginInput`.
  - Changed structure of `LogicOutcome` entries.
  - Added node-level log entry when an input was `DroppedFromQueue`.
  - In course of this, the `log-filter` executable was removed as nobody is actively using it and other off-the-shelf utilities to manipulate structured JSON logs (`jq` is already quite powerful) are recommended.
  - Renamed prometheus metric `hydra_head_events -> hydra_head_inputs`.

- **BREAKING** Hydra scripts changed due to updates in the `plutus` toolchain:
  - Overall slight increase in script size.
  - 50% less memory usage in `close` and `contest` transactions.
  - Slightly less memory usage in `abort` and may be possible with 6 parties now.

- **BREAKING** Transaction serialization on hydra-node api and persisted data changed.

- Introduce `EventSource` and `EventSink` interfaces in `hydra-node`:
  - These handles can now be used as "extension points" to make the `hydra-node` store and load its state differently or expose `StateEvent`s to other, external services.
  - Internal refactoring of persistence mechanism as event source and sink in a backward-compatible way.
  - More details can be found in [ADR21](https://hydra.family/head-protocol/adr/21/)

- Add metadata to identify Hydra protocol transactions created by `hydra-node`.

- Provide more details about why a command failed. Added the state of the head logic at the point of failure.

- Fix a bug where the `hydra-node` would not correctly observe a contest transaction and fail to fanout a head [#1260](https://github.com/cardano-scaling/hydra/issues/1260).

- Add `contestationDeadline` to the `HeadIsContested` output on the `hydra-node` API.

- Add `--sanchonet` option to `hydra-cluster` binary.

- Enhance `hydra-cluster --devnet` mode to produce a constant stream of snaphsots by re-spending the sandbox UTxO.

- Reduce cost of transactions submitted by `hydra-node` by better estimating fees in internal wallet [#1315](https://github.com/cardano-scaling/hydra/pull/1315).

- Fix conversion of `Conway` blocks in `hydra-node` and `hydra-chain-observer`. This also includes tests that verify `hydra-node` working on Conway networks like `sanchonet` and the `hydra-explorer` observing heads on `sanchonet`.

- Fix a bug in the contest observation where contesters were extracted from the
  input instead of the output datum.
  [#1266](https://github.com/cardano-scaling/hydra/pull/1266)


## [0.15.0] - 2024-01-18

- Tested with `cardano-node 8.7.3` and `cardano-cli 8.17.0.0`.

- **BREAKING** Remove head state from `hydra-node` chain layer [#1196](https://github.com/cardano-scaling/hydra/pull/1196):
  - Not maintain head state in the chain layer anymore and all decision making (whether it's "our" head) is now fully contained in the logic layer.
  - This is a breaking change on the persisted `state` file, which now only stores so-called `spendableUTxO`. This raises a `PersistenceException` if an incompatible `state` file is loaded.
  - Heads need to be closed before upgrade to this version, as wiping `state` in the `--persistence-dir` is needed.
  - This also changes the `NodeOptions` log output because of internal restructuring of chain layer configuration.

- New `offline` sub-command for `hydra-node` [#1118](https://github.com/cardano-scaling/hydra/pull/1118), [#1222](https://github.com/cardano-scaling/hydra/pull/1222):
  - Initializes a head with given `--initial-utxo` parameter, and does not connect to the Cardano network.
  - Transactions submitted on the L2 are validated as usual, where the offline chain simulates time passing in slots.
  - The `--ledger-genesis` option allows to give a shelley genesis file to configure start time and slot length of the simulated chain time.

- Prepare `hydra-node` for the upcoming `Conway` hard-fork [#1177](https://github.com/cardano-scaling/hydra/issues/1177):
  - Interactions with `cardano-node` are updated to work in both, `Babbage` and `Conway` era.
  - Unsupported eras are reported as error when starting.

- Add a default for `hydra-node --node-id` (`"hydra-node-1"`) to simplify configuration.

- Fix `hydra-node` API reference & schema for `/protocol-parameters` [#1241](https://github.com/cardano-scaling/hydra/pull/1241). This now matches the JSON returned by `cardano-cli query protocol-parameters`, expected at `hydra-node --ledger-protocol-parameters` and produced by the API endpoint.

- The `hydra-cluster` binary can bootstrap `cardano-node`s running on public networks using `mithril-client`.

- **BREAKING** Internal changes to `hydra-cardano-api`:
  - Renamed `HasMultiAsset` type class to `IsMaryEraOnwards`. Use `maryEraOnwards` to produce witnesses for features from mary onwards.
  - Renamed `HasScriptData` type class to `IsAlonzoEraOnwards`. Use `alonzoEraOnwards` to produce witnesses for features from alonzo onwards.
  - Renamed `HasInlineDatums` type class to `IsBabbageEraOnwards`. Use `babbageEraOnwards` to produce witnesses for features from babbage onwards.

## [0.14.0] - 2023-12-04

- **BREAKING** Multiple changes to the Hydra Head protocol on-chain:

  - Sign the head identifier as part of snapshot signature and verify it
    on-chain. This fully addresses security advisory
    [CVE-2023-42806](https://github.com/cardano-scaling/hydra/security/advisories/GHSA-gr36-mc6v-72qq).

  - Switched to using inline datums instead of (optionally) published datums in
    transactions. [#1162](https://github.com/cardano-scaling/hydra/pull/1162)

  - Upgraded toolchain to GHC 9.6 and a newer `plutus-tx` compiler.

- **BREAKING** Internal persisted chain state serialization changed when
  switching to inline datums. Make sure to close heads before and wipe the
  `--persistence-dir` before using this `hydra-node` version.

- **BREAKING** Introduced messages resending logic in the `Network` layer to
  improve reliability in the face of connection issues.
  [#188](https://github.com/cardano-scaling/hydra/issues/188) This persists
  network messages on disk in order to gracefully handle crashes and detects
  inconsistencies between persisted state and configuration.

- Increased maximum number of parties to 5. This is possible to small
  optimizations on the Head protocol transactions.

- Removed hard-coded deposit of 2₳ from internal wallet. Now the wallet does
  only use as much deposit for script outputs as minimally needed and reduces
  the Ada locked throughout a head life-cycle.
  [#1176](https://github.com/cardano-scaling/hydra/pull/1176)

- Clients are notified when head initialization is ignored via a new
  `IgnoredHeadInitializing` API server output. This helps detecting
  misconfigurations of credentials and head parameters (which need to match).
  [#529](https://github.com/cardano-scaling/hydra/issues/529)

- Removed false positive `PostTxOnChainFailed` error from API outputs when the
  collect transaction of another `hydra-node` was "faster" than ours.
  [#839](https://github.com/cardano-scaling/hydra/issues/839)

- Hydra node API `submit-transaction` endpoint now accepts three types of
  encoding: Base16 encoded CBOR string, a TextEnvelope with CBOR and full JSON.
  [#1111](https://github.com/cardano-scaling/hydra/issues/1111)

- Improved `gen-hydra-keys` command to not overwrite keys if they are present
  already. [#1136](https://github.com/cardano-scaling/hydra/issues/1136)

- Add a `hydra-chain-observer` executable to subscribe to a chain and just
  observe Hydra Head transactions (with minimal information right now).
  [#1158](https://github.com/cardano-scaling/hydra/pull/1158)

- Fixed `hydra-tui` key bindings for exiting in dialogs.
  [#1159](https://github.com/cardano-scaling/hydra/issues/1159)

## [0.13.0] - 2023-10-03

- **BREAKING** Update to plutus 1.9. This changes the script hashes.

- Query at the tip for local cardano-node queries. We witnessed these queries
  failing in case of a rollback and always querying at the tip seems to fix
  this.

- **BREAKING** Changes to `hydra-plutus` scripts.

- Add option to draft a commit tx using inline datums.

- Remove hydra-tools package. Move functionality to generate hydra keys to the
  hydra-node executable.

- Changes to `hydra-node` state persistency:
  - Remove the recursive definition of the chain state.
  - This makes the event store more lightweight and easier to read and work with.

## [0.12.0] - 2023-08-18

- **BREAKING** Support new cardano-node version 8.1.2

  - Updated chain client and internal (layer 2) ledger versions to support the
    new cardano-node versions. No direct impact on hydra clients expected.

  - The JSON encoding of transaction as used at the `hydra-node` API changed
    slightly. Verification key witnesses (`keys` field `witnesses`) are not
    double wrapped cbor arrays anymore. Do not rely on this serialization as we
    will change this again into a more "cardanonical" form. Alternative: Use
    `cbor=true` query parameter to receive full CBOR encoded transactions.

  - The last stable cardano-node version 1.35.7 is not compatible anymore.

- **BREAKING** Remove the _DEPRECATED_ `Commit` websocket command to commit
  funds held by the `hydra-node` internal wallet. Use the external commit feature
  instead. Also rename the `ErrNoFuelUTxOFound` to `ErrNoUTxOFound`.

- **BREAKING** Changes to Hydra scripts due to upgrading our plutus version and
  toolchain to GHC 9.2.8.

- **BREAKING** Change persisted state to be a sequence of events instead. This
  increases the performance of the `hydra-node` as less data needs to be written
  and prepares internal architecture for more event-sourced improvements.

- **BREAKING** Introduce authenticated network messages [#965](965):

  - Peers will sign messages before broadcasting them to other peers,
    and verify signature of received messages is from a known party
    and of course valid.

- **BREAKING** Layer 2 protocol change:

  - Wait for all transactions requested in a snapshot to be seen before
    acknowledging it, and only send transaction ids in snapshot requests.

- Enhanced `hydra-node` api.

  - New HTTP endpoint (`POST /cardano-transaction`) to submit a transaction on L1.

  - `Greetings` message now contains also the hydra-node version.

  - New HTTP endpoint (`GET /protocol-parameters`) which provides the current protocol parameters.

- Fixed a bug in `hydra-node` (on-chain protocol not affected) where
  multisignature verification would silently ignore certain keys in case the
  list of verification keys is not of same length as the list of signatures.

- Fixed a bug in the `hydra-tui` dialogs where recipient and UTxO to spend where
  not correctly selected.

- **BREAKING** Changes to `hydra-cardano-api`:

  - Drop `UsingRawBytesHex` as it is available upstream in `cardano-api` now.
  - Remove `totalExecutionCost` as `cardano-ledger` provides `getMinFeeTx` now.
  - Add `BundledProtocolParameters` pattern for latest `Era` to `Hydra.Cardano.Api`.
  - Add `ledgerEraVersion` for the latest `Era` en-/decoder version.
  - Change `minUTxOValue` to take `BundledProtocolParameters`.
  - Add `fromLedgerMultiAsset` helper as transactions only `mint` `MultiAsset`.

- Created `hydra-plutus-extras` package to re-use some utilities better between
  packages.

## [0.11.0] - 2023-06-30

- **BREAKING** Allow to commit multiple `UTxO` [#774](774)
    - This changes `hydra-plutus` scripts to allow commit transactions which
      spend multiple UTxOs into a Hydra head.
    - Removes the `MoreThanOneUTxOCommitted` server output on the API.

- Support commits from external wallets [#215](215)
    - Added the `/commit` HTTP endpoint to the `hydra-node` for creating a draft
      `commit` transaction to commit requested UTxO into a head. This
      transaction can be signed and submitted to the network by the hydra client
      now instead of `hydra-node`.
    - Commits via `/commit` also allow to commit scripts into a Hydra Head. For
      that, the UTxO entry in the HTTP request needs to provide a `witness` with
      script, datum, and redeemer to be used.
    - Removed the need to mark fuel when using external commits. Fees for Hydra
      protocol transactions are paid the largest UTxO held by the internal
      wallet if no marked fuel UTxO is present.
    - **BREAKING** The `hydra-tui` now uses the `--cardano-signing-key` to
      select and commit "external funds" to the Hydra Head. If you have used
      this in the past, make sure to **not use the same key** as also given to
      the `hydra-node`.

- _DEPRECATED_ the `Commit` command to commit funds held by the `hydra-node`
  internal wallet. Use the external commit feature instead.

- Make `hydra-node` support time bounded transactions [#196](196)
    - The `hydra-node` tracks time as seen on-chain and uses that to validate
      any transactions, which can now use validity ranges the same way as on the
      layer 1.
    - Added current chain slot and time to log outputs.

- **BREAKING** API output `SnapshotConfirmed` only includes transaction ids.
  [#922](922)

- **BREAKING** Changed to the persisted state by removing the plutus scripts
  from the internal chain state and adding the `headId`.
    - Only the `seedTxIn` parameter is stored and the `hydra-node` will use the
      script compiled into it instead.
    - This substantially decreases the size of persisted and logged data.

- **BREAKING** Changed the `hydra-node` command line options:
    - Removed `--ledger-genesis` argument and query this information from
      `cardano-node` now. [#863](863)
    - `--version` always displays git revision (SHA) alongside the declared
      version. [#849](849)

- Fixed a bug where `hydra-node` resets head state when replaying close of
  another head. [#927](927)

- Fixed a bug where `hydra-node` reports a wrong head status on `Greetings`
  after restart. [#932](932)

- Decreased verbosity of logs [#849](849)
    - `BeginEvent`/`EndEvent` and `BeginEffect`/`EndEffect` log items are now
      paired using a numeric `eventId` and `effectId`.
    - Repurpose `log-filter` executable to compute duration of events and
      effects.

[#774]: https://github.com/cardano-scaling/hydra/pull/774
[#215]: https://github.com/cardano-scaling/hydra/issues/215
[#196]: https://github.com/cardano-scaling/hydra/issues/196
[#922]: https://github.com/cardano-scaling/hydra/pull/922
[#863]: https://github.com/cardano-scaling/hydra/pull/863
[#849]: https://github.com/cardano-scaling/hydra/issues/849
[#927]: https://github.com/cardano-scaling/hydra/issues/927
[#932]: https://github.com/cardano-scaling/hydra/issues/932
[#849]: https://github.com/cardano-scaling/hydra/pull/859

## [0.10.0] - 2023-05-11

- Make `hydra-node` compatible to mainnet [#713](713)

    - **BREAKING** Change to command line options: Replaced `--network-id` with
      `--mainnet` or `--testnet-magic`.

    - Hard-coded temporary **limit of 100 ADA** for commits to a head on
      mainnet. This will be incraeased or be made configurable in future
      versions.

- **BREAKING** Change in internal handling of rollbacks. Now, the `hydra-node`
  does only rollback it's low level state and not report when a rollback
  happened, under the optimistic assumption that the Hydra protocol transactions
  are still applicable and the Head is unaffected by the rollback. This was
  needed to avoid [#784](784) and will be further improved in [#185](185). This
  removes `RolledBack` server output from the API and also changes the log
  format of the internal `Rollback` event.

- Reject commits of `UTxO` containing `ReferenceScript` to avoid a head not
  being finalizable by the `hydra-node`. The layer 1 scripts still accept these
  outputs, but we would not be able to automatically finalize a head which was
  opened from commits with reference scripts. Reference scripts on the layer 2
  ledger (e.g. included in transactions via `NewTx`) are non-problematic.
  [#766](766)

- All participants try to collect once seeing the last `commitTx`. [#786](786)
  This may lead to misleading errors on the logs about not being able to post
  collect transactions (see also [#839](839)).

- The `hydra-node` detects misconfiguration and mismatch of command line options
  with persisted state. [#764](764)

- Fixed a bug where the `hydra-node` would crash sometimes when the
  `cardano-node` switches onto a fork, which is a common event on mainnet.
  [#560](560)

- **BREAKING** Hydra scripts changed, need to use new `--hydra-scripts-tx-id`

    - Check contract continuity of state machine, i.e. that the output with the
      state datum and ST is actually owned by vHead.
      [#777](777)

    - Collect the right value in `collect` transactions (had been dropped for cost
      reasons, but found a constant cost way to do it).

    - The right `headId` is enforced in `commit` transactions.

    - Updated `plutus-tx` tool-chain. This also resulted in changed return type
      of `validatorScript` functions of script modules to `SerialisedScript`.
      [#826](826)

    - Use of a custom script context for `vInitial` and `vCommit` validators to
      reduce cost of transactions again.
      [#825](825)

    - The hydra scripts are persisted in `hydra-plutus/scripts` and golden tests
      ensure they are not changed accidentally.
      [#772](772)

- **BREAKING** Changes to `hydra-node` API

    - Configurable API using query parameters. [#380](380) Clients can decide to:
        - Skip observing history of events before they connected
        - View the transactions in the server output encoded as CBOR
        - Prevent utxo display in `SnapshotConfirmed` server outputs
          [#808](808)

    - `Greetings` message is now only sent last (after replaying history) on
      connection and added additional information [#813](813):
        - `headStatus` - representing current hydra head status
        - `snapshotUtxo` - containing UTxOs and updating on each `SnapshotConfirmed` message

    - Updated `hydra-tui` to handle `Greetings` message accordingly. Make sure
      to use the same version.

    - Reference scripts in the `hydra-node` API (e.g. on `NewTx`) are not
      decodable when using `SimpleScriptV2` envelope anymore (just use
      `SimpleScript`).

- Versioned the documentation website, now the last released, stable is the
  default available at https://hydra.family/head-protocol, while the
  bleeding-edge from `master` branch is available at
  https://hydra.family/head-protocol/unstable. [#803](803) [#805](805) [#783](783)

- Add the
  [specification](https://github.com/cardano-scaling/hydra/tree/master/spec) to
  the repository and
  [website](https://hydra.family/head-protocol/dev/specification).
  [#693](693)

- Disabled `aarch64-darwin` support, until a `cardano-node` for this platform is
  also available.

- Use the server-provided `timestamp` of messages in the `hydra-tui`. [#837](837)

- **BREAKING** Changes to `hydra-cardano-api` [#826](826):
  - Removed `HasPlutusScriptVersion` and `plutusScriptVersion` with upstream version from `cardano-api`.
  - Renamed `getScriptData` to `txOutScriptData` to not conflict with the new function in `cardano-api`.
  - Changed `toScriptData`, `toLedgerData`, `fromLedgerData`,
    `txOutScriptData` and `lookupScriptData` to return or require a
    `HashableScriptData` instead.
  - Added `fromScriptData` generic conversion function.
  - Changed signature of `totalExecutionCost` to be more clearly `Babbage` era specific.
  - Changed `fromPlutusScript` to take new `SerialisedScript` type (it's just an alias now).
  - Added `genTxIn` and `arbitrary` instance for `TxIn`.
  - Added `getChainPoint`.

[185]: https://github.com/cardano-scaling/hydra/issues/185
[380]: https://github.com/cardano-scaling/hydra/issues/380
[693]: https://github.com/cardano-scaling/hydra/issues/693
[713]: https://github.com/cardano-scaling/hydra/issues/713
[764]: https://github.com/cardano-scaling/hydra/pull/764
[766]: https://github.com/cardano-scaling/hydra/pull/766
[772]: https://github.com/cardano-scaling/hydra/pull/772
[777]: https://github.com/cardano-scaling/hydra/pull/777
[783]: https://github.com/cardano-scaling/hydra/pull/783
[784]: https://github.com/cardano-scaling/hydra/issues/784
[786]: https://github.com/cardano-scaling/hydra/pull/786
[803]: https://github.com/cardano-scaling/hydra/pull/803
[805]: https://github.com/cardano-scaling/hydra/pull/805
[808]: https://github.com/cardano-scaling/hydra/pull/808
[813]: https://github.com/cardano-scaling/hydra/pull/813
[825]: https://github.com/cardano-scaling/hydra/pull/825
[826]: https://github.com/cardano-scaling/hydra/pull/826
[826]: https://github.com/cardano-scaling/hydra/pull/826
[837]: https://github.com/cardano-scaling/hydra/issues/837
[839]: https://github.com/cardano-scaling/hydra/issues/839

## [0.9.0] - 2023-03-02

:dragon_face: Renamed the repository from `hydra-poc` to [`hydra`](https://github.com/cardano-scaling/hydra)!

### Changes to `hydra-node`

- **BREAKING** Changes in the persistence format
  [#725](https://github.com/cardano-scaling/hydra/pull/725),
  [#745](https://github.com/cardano-scaling/hydra/pull/745).

- **BREAKING** Changes to the API:
  + Removed `TxSeen` and `TxExpired` server outputs. Use the `TxValid` and
    `TxInvalid` responses instead.
  + All participants now see `TxValid` for all valid transactions (it replaces `TxSeen`).
  + Renamed `ReadyToCommit -> HeadIsInitializing`
  + Added a `headId` to most server outputs. [#678](https://github.com/cardano-scaling/hydra/pull/678)
  + Added a `timestamp` and a monotonic `seq`uence number. [#618](https://github.com/cardano-scaling/hydra/pull/618)

- **BREAKING** Addressed short-comings in `hydra-plutus` scripts
  [#452](https://github.com/cardano-scaling/hydra/pull/452) and improved their
  performance / reduced cost
  [#652](https://github.com/cardano-scaling/hydra/pull/652),
  [#701](https://github.com/cardano-scaling/hydra/pull/701),
  [#709](https://github.com/cardano-scaling/hydra/pull/709). Roughly the cost of
  transactions according to our
  [benchmarks](https://hydra.family/head-protocol/benchmarks/transaction-cost/)
  changed:

  + Init increased by 10%.
  + Commit reduced by 50%.
  + Collect reduced by 30%.
  + Close reduced by 0.2-0.3₳
  + Contest reduced by 0.1-0.2₳.
  + Abort reduced by 0.1-0.3₳.
  + Fanout reduced by 0.2-0.3₳.

- **BREAKING** Change the way contestation period and deadline are handled:
  + There is a new hydra-node flag `--contestation-period` expressed in seconds
    to control the close tx validity bounds as well as determine the
    contestation deadline. For example, with `--contestation-period` 60s, the
    node will close the head 60s after submitting the close transaction and
    other parties will have another 60s to contest. This means the deadline may
    be up `2 * --contestation-period` after a close transaction.
    [#615](https://github.com/cardano-scaling/hydra/pull/615) and
    [ADR21](https://hydra.family/head-protocol/adr/21/)
  + If hydra-node receives a `init` transaction with _not matching_
    `--contestation-period` then this tx is ignored which implies that all
    parties need to agree on the same value for contestation period.
  + Removed `contestationPeriod` from the `Init` API request payload.
  + The deadline get's pushed by `--contestation-period` **on each** contest
    transaction. [#716](https://github.com/cardano-scaling/hydra/pull/716)

- Change the way the internal wallet initializes its state.
  [#621](https://github.com/cardano-scaling/hydra/pull/621)
  + The internal wallet does now always query ledger state and parameters at the
    tip. This should fix the `AcquireFailure` issues.

- Added `NoFuelUTXOFound` error next to the already existing `NotEnoughFuel`.
  Previously the node would fail with `NotEnoughFuel` when utxo was not found.
  Now `NotEnoughFuel` is used when there is not enough fuel and
  `NoFuelUTXOFound` when utxo was not to be found.

- Added support have `hydra-node` to start following the chain from _genesis_ by
  setting `--start-chain-from 0`.

- Added script sizes to `hydra-node --script-info` and published transaction
  cost benchmarks.

- Changes to the logs:
  + HeadLogic `Outcome` is now being logged on every protocol step transition.
  + Added intermediate `LastSeenSnapshot` and extended `RequestedSnapshot` seen snapshot states.
  + Changed wallet-related logs of `BeginInitialize`, `EndInitialize` and added
    `SkipUpdate`.

### Changes to `hydra-cardano-api`

- **BREAKING** Remove `Hydra.Cardano.Api.SlotNo` module.
- **BREAKING** Replace `fromConsensusPointHF` with `fromConsensusPointInMode` and
  `toConsensusPointHF` with `toConsensusPointInMode`.
- Re-export new `AcquiringFailure` type from `cardano-api`.
- Add `fromPlutusCurrencySymbol` conversion function.
- Introduce new `Hydra.Cardano.Api.Pretty` module and move functions
  `renderTx`, `renderTxWithUTxO` and `renderTxs` from `hydra-node` package to
  this new module.

### Other changes

- `hydra-cluster` executable can be used to provide a local cardano "network"
  with `--devnet` argument

- Switched to using [nix flakes](https://nixos.wiki/wiki/Flakes) and
  [CHaP](https://input-output-hk.github.io/cardano-haskell-packages/all-packages/)
  + Makes configuration of binary-caches easier to discover (you get asked about adding them).
  + Will make bumping dependencies (e.g. cardano-node) easier.
  + Build commands for binaries and docker images change, see updated [Contribution Guidelines](https://github.com/cardano-scaling/hydra/blob/master/CONTRIBUTING.md)

## [0.8.1] - 2022-11-17

- **BREAKING** Implemented [ADR18](https://hydra.family/head-protocol/adr/18) to keep only a single state:
  + The `hydra-node` now only uses a single `state` file in `--persistence-dir` to keep it's state.
  + The `chainState` does not include read-only chain context information anymore (is smaller now).
  + Include the `chainState` in `InvalidStateToPost` errors.
  + Moved received transaction ids into `RolledForward` log message.
  + Reduce log size by removing ChainContext. [#598](https://github.com/cardano-scaling/hydra/issues/598)

- **BREAKING** Changed internal wallet logs to help with debugging [#600](https://github.com/cardano-scaling/hydra/pull/600)
  + Split `ApplyBlock` into `BeginUpdate` and `EndUpdate`
  + Split `InitializedWallet` into `BeginInitialize` and `EndInitialize`

- After restarting `hydra-node`, clients will receive the whole history.  [#580](https://github.com/cardano-scaling/hydra/issues/580)
  + This history will be stored in the `server-output` file in `--persistence-dir`.
  + Clients should use `Greetings` to identify the end of a [restart/replay of events](https://hydra.family/head-protocol/docs/api-behavior#replay-of-past-server-outputs).

- Fixed observing the chain for Hydra L1 transactions after restart. [599](https://github.com/cardano-scaling/hydra/issues/599)

- `hydra-cardano-api` now published on [Cardano Haskell Packages (CHaP)](https://input-output-hk.github.io/cardano-haskell-packages/package/hydra-cardano-api-0.8.0/). [#504](https://github.com/cardano-scaling/hydra/issues/504)

## [0.8.0] - 2022-10-27

- **BREAKING** Hydra keys now use the text envelope format.
  + `hydra-tools` executable now produces keys in the same format as cardano keys so this should make key handling simpler.
  +  Take a look at the [example](https://github.com/cardano-scaling/hydra/blob/master/docs/docs/getting-started/quickstart.md#hydra-keys) on how to use `hydra-tools` to generate necessary hydra keys.

- **BREAKING** hydra-node command line flag `--node-id` is now mandatory.
  + Instead of `Host` we are using the `node-id` in the server messages like + `PeerConnected/Disconnected` which are also used in
  + the TUI to distinguish between different connected peers.
  + This also changes the way how `NodeId`s are represented on the API.

- **BREAKING** Keep track of `contestationDeadline` instead of `remainingContestationPeriod` and fix `ReadyToFanout`. [#483](https://github.com/cardano-scaling/hydra/pull/483)
  + Clients can now rely on `ReadyToFanout`, such that sending a `Fanout` input after seeing this output will never be "too early".
  + The `HeadIsClosed` server output now contains the deadline instead of the remaining time.
  + See `hydra-tui` for an example how to use the `contestationDeadline` and `ReadyToFanout`.
  + See [ADR20](./docs/adr/2022-08-02_020-handling-time.md) for details and the rationale.

- **BREAKING** Several changes to the API:
  + The `InitialSnapshot` only contains the `initialUTxO` as the rest of the information was useless. [#533](https://github.com/cardano-scaling/hydra/pull/533)
  + Renamed `CannotSpendInput -> InternalWalletError` and `CannotCoverFees -> NotEnoughFuel`. [#582](https://github.com/cardano-scaling/hydra/pull/582)

- **BREAKING** Changed logs to improve legibility and trace on-chain posting errors. [#472](https://github.com/cardano-scaling/hydra/pull/472)
  + Strip chain layer logs to only contain `TxId` instead of full transactions in the nominal cases.
  + Renamed log entry prefixes `Processing -> Begin` and `Processed -> End`.
  + Added `PostingFailed` log entry.

- **BREAKING** The `hydra-cluster` executable (our smoke test) does require `--publish-hydra-scripts` or `--hydra-scripts-tx-id` now as it may be provided with pre-published hydra scripts.

- The `hydra-node` does persist L1 and L2 states on disk now: [#187](https://github.com/cardano-scaling/hydra/issues/187)
  + New `--persistence-dir` command line argument to configure location.
  + Writes two JSON files `headstate` and `chainstate` to the persistence directory.
  + While introspectable, modification of these files is not recommended.

- *Fixed bugs* in `hydra-node`:
  + Crash after `3k` blocks because of a failed time conversion. [#523](https://github.com/cardano-scaling/hydra/pull/523)
  + Internal wallet was losing memory of spent fuel UTxOs in presence of transaction failures. [#525](https://github.com/cardano-scaling/hydra/pull/525)
  + Node does not see some UTxOs sent to the internal wallet on startup. [#526](https://github.com/cardano-scaling/hydra/pull/526)
  + Prevent transactions from being resubmitted for application over and over. [#485](https://github.com/cardano-scaling/hydra/pull/485)

- Prevent misconfiguration of `hydra-node` by logging the command line options used and error out when:
  + provided number of Hydra parties exceeds a known working maximum (currently 4)
  + number of provided Cardano and Hydra keys is not the same

- Added a `hydra-tools` executable, to help with generating Hydra keys and get hold of the marker datum hash. [#474](https://github.com/cardano-scaling/hydra/pull/474)

- Compute transaction costs as a "min fee" and report it in the [tx-cost benchmark](https://hydra.family/head-protocol/benchmarks/transaction-cost/).

- Update [hydra-node-options](https://hydra.family/head-protocol/docs/getting-started/quickstart/#hydra-node-options) section in docs.

- Publish test results on [website](https://hydra.family/head-protocol/benchmarks/tests/hydra-node/hspec-results). [#547](https://github.com/cardano-scaling/hydra/pull/547)

- Improved `hydra-tui` user experience:
  + Fixed too fast clearing of errors and other feedback [#506](https://github.com/cardano-scaling/hydra/pull/506)
  + Introduced a pending state to avoid resubmission of transactions [#526](https://github.com/cardano-scaling/hydra/pull/526)
  + Can show full history (scrollable) [#577](https://github.com/cardano-scaling/hydra/pull/577)

- Build & publish static Linux x86_64 executables on each [release](https://github.com/cardano-scaling/hydra/releases/tag/0.8.0) :point_down: [#546](https://github.com/cardano-scaling/hydra/pull/546)

## [0.7.0] - 2022-08-23

- **BREAKING** Switch to `BabbageEra` and `PlutusV2`.
  + `hydra-cardano-api` now uses `Era = BabbageEra` and constructs `PlutusV2` scripts.
  + `hydra-plutus` scripts now use the `serialiseData` builtin to CBOR encode data on-chain.
  + `hydra-node` now expects `BabbageEra` blocks and produces `BabbageEra` transactions.
  + `hydra-cluster` now spins up a stake pool instead of a BFT node (not possible in `Praos` anymore).
  + As a consequence, the Hydra scripts in `hydra-plutus` have now different script hashes.

- **BREAKING** Use reference inputs and reference scripts in `abort` transaction.
  + Need to provide a `--hydra-scripts-tx-id` to the `hydra-node` containing the current (`--script-info`) Hydra scripts.
  + Added the `publish-scripts` sub-command to `hydra-node` to publish the current Hydra scripts.

- Added a `hydra-cluster` executable, which runs a single scenario against a known network (smoke test) [#430](https://github.com/cardano-scaling/hydra/pull/430) [#423](https://github.com/cardano-scaling/hydra/pull/430).

- Use deadline when posting a `fanoutTx` instead of the current slot [#441](https://github.com/cardano-scaling/hydra/pull/441).

- The user manual is now also available in Japanese thanks to @btbf! :jp:

- Fixed display of remaining contestation period in `hydra-tui` [#437](https://github.com/cardano-scaling/hydra/pull/437).

## [0.6.0] - 2022-06-22

#### Added

- Implement on-chain contestation logic [#192](https://github.com/cardano-scaling/hydra/issues/192):
  + Node will automatically post a `Contest` transaction when it observes a `Close` or `Contest` with an obsolete snapshot
  + Posting a fan-out transaction is not possible before the contestation deadline has passed

- Transactions can now be submitted as raw CBOR-serialized object, base16 encoded, using the `NewTx` client input. This also supports the text-envelope format from cardano-cli out of the box. See the [api Reference](https://hydra.family/head-protocol/api-reference#operation-publish-/-message).

- **BREAKING** The `hydra-node` does not finalize Heads automatically anymore.
  + Instead clients do get a new `ReadyToFanout` server output after the contestation period and
  + Clients can use the `Fanout` client input command to deliberately finalize a Head when it is closed and the contestation period passed.

- Remaining contestation period is included in `HeadIsClosed` and displayed in `hydra-tui`.

#### Changed

- **BREAKING**: The starting state of a Head is renamed to `IdleState`, which is visible in the log API.

#### Fixed

- Head script to check UTxO hash upon closing the head correctly [#338](https://github.com/cardano-scaling/hydra/pull/338). Previously it was possible to close the head with arbitrary UTxO.
- Clients can fanout a Head closed without any off-chain transactions (eg. with initial snapshot)  [#395](https://github.com/cardano-scaling/hydra/issues/395)

## [0.5.0] - 2022-05-06

#### Added

- Start `hydra-node` tracking the chain starting at a previous point using new `--start-chain-from` command line option [#300](https://github.com/cardano-scaling/hydra/issues/300).
  + This is handy to re-initialize a stopped (or crashed) `hydra-node` with an already inititalized Head
  + Note that off-chain state is NOT persisted, but this feature is good enough to continue opening or closing/finalizing a Head

- Handle rollbacks [#184](https://github.com/cardano-scaling/hydra/issues/184)
  + Not crash anymore on rollbacks
  + Rewind the internal head state to the point prior to rollback point
  + Added `RolledBack` server output, see [API reference](https://hydra.family/head-protocol/api-reference)
  + See the [user manual](https://hydra.family/head-protocol/dev/rollbacks/) for a detailed explanation on how rollbacks are handled.

- [Hydra Network](https://hydra.family/head-protocol/dev/networking) section on the website about networking requirements and considerations

- [Benchmarks](https://hydra.family/head-protocol/benchmarks) section on the website with continuously updated and published results on transaction costs of Hydra protocol transactions
  + These are also performed and reported now on every PR -> [Example](https://github.com/cardano-scaling/hydra/pull/340#issuecomment-1116247611)

- New architectural decision records:
  + [ADR-0017: UDP for Hydra networking](https://hydra.family/head-protocol/adr/17)
  + [ADR-0018: Single state in Hydra.Node](https://hydra.family/head-protocol/adr/18)

- Improved `hydra-node --version` to show an easier to understand and accurate revision based on `git describe`

- Added `hydra-node --script-info` to check hashes of plutus scripts available in a `hydra-node`.
  + This can also be seen as the "script version" and should stabilize as we progress in maturity of the codebase.

#### Changed

- **BREAKING** Switch to Ed25519 keys and proper EdDSA signatures for the Hydra Head protocol
  + The `--hydra-signing-key` and consequently `--hydra-verification-key` are now longer and not compatible with previous versions!

- **BREAKING** The Hydra plutus scripts have changed in course of finalizing [#181](https://github.com/cardano-scaling/hydra/issues/181)
  + All Hydra protocol transactions need to be signed by a Head participant now
  + This changes the script address(es) and the current `hydra-node` would not detect old Heads on the testnet.

- **BREAKING** Renamed server output `UTxO -> GetUTxOResponse`
  + This should be a better name for the response of `GetUTxO` client input on our API :)

- Updated our dependencies (`plutus`, `cardano-ledger`, etc.) to most recent released versions making scripts smaller and Head transactions slightly cheaper already, see benchmarks for current limits.

#### Fixed

- Reject commit transactions locking a UTxO locked by Byron addresses, part of [#182](https://github.com/cardano-scaling/hydra/issues/182)
  + This would render a Head unclosable because Byron addresses are filtered out by the ledger and not visible to plutus scripts

- Fix instructions in [demo setup without docker](https://hydra.family/head-protocol/docs/getting-started/demo/without-docker) to use `0.0.0.0` and correct paths.

#### Known Issues

- TUI quickly flashes an error on fanout. This is because all nodes try to post a fanout transaction, but only one of the participants' transactions wins. Related to [#279](https://github.com/cardano-scaling/hydra/issues/279)
- Recipient addresses to send money to in the TUI are inferred from the current UTXO set. If a party does not commit a UTXO or consumes all its UTXO in a Head, it won't be able to send or receive anything anymore.
- TUI crashes when user tries to post a new transaction without any UTXO remaining.
- The internal wallet of hydra-node requires a UTXO to be marked as "fuel" to drive the Hydra protocol transactions. See [user manual](https://hydra.family/head-protocol/docs/getting-started/demo/with-docker/#seeding-the-network).

## [0.4.0] - 2022-03-23

#### Added

- Our [user manual 📖](https://hydra.family/head-protocol) is now available! It includes installation and usage instructions, a full API reference and also a knowledge base about Hydra concepts. The manual will be an ever-evolving source of documentation that we'll maintain alongside the project.
- Support multiple Heads per Cardano network by identifying and distinguishing transactions of individual Head instances [#180](https://github.com/cardano-scaling/hydra/issues/180).
- Mint and burn state token used to thread state across the OCV state machine, and participation tokens for each party in the head [#181](https://github.com/cardano-scaling/hydra/issues/181)
- Provide (mandatory) command-line options `--ledger-genesis` and `--ledger-protocol-parameters` to configure the ledger that runs _inside a head_. Options are provided as filepath to JSON files which match formats from `cardano-cli` and `cardano-node` [#180](https://github.com/input-output-hk/hydra/issues/180).
- Created [hydra-cardano-api](https://hydra.family/head-protocol/haddock/hydra-cardano-api/) as wrapper around [cardano-api](https://github.com/input-output-hk/cardano-node/tree/master/cardano-api#cardano-api) specialized to the latest Cardano's era, and with useful extra utility functions.
- Two new architectural decision records:
  - [ADR-0014: Token usage in Hydra Scripts](https://hydra.family/head-protocol/adr/14)
  - [ADR-0015: Configuration Through an Admin API](https://hydra.family/head-protocol/adr/15)

#### Changed

- `--network-magic` option for the `hydra-node` and `hydra-tui` has been changed to `--network-id`. Also, the `hydra-tui` command-line used to default to mainnet when not provided with any `--network-magic` option, it doesn't anymore, `--network-id` is mandatory. [#180](https://github.com/input-output-hk/hydra/issues/180)
- Optimize the `CollectCom` transition of the on-chain Hydra contract to allow collecting commits from more than 2 parties! [#254](https://github.com/cardano-scaling/hydra/issues/254)
- Use a faucet to distribute funds in test suites and the `demo/` setup.
- Internally, better decouple the management of the on-chain head state from the network component. While not visible to the end user, this improvement paves the way for better handling rollbacks and on-chain _"instability"_ of newly posted transactions. [#184](https://github.com/input-output-hk/hydra/issues/184)
- Internally, improved and consolidate generators used for property-based testing to cover a wider range of cases, be more consistent and also faster (avoiding to generate too large nested data-structures).

#### Fixed

- `Hydra.Network.Ouroboros` not using hard-coded valency values anymore to allow more than 7 peer connections [#203](https://github.com/cardano-scaling/hydra/issues/203).
- Build issues due to explicit packages list in nix shell [#223](https://github.com/cardano-scaling/hydra/issues/223).
- `hydra-tui` to show form focus, indicate invalid fields in dialogs and only allow valid values to be submitted [#224](https://github.com/cardano-scaling/hydra/issues/224).
- Repaired benchmarks and improved collected metrics; in particular, benchmarks now collect CPU usage and provide average confirmation times over 5s windows.
- Fixed a bug in the Fanout transaction scheduling and submission where clients would attempt to post a fanout transaction before a 'Close' transaction is even observed. Now, every participant of the head will attempt to post a fanout a transaction after they successfully observed a transaction. Of course, the layer 1 will enforce that only one fanout is posted [#279](https://github.com/input-output-hk/hydra/issues/279).

#### Known Issues

- Only no or one utxo can be committed to a Head.
- Recipient addresses to send money to in the TUI are inferred from the current UTXO set. If a party does not commit a UTXO or consumes all its UTXO in a Head, it won't be able to send or receive anything anymore.
- TUI crashes when user tries to post a new transaction without any UTXO remaining.
- The internal wallet of hydra-node requires a UTXO to be marked as "fuel" to drive the Hydra protocol transactions. See [user manual](https://hydra.family/head-protocol/docs/getting-started/demo/with-docker/#seeding-the-network).
- Aborting a head with more than 2 participants (i.e. `> 2`) requires increase in tx size limit over current mainchain parameters to ~20KB.
- Head can collect at most 3 commits and each party can commit either 1 or 0 UTXO to a Head.
- The head cannot be finalized if holding more than ~100 assets (or ~50 ada-only UTxO entries) with the standard tx size of 16KB.

## [0.3.0] - 2022-02-02

#### Added

- Implementation of on-chain verification of Hydra Head lifecycle without contests. This first version with its various shortcuts is documented on examples of the [full](./docs/adr/img/on-chain-full.jpg) and [abort](./docs/adr/img/on-chain-abort.jpg) on-chain life-cycles of a Hydra Head
- Enable nix-shell on Mac
- Build separate docker images for `hydra-node` and `hydra-tui` available as [packages](https://github.com/orgs/input-output-hk/packages?repo_name=hydra) from GitHub repo
- Utility executable `inspect-script` to dump contracts for further analysis
- CBOR encoder and Merkle-Tree in Plutus as separate packages `plutus-cbor` and `plutus-merkle-tree`, released & tagged separately

#### Changed

- Package `local-cluster` is now `hydra-cluster`.
- Use `cardano-api` types and functions to interact with chain.
- Refine computation of fees from internal wallet.
- Remove several sources of `error` in chain interaction component.

#### Known issues

- `collectComTx` requires increase in tx size limit over current mainchain parameters to 32KB, which should be alleviated with Plutus optimisations and merging all contracts in one in future releases
- Head can collect at most 9 commits and each party can commit either 1 or 0 UTXO to a Head
- `fanoutTx` cannot handle more than 100 UTxO with the standard tx size of 16KB (200 with the temporary increase for test purpose).
- Known issues from `0.2.0` still apply

## [0.2.0] - 2021-12-14

#### Added
- Direct chain integration which allows to connect to a real cardano-node /
  devnet; no on-chain validators though.
- Support alonzo transactions inside the Hydra Head. For now using a `freeCostModel`.
- Command line options `--node-socket`, `--network-magic` and
  `--cardano-{signing,verification}-key` to `hydra-node` and `hydra-tui` to
  configure the Cardano network access.

#### Changed
- Command line options of `hydra-node` quite significantly to distinguish hydra
  credentials from cardano credentials.
- Commit and transaction creation logic of TUI to use cardano credentials.

#### Removed
- ZeroMQ mock-chain executable, chain component and corresponding `hydra-node`
  command line options.
- ZeroMQ based network component.
- Aliases from party identifiers.

#### Fixed
- `hydra-tui` to correctly show current state when re-connecting.

#### Known issues
- There can only be one Head per Cardano network (i.e. on the devnet).
- Only no or one utxo can be committed to a Head.
- Recipient addresses to send money to in the TUI are inferred from the current
  UTXO set. If a party does not commit a UTXO or consumes all its UTXO in a
  Head, it won't be able to send or receive anything anymore.
- TUI crashes when user tries to post a new transaction without any UTXO
  remaining.
- Not an issue, but a workaround: The internal wallet of `hydra-node` requires a
  UTXO to be marked as "fuel" to drive the Hydra protocol transactions.

## [0.1.0] - 2021-09-30

- First proof-of-concept for a `hydra-node`

### Added
- Coordinated Hydra Head protocol
- Single Head per hydra-node
- Stubbed chain using external process
- Network statically configured, direct TCP connections
- WebSocket, message-based API Server
- Terminal user interface client
