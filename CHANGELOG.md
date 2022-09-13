# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## [0.8.0] - UNRELEASED

- **BREAKING** Keep track of `contestationDeadline` instead of `remainingContestationPeriod` and fix `ReadyToFanout`.
  + Clients can now rely on `ReadyToFanout`, such that sending a `Fanout` input after seeing this output will never be "too early".
  + The `HeadIsClosed` server output now contains the deadline instead of the remaining time.
  + See `hydra-tui` for an example how to use the `contestationDeadline` and `ReadyToFanout`.
  + See [ADR20](./docs/adr/2022-08-02_020-handling-time.md) for details and the rationale.

- **BREAKING** Changed logs to improve legibility and trace on-chain posting errors.
  + Strip chain layer logs to only contain `TxId` instead of full transactions in the nominal cases.
  + Renamed log entry prefixes `Processing -> Begin` and `Processed -> End`.
  + Added `PostingFailed` log entry.

- **BREAKING** The `hydra-cluster` executable (our smoke test) does require `--publish-scripts` or `--hydra-scripts-tx-id` now as it may be provided with pre-published hydra scripts.

- Added a `hydra-tools` executable, which provides basic commands to help working with Hydra Heads:
  + Generate a pair of Hydra keys
  + Output the marker datum hash

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

- Added a `hydra-cluster` executable, which runs a single scenario against a known network (smoke test) [#430](https://github.com/input-output-hk/hydra-poc/pull/430) [#423](https://github.com/input-output-hk/hydra-poc/pull/430).

- Use deadline when posting a `fanoutTx` instead of the current slot [#441](https://github.com/input-output-hk/hydra-poc/pull/441).

- The user manual is now also available in Japanese thanks to @btbf! :jp:

- Fixed display of remaining contestation period in `hydra-tui` [#437](https://github.com/input-output-hk/hydra-poc/pull/437).

## [0.6.0] - 2022-06-22

#### Added

- Implement on-chain contestation logic [#192](https://github.com/input-output-hk/hydra-poc/issues/192):
  + Node will automatically post a `Contest` transaction when it observes a `Close` or `Contest` with an obsolete snapshot
  + Posting a fan-out transaction is not possible before the contestation dealine has passed

- Transactions can now be submitted as raw CBOR-serialized object, base16 encoded, using the `NewTx` client input. This also supports the text-envelope format from cardano-cli out of the box. See the [api Reference](https://hydra.family/head-protocol/api-reference#operation-publish-/-message).

- **BREAKING** The `hydra-node` does not finalize Heads automatically anymore.
  + Instead clients do get a new `ReadyToFanout` server output after the contestation period and
  + Clients can use the `Fanout` client input command to deliberately finalize a Head when it is closed and the contestation period passed.

- Remaining contestation period is included in `HeadIsClosed` and displayed in `hydra-tui`.

#### Changed

- **BREAKING**: The starting state of a Head is renamed to `IdleState`, which is visible in the log API.

#### Fixed

- Head script to check UTxO hash upon closing the head correctly [#338](https://github.com/input-output-hk/hydra-poc/pull/338). Previously it was possible to close the head with arbitrary UTxO.
- Clients can fanout a Head closed without any off-chain transactions (eg. with initial snapshot)  [#395](https://github.com/input-output-hk/hydra-poc/issues/395)

## [0.5.0] - 2022-05-06

#### Added

- Start `hydra-node` tracking the chain starting at a previous point using new `--start-chain-from` command line option [#300](https://github.com/input-output-hk/hydra-poc/issues/300).
  + This is handy to re-initialize a stopped (or crashed) `hydra-node` with an already inititalized Head
  + Note that off-chain state is NOT persisted, but this feature is good enough to continue opening or closing/finalizing a Head

- Handle rollbacks [#184](https://github.com/input-output-hk/hydra-poc/issues/184)
  + Not crash anymore on rollbacks
  + Rewind the internal head state to the point prior to rollback point
  + Added `RolledBack` server output, see [API reference](https://hydra.family/head-protocol/api-reference)
  + See the [user manual](https://hydra.family/head-protocol/core-concepts/rollbacks/) for a detailed explanation on how rollbacks are handled.

- [Hydra Network](https://hydra.family/head-protocol/core-concepts/networking) section on the website about networking requirements and considerations

- [Benchmarks](https://hydra.family/head-protocol/benchmarks) section on the website with continuously updated and published results on transaction costs of Hydra protocol transactions
  + These are also performed and reported now on every PR -> [Example](https://github.com/input-output-hk/hydra-poc/pull/340#issuecomment-1116247611)

- New architectural decision records:
  + [ADR-0017: UDP for Hydra networking](https://hydra.family/head-protocol/adr/17)
  + [ADR-0018: Single state in Hydra.Node](https://hydra.family/head-protocol/adr/18)

- Improved `hydra-node --version` to show an easier to understand and accurate revision based on `git describe`

- Added `hydra-node --script-info` to check hashes of plutus scripts available in a `hydra-node`.
  + This can also be seen as the "script version" and should stabilize as we progress in maturity of the codebase.

#### Changed

- **BREAKING** Switch to Ed25519 keys and proper EdDSA signatures for the Hydra Head protocol
  + The `--hydra-signing-key` and consequently `--hydra-verification-key` are now longer and not compatible with previous versions!

- **BREAKING** The Hydra plutus scripts have changed in course of finalizing [#181](https://github.com/input-output-hk/hydra-poc/issues/181)
  + All Hydra protocol transactions need to be signed by a Head participant now
  + This changes the script address(es) and the current `hydra-node` would not detect old Heads on the testnet.

- **BREAKING** Renamed server output `UTxO -> GetUTxOResponse`
  + This should be a better name for the response of `GetUTxO` client input on our API :)

- Updated our dependencies (`plutus`, `cardano-ledger`, etc.) to most recent released versions making scripts smaller and Head transactions slighly cheaper already, see benchmarks for current limits.

#### Fixed

- Reject commit transactions locking a UTxO locked by Byron addresses, part of [#182](https://github.com/input-output-hk/hydra-poc/issues/182)
  + This would render a Head unclosable because Byron addresses are filtered out by the ledger and not visible to plutus scripts

- Fix instructions in [demo setup without docker](https://hydra.family/head-protocol/docs/getting-started/demo/without-docker) to use `0.0.0.0` and correct paths.

#### Known Issues

- TUI quickly flashes an error on fanout. This is because all nodes try to post a fanout transaction, but only one of the participants' transactions wins. Related to [#279](https://github.com/input-output-hk/hydra-poc/issues/279)
- Recipient addresses to send money to in the TUI are inferred from the current UTXO set. If a party does not commit a UTXO or consumes all its UTXO in a Head, it won't be able to send or receive anything anymore.
- TUI crashes when user tries to post a new transaction without any UTXO remaining.
- The internal wallet of hydra-node requires a UTXO to be marked as "fuel" to drive the Hydra protocol transactions. See [user manual](https://hydra.family/head-protocol/docs/getting-started/demo/with-docker/#seeding-the-network).

## [0.4.0] - 2022-03-23

#### Added

- Our [user manual 📖](https://hydra.family/head-protocol) is now available! It includes installation and usage instructions, a full API reference and also a knowledge base about Hydra concepts. The manual will be an ever-evolving source of documentation that we'll maintain alongside the project.
- Support multiple Heads per Cardano network by identifying and distinguishing transactions of individual Head instances [#180](https://github.com/input-output-hk/hydra-poc/issues/180).
- Mint and burn state token used to thread state across the OCV state machine, and participation tokens for each party in the head [#181](https://github.com/input-output-hk/hydra-poc/issues/181)
- Provide (mandatory) command-line options `--ledger-genesis` and `--ledger-protocol-parameters` to configure the ledger that runs _inside a head_. Options are provided as filepath to JSON files which match formats from `cardano-cli` and `cardano-node` [#180](https://github.com/input-output-hk/hydra-poc/issues/180).
- Created [hydra-cardano-api](https://hydra.family/head-protocol/haddock/hydra-cardano-api/) as wrapper around [cardano-api](https://github.com/input-output-hk/cardano-node/tree/master/cardano-api#cardano-api) specialized to the latest Cardano's era, and with useful extra utility functions.
- Two new architectural decision records:
  - [ADR-0014: Token usage in Hydra Scripts](https://hydra.family/head-protocol/adr/14)
  - [ADR-0015: Configuration Through an Admin API](https://hydra.family/head-protocol/adr/15)

#### Changed

- `--network-magic` option for the `hydra-node` and `hydra-tui` has been changed to `--network-id`. Also, the `hydra-tui` command-line used to default to mainnet when not provided with any `--network-magic` option, it doesn't anymore, `--network-id` is mandatory. [#180](https://github.com/input-output-hk/hydra-poc/issues/180)
- Optimize the `CollectCom` transition of the on-chain Hydra contract to allow collecting commits from more than 2 parties! [#254](https://github.com/input-output-hk/hydra-poc/issues/254)
- Use a faucet to distribute funds in test suites and the `demo/` setup.
- Internally, better decouple the management of the on-chain head state from the network component. While not visible to the end user, this improvement paves the way for better handling rollbacks and on-chain _"instability"_ of newly posted transactions. [#184](https://github.com/input-output-hk/hydra-poc/issues/184)
- Internally, improved and consolidate generators used for property-based testing to cover a wider range of cases, be more consistent and also faster (avoiding to generate too large nested data-structures).

#### Fixed

- `Hydra.Network.Ouroboros` not using hard-coded valency values anymore to allow more than 7 peer connections [#203](https://github.com/input-output-hk/hydra-poc/issues/203).
- Build issues due to explicit packages list in nix shell [#223](https://github.com/input-output-hk/hydra-poc/issues/223).
- `hydra-tui` to show form focus, indicate invalid fields in dialogs and only allow valid values to be submitted [#224](https://github.com/input-output-hk/hydra-poc/issues/224).
- Repaired benchmarks and improved collected metrics; in particular, benchmarks now collect CPU usage and provide average confirmation times over 5s windows.
- Fixed a bug in the Fanout transaction scheduling and submission where clients would attempt to post a fanout transaction before a 'Close' transaction is even observed. Now, every participant of the head will attempt to post a fanout a transaction after they successfully observed a transaction. Of course, the layer 1 will enforce that only one fanout is posted [#279](https://github.com/input-output-hk/hydra-poc/issues/279).

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
- Build separate docker images for `hydra-node` and `hydra-tui` available as [packages](https://github.com/orgs/input-output-hk/packages?repo_name=hydra-poc) from GitHub repo
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
- TUI crashes when user tries to post a new transaction wihout any UTXO
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
