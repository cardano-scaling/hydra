# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## [0.3.0] - UNRELEASED

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
- Commit and transaction creation logic of TUI to use cardano credentials

### Removed
- ZeroMQ mock-chain executable, chain component and corresponding `hydra-node` command line options
- ZeroMQ based network component
- Aliases from party identifiers.

#### Fixed
- `hydra-tui` to correctly show current state when re-connecting.

#### Known issues
- Only no or one utxo can be committed to a Head.
- The addresses to send money to in the Head is inferred from the current UTXO set. If a party does
  not commit a UTXO or consumes all its UTXO in a Head, it won't be able to send or receive anything
  anymore.
- TUI can crash when user tries to post a new transaction wihout any UTXO remaining

## [0.1.0] - 2021-09-30

- First proof-of-concept for a `hydra-node`

### Added
- Coordinated Hydra Head protocol
- Single Head per hydra-node
- Stubbed chain using external process
- Network statically configured, direct TCP connections
- WebSocket, message-based API Server
- Terminal user interface client
