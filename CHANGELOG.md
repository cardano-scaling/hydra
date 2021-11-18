# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## [0.2.0] - UNRELEASED

#### Added
- Direct chain integration which allows to connect to a real cardano-node /
  devnet; no on-chain validators though.

### Removed
- ZeroMQ mock-chain executable, chain component and corresponding `hydra-node` command line options
- ZeroMQ based network component
- Aliases from party identifiers.

## [0.1.0] - 2021-09-30

- First proof-of-concept for a `hydra-node`

### Added
- Coordinated Hydra Head protocol
- Single Head per hydra-node
- Stubbed chain using external process
- Network statically configured, direct TCP connections
- WebSocket, message-based API Server
- Terminal user interface client
