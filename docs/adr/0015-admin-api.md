# 15. Configure Node through an Admin API

Date: 2022-03-04

## Status

:hammer_and_wrench:

## Context

* Hydra-node currently requires a whole slew of command-line arguments to configure properly its networking layer: `--peer` to connect to each peer, `--cardano-verification-key` and `--hydra-verification-key` to identify the peer on the L1 and L2 respectively.
* This poses significant challenges for operating a _cluster_ of Hydra nodes as one needs to know beforehand everything about the cluster, then pass a large number of arguments to some program or docker-compose file, before any node can be started
  * This is a pain that's been felt first-hand for benchmarking and testing purpose
* Having static network configuration is probably not sustainable in the long run, even if we don't add any fancy multihead capabilities to the node, as it would make it significantly harder to have automated creation of Heads.
* There's been an [attempt](https://github.com/input-output-hk/hydra-poc/pull/222) at providing a file-based network configuration but this was deemed unconvincing
* [Hydra paper (sec. 4, p. 13)](https://eprint.iacr.org/2020/299.pdf) explicitly assumes the existence of a _setup_ phase

## Decision

* Hydra-node exposes an _Administrative API_ to enable configuration of the Hydra network using "standard" tools
  * API is exposed as a set of HTTP endpoints on some port, consuming and producing JSON data
  * It is documented as part of the User's Guide for Hydra Head
* This API provides _commands_ and _queries_ to:
  * Add/remove _peers_ providing their address and keys,
  * List currently known peers and their connectivity status,
  * Start/stop/reset the Hydra network

## Consequences

* The API must be secured/authorised possibly using standard HTTP mechanisms
* It's easy to deploy Hydra nodes with some standard configuration, then dynamically configure them
* It makes it possible to _reconfigure_ a Hydra node with different peers
* The _Client API_ should reflect the state of the network and disable `Init`ing a head if the network layer is not started
* Operational tools could be built easily on top of the API, for command-line or Web-based configuration
