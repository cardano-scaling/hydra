---
title: Clients
---

# Clients

While the `hydra-node` tracks the main chain for opening/closing heads and connects to other nodes to form an overlay network, it also offers a [client API](/api-reference) to _use_ and _administrate_ the Hydra head. This API can be used directly by applications integrating with Hydra through SDKs, or via third-party components.

## Client applications

- [hydra-tui](https://hydra.family/head-protocol/docs/getting-started#use-the-head): Example client offering a terminal user interface (TUI) that can be used to administrate heads and submit simple payment transactions
- [kupo](https://cardanosolutions.github.io/kupo/#section/Getting-started/-hydra-host-hostname-hydra-port-port-number): Lightweight indexer that directly supports connecting to a `hydra-node`

## Client libraries

- [hydrasdk](https://hydrasdk.com/): A comprehensive software development kit for building Cardano DApps and wallet applications with Hydra Layer 2 integration
- [meshjs](https://meshjs.dev/providers/hydra): Provider for the [meshjs SDK](https://meshjs.dev/) which supports the full Head life-cycle
- [blaze](https://github.com/butaneprotocol/blaze-cardano/blob/main/packages/blaze-query/src/hydra.ts): Experimental provider for the [blaze SDK](https://blaze.butane.dev/)


:::info
Contributions to complete and keep this list up-to-date are more than welcome!
:::
