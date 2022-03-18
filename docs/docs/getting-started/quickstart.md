---
sidebar_position: 3
---

# QuickStart

> Your first steps with a `hydra-node`.

Running a Hydra head means running a Hydra node connected to some other Hydra nodes and connected to a Cardano node. A working [cardano-node](https://github.com/input-output-hk/cardano-node/) is therefore a pre-requisite for running a Hydra head. In this guide, we won't go over details about running a Cardano node and we invite you to look for existing documentation on the matter if need be.

:::tip cardano-node & cardano-cli
We recommend using containers and the [official Docker image](https://hub.docker.com/r/inputoutput/cardano-node) for running a Cardano node. 

This image contains both `cardano-node` and `cardano-cli`. The latter is handy to run various commands, for example to create addresses and to generate keys.
:::

So far, the `hydra-node` command-line provide a single command only for starting a node. The entire configuration is provided through command-line options and is fully static. Options are used to configured various elements, and can be summarized as follows (note however that reference documentation for those options is available under the `--help` option):

Options                                                 | Description
---                                                     | ---
`--node-id`                                             | The Hydra node identifier, serving as identifier within the Head network.
`--peer`                                                | The Hydra network peers address. Must be provided multiple time, one for each peer.
`--host` <br/> `--port`                                 | This Hydra node host and port, to which peers from the Hydra network can connect to.
`--node-socket`                                         | The Cardano node's IPC socket filepath, used for inter-process communication with the node.
`--ledger-genesis` <br/> `--ledger-protocol-parameters` | The Hydra ledger rules and parameters for the head.
`--hydra-signing-key` <br/> `--cardano-signing-key` <br/> `--hydra-verification-key` <br/> `--cardano-verification-key` | The Cardano and Hydra credentials for peers and the the node itself. Those options may also be provided multiple times depending on the number of peers. 

Also, optionally:

Options                         | Description
---                             | ---
`--api-host` <br/> `--api-port` | The Hydra API host and port, to interact with the [WebSocket API](/api-reference).
`--monitoring-port`             | The port this node listens on for monitoring and metrics via Prometheus. If left empty, monitoring server is not started.

:::info  Dynamic Configuration 

We realise that the command-line in its current form isn't as user-friendly as it could, and is somewhat cumbersome to use for setting up large clusters.  

There are however plans to make the configuration more user-friendly and configurable dynamically; see [#240](https://github.com/input-output-hk/hydra-poc/issues/240) & [ADR-15](/adr/15)

... to be continued
