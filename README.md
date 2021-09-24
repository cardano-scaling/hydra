# <p align="center">:construction: Hydra Proof of Concept (POC) 🚧</p>

<div align="center">
  <p>A home to our colorful experiments and prototypes.</p>
  <a href='https://github.com/input-output-hk/hydra-poc/actions'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/CI?label=Tests&style=for-the-badge" /></a>
  <a href='https://hub.docker.com/r/inputoutput/hydra/tags'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/Docker?label=Docker&style=for-the-badge" /></a>
</div>

## Introduction

Hydra is the layer-two scalability solution for Cardano, which aims to increase
the speed of transactions (low latency, high throughput) and minimize
transaction cost.

This repository contains the proof-of-concept implementation the Hydra
engineering team has put together during exploration and can be considered a
"developer preview". It outlines the basic architecture of a `hydra-node`, which
runs a simplified (coordinated) [Hydra Head
protocol](https://eprint.iacr.org/2020/299.pdf), connects to other hydra-nodes,
interfaces the Cardano blockchain and provides an API to clients such as the
included terminal user interface `hydra-tui`.

:warning: This is still prototypical and exploratory work shared here for your
interest - it is NOT ready for production (yet) :warning:

Please also note that as we did develop this while in "move fast, break things"
experimentation mode, the code quality seen around here may not always represent
our best practices and you will find many code smells and dirty hacks.

Thanks for visiting and enjoy!

## Features

Proof of concept:
- [x] Coordinated Hydra Head protocol
- [x] Single Head per hydra-node
- [x] Stubbed chain using external process
- [x] Network statically configured, direct TCP connections
- [x] WebSocket, message-based API Server
- [x] Terminal user interface client

Later:
- [ ] Cardano-node integration (direct or via PAB)
- [ ] Persisted Head state
- [ ] Multiple Heads per hydra-node, managed via API
- [ ] Support for external wallets (e.g. hardware wallets)
- [ ] Optimistic Head closure and incremental de-/commit protocol extension
- [ ] Relay-capable, mesh network

## 👷‍♂️ Development

We provide a `shell.nix` to set up a development environment. So a simple call
to `nix-shell` should put everything in place for building, testing and
general development.

Make sure the following caches are listed in your `nix.conf` for a speedy setup:

```
substituters = https://cache.nixos.org https://iohk.cachix.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

Also, some of us use [direnv](https://direnv.net/) and
[nix-direnv](https://github.com/nix-community/nix-direnv) to automatically
import the nix-shell environment into our favorite shell or editor.

### Building & Testing

Within the `nix-shell`, `cabal build` and `cabal test` should work as expected.

You can also use `nix-build` to build the project and all its executables. You
will find them in `result/bin/` after the build.

### Updating nix materialization

We rely on haskell.nix's "materialization" to not needing to create cabal build
plans on every invocation of a nix-build (as this would take > 15mins for our
huge dependency tree).

Thus, the build plan is persisted into the repository at
`nix/hydra-poc.materialized/` and needs to be bumped whenever _anything_ in the
`cabal.project` or any of the `.cabal` files changes.

If the `cabal build` succeeds, but the `nix-build` fails, it's usually a good
idea to do the following:

* Uncomment `checkMaterialization = true;` in `default.nix`
* Execute `nix-build -A hydra-node.components.exes.hydra-node`. This command should fail because of an incorrect sha256 value.
* Update `plan-sha256` in `default.nix` to the expected hash value
* Execute `nix-build -A hydra-node.project.plan-nix.passthru.updateMaterialized | bash`
* Follow the instructions in the output by running the `/nix/store/<somehash>-updateMaterialized` script.
* Comment out `checkMaterialization = true;` again and commit the changes

## Documentation

Documentation is published online at https://input-output-hk.github.io/hydra-poc from the [docs](docs/README.md) directory.

API Documentations are available for:
* The [WebSocket API](https://input-output-hk.github.io/json-schema-viewer/#/?url=https://raw.githubusercontent.com/input-output-hk/hydra-poc/master/hydra-node/api.yaml)
* The [Log API](https://input-output-hk.github.io/json-schema-viewer/#/?url=https://raw.githubusercontent.com/input-output-hk/hydra-poc/master/hydra-node/api-log.yaml)

[Haddock](https://www.haskell.org/haddock/) documentation is also published for each package:
* [hydra-prelude](https://input-output-hk.github.io/hydra-poc/haddock/hydra-prelude/index.html)
* [hydra-node](https://input-output-hk.github.io/hydra-poc/haddock/hydra-node/index.html)
* [hydra-plutus](https://input-output-hk.github.io/hydra-poc/haddock/hydra-plutus/index.html)
* [local-cluster](https://input-output-hk.github.io/hydra-poc/haddock/local-cluster/index.html)
* [merkle-patricia-tree](https://input-output-hk.github.io/hydra-poc/haddock/merkle-patricia-tree/index.html)

## Try it out

Either using the `demo/docker-compose.yaml` or when in the `nix-shell`, you can
start a demo scenario with three parties using the following commands each in a
separate shell:

``` sh
$ cabal exec mock-chain
```

``` sh
$ cabal exec hydra-node -- --node-id 1 --api-port 4001 \
  --port 5001 --peer "localhost:5002" --peer "localhost:5003" \
  --me "demo/alice.sk" --party "demo/bob.vk" --party "demo/carol.vk"
```

``` sh
$ cabal exec hydra-node -- --node-id 2 --api-port 4002 \
  --port 5002 --peer "localhost:5001" --peer "localhost:5003" \
  --me "demo/bob.sk" --party "demo/alice.vk" --party "demo/carol.vk"
```

``` sh
$ cabal exec hydra-node -- --node-id 3 --api-port 4003 \
  --port 5003 --peer "localhost:5001" --peer "localhost:5002" \
  --me "demo/carol.sk" --party "demo/alice.vk" --party "demo/bob.vk"
```

Then you can interact with any of the three `hydra-node` instances using `ws`
and connecting to the individual port, e.g. to initiate a head via alice's
`hydra-node`:

``` sh
$ ws ws://127.0.0.1:4001
> {"tag":"Init"}
< {"parties":[42,51,66],"tag":"ReadyToCommit"}
```
