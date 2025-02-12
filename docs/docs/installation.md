---
sidebar_position: 2
---

# Installation

Selecting the right installation method for the `hydra-node` depends on your requirements and technical preferences. Below are the primary methods to get started quickly and efficiently.

### Docker

The quickest way to install and run a `hydra-node` is by using Docker images:

```shell
docker pull ghcr.io/cardano-scaling/hydra-node
docker run --rm ghcr.io/cardano-scaling/hydra-node --help
```

### Prebuilt binaries

We provide statically linked binaries of `hydra-node` and `hydra-tui` for x86_64 Linux and ARM64 MacOS platforms. These binaries are available as attachments in our [GitHub releases](https://github.com/cardano-scaling/hydra/releases).

<!-- TODO: Document run-time dependency 'etcd' -->

### Build from source

While using Docker is the recommended way to _use_ Hydra, you can also build the `hydra-node` from source. We recommend using [Nix](https://nixos.org/download.html) for this and you can find instructions in our [contributing guidelines](https://github.com/cardano-scaling/hydra/blob/master/CONTRIBUTING.md).
