---
sidebar_position: 2
---

# Installation

Selecting the right installation method for the `hydra-node` depends on your requirements and technical preferences. Below are the primary methods to get started quickly and efficiently.

### Docker

The quickest way to install and run a `hydra-node` is by using Docker images:

```shell
docker pull ghcr.io/input-output-hk/hydra-node
docker run --rm ghcr.io/input-output-hk/hydra-node --help
```

### Prebuilt binaries

We provide statically linked binaries of `hydra-node` and `hydra-tui` for x86_64 Linux and ARM64 MacOS platforms. These binaries are available as attachments in our [github releases](https://github.com/input-output-hk/hydra/releases).


### Build from source

While using Docker is the recommended way to _use_ Hydra, you can also build the `hydra-node` from source. We recommend using [Nix](https://nixos.org/download.html) for this and you can find instructions in our [contributing guidelines](https://github.com/input-output-hk/hydra/blob/master/CONTRIBUTING.md).
