---
sidebar_position: 2
---

# Installation

### Docker

The quickest way to get a `hydra-node` installed and running is to use our Docker images:

```shell
docker pull ghcr.io/input-output-hk/hydra-node
docker run --rm ghcr.io/input-output-hk/hydra-node --help
```

### Prebuilt binaries

We provide statically linked (as much as possible) binaries of `hydra-node` and `hydra-tui` for x86_64 Linux and ARM64 MacOS platforms as attachment of our [github releases](https://github.com/input-output-hk/hydra/releases).

### Build from source

While using docker is the recommended way to _use_ Hydra, you can also build the `hydra-node` from source. We do recommend using [nix](https://nixos.org/download.html) for this and the instructions can be found in our [Contributing Guidelines](https://github.com/input-output-hk/hydra/blob/master/CONTRIBUTING.md).
