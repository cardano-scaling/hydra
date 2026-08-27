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

The `hydra-node` image is published for `linux/amd64` and `linux/arm64`, so the
above pulls the image native to your machine. The `hydra-tui`, `hydraw` and
`hydra-chain-observer` images are published for `linux/amd64` only. Running
those on an arm64 host needs emulation, which Docker Desktop sets up for you but
a plain Linux install does not, unless `qemu-user-static` binfmt handlers are
registered.

### Dependencies

There is **one** run-time dependency of `hydra-node`:

- [`etcd`](https://etcd.io/docs/v3.5/install/) used internally to create a reliable network between nodes

You can ignore this if you are using the Docker image.

### Prebuilt binaries

We provide binaries of `hydra-node` and `hydra-tui` for x86_64 Linux, aarch64 Linux and ARM64 MacOS platforms. These binaries are available as attachments in our [GitHub releases](https://github.com/cardano-scaling/hydra/releases). The x86_64 Linux ones are statically linked; the aarch64 Linux ones are not, as there is no musl cross build for that platform yet.

:::info
Recent versions of MacOS block unverified binaries to protect your system.

> *Apple cannot verify that "hydra-node" is free of malware that may harm your Mac or compromise your privacy.*

To proceed, you’ll need to allow it manually in the **Privacy & Security** section of your system settings.
:::

### Build from source

While using Docker is the recommended way to _use_ Hydra, you can also build the `hydra-node` from source. We recommend using [Nix](https://nixos.org/download.html) for this and you can find instructions in our [contributing guidelines](https://github.com/cardano-scaling/hydra/blob/master/CONTRIBUTING.md).
