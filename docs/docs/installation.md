---
sidebar_position: 2
---

# Installation

Selecting the right installation method for the `hydra-node` depends on your requirements and technical preferences. Below are the primary methods to get started quickly and efficiently.

### Docker

For rapid deployment, Docker offers the most straightforward method to install and run `hydra-node`. Use the following commands to pull the latest Docker image and display the help options to verify the installation:

```shell
docker pull ghcr.io/input-output-hk/hydra-node
docker run --rm ghcr.io/input-output-hk/hydra-node --help
```

### Prebuilt binaries

For users who prefer a traditional installation or do not use Docker, prebuilt binaries are available. We provide prebuilt binaries of `hydra-node` and `hydra-tui` for both x86_64 Linux and ARM64 MacOS platforms. These binaries are statically linked (as much as possible) to ensure ease of use across different systems. For more information, see our [github releases](https://github.com/input-output-hk/hydra/releases).


### Build from source

While using docker is the recommended way to _use_ Hydra, you can also build the `hydra-node` from source. We recommend using [nix](https://nixos.org/download.html) for this and the instructions can be found in our [Contributing Guidelines](https://github.com/input-output-hk/hydra/blob/master/CONTRIBUTING.md).
