---
sidebar_position: 2
---

# Installation

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

> Where to get Hydra from?

## Using Docker

The quickest way to get a hydra-node running is to use our Docker images:

````mdx-code-block
<TerminalWindow>

```
docker pull ghcr.io/input-output-hk/hydra-node:0.8.1
docker run --rm ghcr.io/input-output-hk/hydra-node --help
```

</TerminalWindow>
````

## Using prebuilt static binaries

We provide builds for `hydra-node`, `hydra-tools` and `hydra-tui` as statically
linked `x64_86` binaries. These binaries can be found in our CI or attached to
[github releases](https://github.com/input-output-hk/hydra/releases)
starting with version `0.8.1`.

## Building from source

While using docker is the recommended way to _use_ Hydra, you can also build the `hydra-node` from source. We do recommend using [nix](https://nixos.org/download.html) for this though and the instructions can be found in our [Contributing Guidelines](https://github.com/input-output-hk/hydra/blob/master/CONTRIBUTING.md).
