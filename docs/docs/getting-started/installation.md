---
sidebar_position: 2
---

# Installation

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

> Where to get Hydra from?

## Using Docker

The quickest way to get a hydra-node running is to use our Docker images.

````mdx-code-block
<TerminalWindow>

```
docker pull ghcr.io/input-output-hk/hydra-node:latest
docker run --rm ghcr.io/input-output-hk/hydra-node --help
```

</TerminalWindow>
````

## Building from source

While using docker is the recommended way to _use_ Hydra, you can also build the `hydra-node` from source. We do recommend using [nix](https://nixos.org/download.html) for this though and the instructions can be found in our [Contributing Guidelines](../CONTRIBUTING.md).
