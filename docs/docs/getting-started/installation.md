---
sidebar_position: 2
---

# Installation Instructions

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
import LastUpdatedAt from '@site/src/components/LastUpdatedAt';
```

```mdx-code-block
<LastUpdatedAt 
    lastUpdatedAt="{{last-updated-at}}"
    authorInfo="{{author-info}}"
    lastTranslatedAt="{{last-translated-at}}"
/>
```

> Installation instructions for Hydra nodes. We recommend using Docker and dealing with containers as a quick start.

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

:::tip
This is enough for most use cases. Unless you want to install everything from source code, you can skip the next sections and go directly to [Quick Start](/docs/getting-started/quickstart).
:::

## Using Nix

We provide a `shell.nix` to set up a development environment. So a simple call to nix-shell should put everything in place for building, testing, and general development.
Make sure the following caches are listed in your `nix.conf` for a speedy setup:

```nix title="nix.conf"
substituters = https://cache.nixos.org https://iohk.cachix.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

Also, some of us use `direnv` and `nix-direnv` to automatically import & cache the nix-shell environment into our favorite shell or editor.
Within the nix-shell, `cabal build` and `cabal test` should work as expected.

You can also use `nix-build` to build the project and all executables. You will find them in `result/bin/` after the build.

## Using Cabal

1. Install basic Haskell development environment, for example using ghcup. Hydra requires GHC 8.10.7 and a recent cabal (> 3.0).

1. Install various system dependencies On Debian-like:

    ````mdx-code-block
    <TerminalWindow>

    ```
    sudo apt install -y  build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
    sudo apt install -y  libz-dev liblzma-dev libzmq3-dev pkg-config libtool
    ```

    </TerminalWindow>
    ````

    Do not confuse `lzma` with `liblzma-dev`, those are 2 existing package!

1. Install forked libsodium

    ````mdx-code-block
    <TerminalWindow>

    ```
    git clone https://github.com/input-output-hk/libsodium
    cd libsodium/
    git checkout 66f017f16633f2060db25e17c170c2afa0f2a8a1
    ./autogen.sh
    ./configure
    make && sudo make install
    ```

    </TerminalWindow>
    ````

1. To run integration tests & benchmarks, do install a recent `cardano-node` to the path. For example, see the official doc [here](https://developers.cardano.org/docs/get-started/installing-cardano-node).

1. Build and test everything:

    ```mdx-code-block
    <TerminalWindow>
    cabal build all && cabal test all
    </TerminalWindow>
    ```
