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

The only run-time dependency of `hydra-node` is [`etcd`](https://etcd.io/), used
internally to create a reliable network between nodes. A suitable binary is
embedded in `hydra-node` and installed into the persistence directory on
startup, so nothing needs to be installed separately. Only when opting out of
the embedded binary with `--use-system-etcd` does `etcd` need to be
[installed](https://etcd.io/docs/v3.6/install/) on the system.

### Prebuilt binaries

We provide binaries of `hydra-node` and `hydra-tui` for x86_64 Linux, aarch64 Linux and ARM64 MacOS platforms. When published, these binaries are attached to our [GitHub releases](https://github.com/cardano-scaling/hydra/releases). If the selected release has no binary attachments, use Docker or build that release from source. The x86_64 Linux ones are statically linked; the aarch64 Linux ones are not, as there is no musl cross build for that platform yet.

The [Binaries workflow](https://github.com/cardano-scaling/hydra/actions/workflows/binaries.yaml)
also builds an experimental x86_64 Windows artifact named
`hydra-x86_64-windows-<version>`. Download and extract the entire ZIP, keeping
the bundled DLLs alongside `hydra-node.exe`. The node embeds `etcd`; no separate
installation is needed. The archive contains only `hydra-node`, without
`hydra-tui`. See [Windows cross-build](#windows-cross-build) for usage and
current limitations.

:::info
Recent versions of MacOS block unverified binaries to protect your system.

> *Apple cannot verify that "hydra-node" is free of malware that may harm your Mac or compromise your privacy.*

To proceed, you’ll need to allow it manually in the **Privacy & Security** section of your system settings.
:::

### Build from source

While using Docker is the recommended way to _use_ Hydra, you can also build the `hydra-node` from source. We recommend using [Nix](https://nixos.org/download.html) for this and you can find instructions in our [contributing guidelines](https://github.com/cardano-scaling/hydra/blob/master/CONTRIBUTING.md).

### Windows cross-build

The experimental Windows target builds `hydra-node.exe` on x86_64 Linux with Nix:

```shell
nix build .#release-windows
```

The result is `result/hydra-x86_64-windows.zip`. Extract the entire archive into
one directory and keep the bundled DLLs alongside `hydra-node.exe`. The node
embeds its Windows `etcd.exe` and installs it in the persistence directory on
startup. No Haskell compiler or separate etcd installation is needed on Windows.

The ZIP includes the DLLs imported by the node and their transitive dependencies.
`WINDOWS-RUNTIME-NOTICES.txt` identifies their licenses, with the original license
and copyright texts in `licenses/`. `runtime-provenance.json` records each DLL's
SHA-256, Nix package, version, source revision or URL, and source hash. It also
records the Hydra revision and the commits, hashes and links for the Nix build
inputs. Source archives, patches and build recipes are not bundled. Keep the
notices and provenance with redistributions, and arrange any required
GPL/LGPL corresponding-source access separately; hashes alone do not provide it.
This inventory covers the DLLs and MinGW runtime; it does not enumerate all
Haskell or embedded etcd dependencies.

The cross-build compiles the Plutus contracts on Linux, then embeds those
artifacts using the `precompiled-scripts` Cabal flag. Both builds use the same
contract sources and dependency pins; the flag selects embedding directly in
the Haskell modules without rewriting their source.
Only the Head validator, Head minting policy and CRS validator use this path;
dummy and example validators are excluded from the Windows build. The deposit
validator is already embedded from its Aiken blueprint by both builds.

From PowerShell:

```powershell
.\hydra-node.exe --version
.\hydra-node.exe --help
.\hydra-node.exe gen-hydra-key --output-file hydra
```

The Binaries workflow produces the Windows artifact alongside the Linux and
macOS artifacts. Its nightly run at 04:00 UTC builds Windows and runs a smoke
test on Windows Server 2022 using that run's ZIP. Pushes, tags, and manual runs
only build the artifacts. The smoke test compares the script catalogue with the
native exporter and checks two offline nodes' API, peer connectivity, shutdown,
and restart from paths containing spaces.

Shutdown interrupts the embedded etcd process before joining its reader threads,
allowing blocked Windows reads to finish. If etcd does not exit within five
seconds, the node terminates it. The smoke test requires the node to report
`user interrupt` with exit code 1 (the current top-level exception handler's
normal interrupt outcome) and release its ports before restarting it with the
same persisted state. Other shutdown exceptions fail the test.

The archive includes only `hydra-node`. Live Cardano connectivity and TLS are
untested; a Cardano connection requires a separate Cardano node or Blockfrost.
