# head-state-viewer

An interactive, step-through visualizer for a Hydra node's `HeadLogic` state
machine. Point it at a node's SQLite event log (`hydra.db`) and scrub back and
forth through every `StateChanged`, watching the head's state, balance, peers,
snapshots and network activity evolve. It can also replay several nodes side by
side to spot divergence, follow a live node, and overlay a node's JSON trace log
to surface the `Wait` reasons the event store does not persist.

The UI is a [Miso](https://haskell-miso.org/) app served from native GHC over a
local Warp server (via `jsaddle-warp`): the binary opens a port and you point a
browser at it.

## Building and running

The viewer is part of the Hydra flake, so the quickest way to run it is:

```sh
nix run .#head-state-viewer -- --help
```

For local development, use `cabal` from inside the dev shell:

```sh
cabal run head-state-viewer -- --help
```

## Examples

Replay a single node's event log (Cardano `Tx` is the default):

```sh
nix run .#head-state-viewer -- --db path/to/hydra.db
# then open http://localhost:8080
```

Start at a particular event and pick a different port:

```sh
nix run .#head-state-viewer -- --db path/to/hydra.db --port 8087 --start-at 130
```

Follow a running node (polls the SQLite file for newly persisted rows):

```sh
nix run .#head-state-viewer -- --db ./hydra.db --follow
```

Overlay the node's JSON trace log to recover `Wait`/reliability reasons (these
are not stored in `hydra.db`):

```sh
nix run .#head-state-viewer -- --db ./hydra.db --log ./node.log
```

Compare several nodes side by side (repeat `--compare` once per node); node
labels come from the containing `state-N/` directory:

```sh
nix run .#head-state-viewer -- \
  --compare state-3/hydra.db \
  --compare state-4/hydra.db \
  --compare state-5/hydra.db
```

Run a self-contained 3-party `SimpleTx` simulation (no database needed); author
inputs with the preset buttons and watch messages route between parties:

```sh
nix run .#head-state-viewer -- --simulate 8080
```

Sanity-check the library wiring without the UI:

```sh
nix run .#head-state-viewer -- --smoke
```

### Cardano vs SimpleTx

Real node databases hold Cardano `Tx` events, which is the default. Databases
produced by `--gen-db` (see below) hold `SimpleTx` events; pass `--simple` to
interpret a database (or `--compare` set) as `SimpleTx`:

```sh
nix run .#head-state-viewer -- --db /tmp/sample.db --simple
```

## Generating example datasets

### A small SimpleTx database

`--gen-db` writes a tiny scripted `SimpleTx` event log (a `Tick`, a node-sync,
and a `HeadOpened`). It is the fastest way to get something on screen:

```sh
nix run .#head-state-viewer -- --gen-db /tmp/sample.db
nix run .#head-state-viewer -- --db /tmp/sample.db --simple
```

### A realistic Cardano database from a cluster test

To exercise the full lifecycle (open, deposit, transactions, snapshots, close,
fanout) against real Cardano `Tx` events, capture the databases written by a
`hydra-cluster` end-to-end test. The test temp directory is normally cleaned up
on success; set `NO_CLEANUP=1` to keep it and print its path:

```sh
cabal build hydra-node:exe:hydra-node hydra-chain-observer:exe:hydra-chain-observer
TMPDIR=/tmp/hydra-capture NO_CLEANUP=1 cabal test hydra-cluster \
  --test-option=--pattern='/processes a single Cardano transaction/'
```

The test prints `NO_CLEANUP set, leaving temp dir: <dir>`. Each node's database
is at `<dir>/state-N/hydra.db` (and `node.log` alongside it for `--log`). For a
standalone copy with the write-ahead log folded in:

```sh
sqlite3 <dir>/state-3/hydra.db ".backup '/tmp/example.db'"
sqlite3 /tmp/example.db "PRAGMA wal_checkpoint(TRUNCATE);"
```

Then replay it (Cardano is the default, so no `--simple`):

```sh
nix run .#head-state-viewer -- --db /tmp/example.db
```

## Developing the UI

The served HTML is an empty shell whose DOM is built over a websocket, so a
one-shot headless screenshot captures a blank page. A dedicated dev shell
provides Playwright (with NixOS-patched browsers, nothing downloaded at runtime)
for scripted screenshots and click-through:

```sh
nix run .#head-state-viewer -- --db /tmp/example.db &   # serve in the background
nix develop .#headStateUI -c node head-state-viewer/tooling/shoot.mjs \
  http://localhost:8080 /tmp/shots
```
