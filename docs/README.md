# Overview

Sources for the [user manual 📖](https://hydra.family/head-protocol/docs).

# Building

The user-manual is built using [Docusaurus 3](https://docusaurus.io/), which combines React components and markdown into a customisable static website. Docusaurus supports a set of plugins and basic features (coming in the form of _'presets'_). We use it to create the actual user manual (docs), documenting our [architectural decision records](https://hydra.family/head-protocol/adr), a custom page for the [API reference](https://hydra.family/head-protocol/api-reference), and various other documentation pages around the Hydra Head protocol.


> [!NOTE]
> You need to enter a `nix develop` shell before building the docs.

Run the Yarn commands below from the `docs/` directory.

#### Installation

```console
$ yarn
```

#### Local Development

```console
$ yarn start
```

This command starts a local development server and opens up a browser window. Most changes are reflected live without having to restart the server.

#### Build

```console
$ yarn build
```

> [!NOTE]
> This now builds the Haskell source code; so may result in long compilation time.

This command generates static content into the `build` directory and can be served using any static contents hosting service.

Note that this will have quite some broken links as we are referring to
generated documentation, test data and benchmarks. To put these artifacts at the
right place before, you can use these `nix` builds from this folder:

```console
nix build .#spec && ln -sfn "$(readlink result)/hydra-spec.pdf" static/hydra-spec.pdf
nix build .#haddocks && \
      rm -rf static/haddocks && \
      mkdir -p static/haddocks && \
      cp -r result/* static/haddocks && \
      chmod -R u+w static/haddocks
```

# Style guide

Please follow this [technical writing style guide](./standalone/writing-style-guide.md) when contributing changes to documentation.

## Diagrams

Maintain most diagrams directly as editable SVG files. Keep text as text
and use explicit positions so fonts and automatic layout do not change the
result.

Regenerate the C4 diagram from its [PlantUML source](./docs/dev/architecture/architecture-c4.puml)
and flamegraphs using the [profiling workflow](./benchmarks/profiling.md#produce-a-function-level-profile).
These generated diagrams keep their own styles. Historical ADR illustrations
retain their original format.

For manually maintained SVGs, use the head lifecycle diagram as the style
reference: Arial, Helvetica, sans-serif; rounded cards; open arrowheads; and
the following colors:

| Element | Color |
| --- | --- |
| Headings | `#194d46` |
| Body text | `#415955` |
| Secondary labels | `#526761` |
| Arrows | `#527b76` |
| Card fill / border | `#f0f7f5` / `#b5d0c9` |
| Emphasis fill / border | `#e0eee9` / `#8fb8ac` |

For article diagrams, start with a 560px-wide canvas, 20px headings, and
16px body text. Prefer wrapping labels and stacking groups to widening the
canvas. Keep visible titles in the page; include an accessible SVG `title`
and `desc`. Homepage illustrations may use larger text and visible titles.

Inspect each diagram in its page at desktop and mobile widths. Check that
labels fit, arrows connect clearly, and the surrounding explanation uses
the same terms and sequence.

Use descriptive alternative text for diagrams and explain their meaning in
the surrounding prose. Prefer copyable commands to screenshots of terminal
output. Keep screenshots only when they explain an interface that text cannot.

### Head-logic diagrams

Run `yarn head-logic-diagrams` after any relevant code changes to get new
pictures of the crem workflows.

### Head playground

The public playground at `/docs/head-playground` uses [Dragology](https://github.com/declarative-dragging/dragology)
for the funds board and the production Hydra Crem workflows for every protocol
transition. React holds only setup, action history and the returned view. The
Haskell shell gives each operator its own `NodeState SimpleTx`, delivers effects
and feeds resulting events into `State.Node.applyEvents`. Rewind replays inputs.
This page hides its sidebar and pagination through its front matter. Step
explanations, message history, and runtime details are expandable.

From `docs/`, build the runtime, then start the docs server:

```console
yarn playground:runtime
yarn docusaurus start --host 127.0.0.1
```

The runtime build uses the unmodified GHC 9.10 JavaScript backend and only Crem's
main library. The compiler is available from Nix's binary cache on macOS.
This runtime needs no C functions, so it uses
[`-ddisable-js-c-sources`](https://downloads.haskell.org/ghc/9.10.3/docs/users_guide/debugging.html#ghc-flag-ddisable-js-c-sources)
to omit C/WebAssembly linking and avoid macOS archive-padding errors without
rebuilding GHC. Adding dependencies that need C or Emscripten requires revisiting
this option.
Rebuild when Haskell sources change. The generated bundle is ignored by Git.
`nix build .#docs` includes it through the standalone `head-playground-runtime`
package. That package does not build Agda, the spec or Cardano cryptography.

`runtime/head-playground-runtime.cabal` compiles the production source files
directly through `hs-source-dirs`. Shared interface modules separate protocol
types from Cardano and networking integration. Internal libraries preserve each
source package's language settings, including the node's `StrictData` and the
transaction library's lazy accumulator caches. `runtime/portable`
contains platform adapters: symbolic identities, signature witnesses and content
commitments. `Hydra.Tx.Crypto.MultiSignature` is shared production source;
only the individual signature primitives have a symbolic implementation in
`portable/tx/Hydra/Tx/Crypto/Signature.hs`. The portable `Crypto.hs` only re-exports
these modules. Signature ordering, count checks and verification errors have
one implementation for both builds.
The runtime also compiles `haskell-accumulator`'s original `Accumulator` module
from the pinned CHaP archive. Its version comes from `hydra-tx.cabal`, so the
native and browser builds use the same multiset operations. Only element hashes
and commitments are symbolic. `Hydra.Tx.Accumulator.Core` shares the UTxO
operations, capacity bound and cached commitment handling.
Party and Head identifier types, hex codecs, snapshot field selection and Host
parsing are shared source as well. Native snapshot encoding uses Plutus data;
the browser supplies a small `Snapshot.Encoding` backend without Plutus shims.
The adapters supply no replacement protocol guards or state transitions.
The actual SimpleTx ledger, snapshot signing field selection, accumulator delta
arithmetic, input workflows, interpreter, retry policy and event machines run
unchanged. The Cabal file lists the modules compiled by each internal library;
there is no Haskell source generation or text extraction step.

The remaining portable modules contain symbolic keys, hashes, commitments and
snapshot encoding; platform type stand-ins; module re-exports; and explicit
failures for unsupported CBOR persistence and network encoding.
The small re-export modules keep existing Hydra imports without pulling in
their native Cardano, networking or cryptographic implementations.

The shell simulates L1 observations and an ordered broadcast network. Deposits
are external wallet submissions; confirmation and activation are separate.
Each AckSn is delivered separately, including to its sender. Operators can post
the same settlement; one confirmed L1 transaction wins. Wallet ownership,
ADA amounts and the sample DEMO native token are presentation metadata around
SimpleTx output identities. The public controls finish one request at a time.
Recovery, Cardano validators, transaction construction and CBOR persistence are
outside this walkthrough.

The view asks the workflow which L2 moves are accepted and uses the production
deposit activation and withdrawal settlement helpers. Its locks and reservations
control the walkthrough's pacing; wallet ownership and L1 confirmation are
simulated inputs, not additional protocol validation.

The payment form and L2 dragging share the Haskell transaction builder. The
form can spend several outputs in one `NewTx`. Reservation labels come from
queued deposit, payment and withdrawal requests and follow the replay history.
The token swap spends both wallets’ outputs in one `NewTx`. Closing, the
contestation deadline notification and fanout use the production workflows;
the shared fanout-output helper determines which closed funds remain visible.

Run `yarn test:head-playground` after building the runtime. It exercises the
compiled JavaScript, including all operator counts, wallet routing, repeated
cycles, payments with multiple inputs, reservations, individual signature
delivery, token swaps, close and fanout, settlement boundaries, invalid requests
and ADA/token conservation.
`just check` runs this check using the Nix-built bundle.

For conformance against the native implementation, run from the repository root
in the usual Haskell shell:

```console
cabal build lib:hydra-node
cabal exec -- node docs/scripts/check-playground-parity.mjs
```

This compiles the same shell against the real hydra-node/hydra-tx libraries,
without portable adapters. It compares intermediate node states, events,
effects and decision traces with the JavaScript artifact. Only identity
encodings, signature payloads and commitment hashes are normalized; signature
counts, ledger contents and protocol fields remain in the comparison.

With docs running, check mouse/touch dragging, keyboard actions, replay,
inspection, mobile layout and errors in a real browser:

```console
nix develop .#headStateUI -c node docs/scripts/check-head-playground.mjs http://127.0.0.1:3000/head-protocol/docs/head-playground
```
