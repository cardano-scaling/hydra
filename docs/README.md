# Overview

Sources for the [user manual 📖](https://hydra.family/head-protocol/docs).

# Building

The user-manual is built using [Docusaurus 3](https://docusaurus.io/), which combines React components and markdown into a customisable static website. Docusaurus supports a set of plugins and basic features (coming in the form of _'presets'_). We use it to create the actual user manual (docs), documenting our [architectural decision records](https://hydra.family/head-protocol/adr), a custom page for the [API reference](https://hydra.family/head-protocol/api-reference), and various other documentation pages around the Hydra Head protocol.

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

Maintain current diagrams directly as editable SVG files. Keep text as text
and use explicit positions so fonts and automatic layout do not change the
result. Historical ADR illustrations retain their original format.

Use the head lifecycle diagram as the style reference: Arial, Helvetica,
sans-serif; rounded cards; open arrowheads; and the following colors:

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
