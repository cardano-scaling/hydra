# Overview

Sources for the [user manual 📖](https://hydra.family/head-protocol/docs).

# Building

The user-manual is built using [Docusaurus 3](https://docusaurus.io/), which combines React components and markdown into a customisable static website. Docusaurus supports a set of plugins and basic features (coming in the form of _'presets'_). We use it to create the actual user manual (docs), documenting our [architectural decision records](https://hydra.family/head-protocol/adr), a custom page for the [API reference](https://hydra.family/head-protocol/api-reference), and various other documentation pages around the Hydra Head protocol.

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
nix build .#spec && ln -s $(readlink result)/hydra-spec.pdf static/hydra-spec.pdf
nix build .#haddocks && \
      rm -rf static/haddock && \
      mkdir -p static/haddock && \
      cp -r result/* static/haddock && \
      chmod +rwx -R static/haddock
```

# Style guide

Please follow this [technical writing style guide](./standalone/writing-style-guide.md) when contributing changes to documentation.
