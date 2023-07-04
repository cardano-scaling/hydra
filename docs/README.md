# Overview

Sources for the [user manual 📖](https://input-output-hk.github.io/hydra).

# Building

The user-manual is built using [Docusaurus 2](https://docusaurus.io/), which combines React components and markdown into a customisable static website. Docusaurus supports a set of plugins and basic features (coming in the form of _'presets'_). The basic set contains in particular three types of documents:

- pages
- docs
- blog posts

Our setup here _hijack_ the blog posts for showing [architectural decision records](https://input-output-hk.github.io/hydra/adr), which gives us a nice way to view them and browse them by tags. Pages can be used for custom pages such as an API reference using full-blown React components. Finally, docs are the most common and translate markdown into nicely structured pages.

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
right place before, you can use these `nix` builds from the repository root:

```console
nix build .#spec && ln -s $(readlink result)/hydra-spec.pdf docs/static/hydra-spec.pdf
nix build ".?submodules=1#haddocks" -o docs/static/haddock

(cd hydra-node; nix develop ".?submodules=1#benchs.hydra-node" --command tx-cost --output-directory $(pwd)/../docs/benchmarks)
(cd hydra-cluster; nix develop ".?submodules=1#benchs.hydra-cluster" --command bench-e2e --scaling-factor 1 --output-directory $(pwd)/../docs/benchmarks)
```

# Translating

Translations of the documentation are provided in the `i18n/{lang}` folder (for example `i18n/fr` for French). Translations of both the content and the various website elements (such as buttons, headers etc...) are needed. To initialize a new language translation (e.g. `fr`), run the following command:

```console
$ yarn write-translations --locale fr
```

This command will pre-generate all the website elements which need to be translated into French (locale `fr`) in JSON files. Translations have to be provided for each items in generated JSON files, mapping ids to messages as such:

```json
{
  "theme.TOCCollapsible.toggleButtonLabel": {
    "message": "Sur cette page",
    "description": "The label used by the button on the collapsible TOC component"
  }
}
```

> Note that only the value of the key `message` needs to be translated, the `description` simply provides context regarding the translated element and is automatically generated.

In addition content files themselves (markdown) need to be copied under their respective directories, and then translated. Here is a table that summarizes the correspondence between the default website structure and their localized versions:

| default          | translations                                                        |
| ---------------- | ------------------------------------------------------------------- |
| `docs/`          | `i18n/{lang}/docusaurus-plugin-content-docs/current/`               |
| `benchmarks/`    | `i18n/{lang}/docusaurus-plugin-content-docs-benchmarks/current/`    |
| `core-concepts/` | `i18n/{lang}/docusaurus-plugin-content-docs-core-concepts/current/` |
| `topologies/`    | `i18n/{lang}/docusaurus-plugin-content-docs-topologies/current/`    |
| `adr/`           | `i18n/{lang}/docusaurus-plugin-content-blog/current/`               |
