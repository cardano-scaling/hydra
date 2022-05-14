# Overview

Sources for the [user manual 📖](https://input-output-hk.github.io/hydra-poc).

# Building 

The user-manual is built using [Docusaurus 2](https://docusaurus.io/), which combines React components and markdown into a customisable static website. Docusaurus supports a set of plugins and basic features (coming in the form of _'presets'_). The basic set contains in particular three types of documents:

- pages
- docs
- blog posts

Our setup here _hijack_ the blog posts for showing [architectural decision records](https://input-output-hk.github.io/hydra-poc/adr), which gives us a nice way to view them and browse them by tags. Pages can be used for custom pages such as an API reference using full-blown React components. Finally, docs are the most common and translate markdown into nicely structured pages.

#### Installation

```
$ yarn
```

#### Local Development

```
$ yarn start
```

This command starts a local development server and opens up a browser window. Most changes are reflected live without having to restart the server.

#### Build

```
$ yarn build
```

This command generates static content into the `build` directory and can be served using any static contents hosting service.


# Translating

Translations of the documentation are provided in the `i18n/{lang}` folder (for example `i18n/fr` for French). Translations of both the content and the various website elements (such as buttons, headers etc...) are needed. To initialize a new language translation (e.g. `fr`), run the following command:

```
yarn write-translations --locale fr
```

This command will pre-generate all the website elements which need to be translated into French (locale `fr`) in JSON files. Content files themselves then to be copied under their respective directories, and then translated. Here is a table that summarizes the correspondence between the default website structure and their localized versions:

| default          | translations                                                |
| -----            | -----                                                       |
| `docs/`          | `i18n/{lang}/docusaurus-plugin-content-docs/`               |
| `benchmarks/`    | `i18n/{lang}/docusaurus-plugin-content-docs-benchmarks/`    |
| `core-concepts/` | `i18n/{lang}/docusaurus-plugin-content-docs-core-concepts/` |
| `topologies/`    | `i18n/{lang}/docusaurus-plugin-content-docs-topologies/`    |
| `adr/`           | `i18n/{lang}/docusaurus-plugin-content-blog/`               |
