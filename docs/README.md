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
