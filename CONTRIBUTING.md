# Contributing to Hydra

Thanks for considering contributing and help us on creating the Hydra protocols! 

The best way to contribute right now is to try things out and provide feedback,
but we also accept contributions to the documentation and the obviously to the
code itself.

This document is contains guidelines to help you get started and how to make sure your contribution gets accepted, making you our newest Hydra contributor!

## Communication channels

Should you have any questions or need some help in getting set up, you can use
these communication channels to reach the Hydra team and get answers in a way
where others can benefit from it as well:

- #ask-hydra on the IOG [Discord server](https://discord.gg/Qq5vNTg9PT)
- Github [Discussions](https://github.com/input-output-hk/hydra-poc/discussions)
- Cardano [StackExchange](https://cardano.stackexchange.com/) using the `hydra` tag

## Your first contribution 

Contributing to the documentation, its translation, reporting bugs or proposing features are awesome ways to get started.

### Documentation + translations

We host our documentation / user manual as a website [here](https://input-output-hk.github.io/hydra-poc).

Each page has an "Edit this page" button which should take you to the source
file containing the markup. Should you would want to extend the documentation or
find some errors, please file an issue pointing to the mistake or even better,
create a pull request with the changes directly!

The documentation is also available in multiple languages and we can easily add more languages. If you want to extend, update or contribute a new translation make sure to check out the instructions [in the docs component](./docs/README.md#Translating).

### Bug reports

[Submit an issue](https://github.com/input-output-hk/hydra-poc/issues/new/choose) using the "Bug report :bug:" template.

For bug reports, it's very important to explain
* what version you used,
* steps to reproduce (or steps you took),
* what behavior you saw (ideally supported by logs), and
* what behavior you expected.

### Feature ideas

[Submit an issue](https://github.com/input-output-hk/hydra-poc/issues/new/choose) using the "Feature idea :though_balloon:" template.

Feature ideas are precursors to high-level features items, which will be discussed and fleshed out to ideally become items on our feature roadmap.

We expect a description of
* why you (or the user) need/want something (e.g. problem, challenge, pain, benefit), and
* what this is roughly about (e.g. description of a new API endpoint or message format).

Note that we do NOT require a detailed technical description, but are much more
interested in *why* a feature is needed. This also helps in understanding the
relevance and ultimately the priority of such an item.

## Making changes

When contributing code, it helps to have discussed the rationale and (ideally)
how something is implemented in a feature idea or bug ticket beforehand.

### Building & Testing

We use and require [nix](https://nixos.org/download.html) to provide a
consistent development environment via a `shell.nix`. So a simple call to
`nix-shell` should put everything in place for building, testing and general
development.

Make sure the following caches are listed in your `nix.conf` for a speedy setup:

```
substituters = https://cache.nixos.org https://iohk.cachix.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

From there you can

* Build the project all its executables using `nix-build`
* Build the `hydra-node` docker image: `docker build . --target hydra-node`
- Build & run the `hydra-node`: `cabal build hydra-node && cabal exec hydra-node -- --version`
- Build & run all tests: `cabal test all` 
- Build & run all benchmarks: `cabal bench all`
- Run `haskell-language-server` for an IDE experience
- Run `hoogle` for symbol & documentation lookup

Also, some of us use [direnv](https://direnv.net/) and
[nix-direnv](https://github.com/nix-community/nix-direnv) to automatically
import the nix-shell environment into our favorite shell or editor and not need
explicit call to `nix-shell`.

Besides these general build instructions, some components might document
additional steps and useful tools in their `README.md` files, e.g. the
[docs](./docs/README.md) or the [hydra-cluster](./hydra-cluster/README.md)

### Coding standards

Make sure to follow our [Coding
Standards](https://github.com/input-output-hk/hydra-poc/wiki/Coding-Standards).
It includes guidelines on Haskell code style, but also on Git commit messages
and some processes (TODO: clarify separation or unify with these guidelines). To
propose new standards or changes to the existing standards file an issue.

### Creating a pull request

We would like to invite anyone to open pull requests (thanks!), but usually require:
  + description of the changes - if your commit messages are great, this is less important
  + change is related to an issue, feature (idea) or bug report - ideally discussed beforehand
  + well-scoped - rather multiple PRs, than a big one
  + quality & maintenance is ensured - usually means automated tests are covering the change

### Versioning & Changelog

During development
  + Make sure `CHANGELOG.md` is kept up-to-date with high-level, technical, but user-focused list of changes according to [keepachangelog](https://keepachangelog.com/en/1.0.0/)
  + Bump `UNRELEASED` version in `CHANGELOG.md` according to [semver](https://semver.org/)
  + All `hydra-` packages are versioned the same, at latest on release their versions are aligned.
  + Other packages are versioned independently of `hydra-` packages and keep a dedicated changelog.

### Releasing

To perform a release
  + Check version to be released is also correct in software components, e.g. `.cabal` files.
  + Replace `UNRELEASED` with a date in [ISO8601](https://en.wikipedia.org/wiki/ISO_8601)
  + Create a signed, annotated git tag of the version: `git tag -as <version>`
  + (ideally) Use the released changes as annotation
