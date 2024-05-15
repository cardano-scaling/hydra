# Contributing to Hydra

Thanks for considering contributing and help us on creating the Hydra protocols!

The best way to contribute right now is to try things out and provide feedback,
but we also accept contributions to the documentation and the obviously to the
code itself.

This document contains guidelines to help you get started and how to make sure
your contribution gets accepted, making you our newest Hydra contributor!

## Ask for help

See [`SUPPORT.md`](SUPPORT.md) should you have any questions or need some help in getting set up.

## Your first contribution

Contributing to the documentation, its translation, reporting bugs or proposing features are awesome ways to get started.

### Documentation + translations

We host our documentation / user manual as a website [here](https://input-output-hk.github.io/hydra).

Each page has an "Edit this page" button which should take you to the source
file containing the markup. Should you want to extend the documentation or
find some errors, please file an issue pointing to the mistake or even better,
create a pull request with the changes directly!

The documentation is also available in multiple languages and we can easily add more languages. If you want to extend, update or contribute a new translation make sure to check out the instructions [in the docs component](./docs/README.md#Translating).

### Bug reports

[Submit an issue](https://github.com/input-output-hk/hydra/issues/new/choose)
using the "Bug report :bug:" template. It's very important that you fill the
template as thoroughly as possible.

### Feature ideas

Feature ideas are precursors to high-level features items, which will be
discussed and fleshed out to ideally become items on our feature roadmap.

We use the [Ideas discussions
category](https://github.com/input-output-hk/hydra/discussions/categories/ideas)
to discuss and vote on feature ideas, but you can also [submit an
issue](https://github.com/input-output-hk/hydra/issues/new/choose) using the
"Feature idea :thought_balloon:" template and we convert that to a discussion.

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
consistent development environment via our `flake.nix`.

Make sure that you have activated flakes in your `nix.conf`:

```
substituters = https://cache.iog.io https://cache.nixos.org
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
experimental-features = nix-command flakes
```

<details>
<summary>For Mac OS X users</summary>

Daemon mode is the recommended way to install and run nix on Mac OS. Therefore,
you might want to add yourself as a _trusted user_ in order to ensure flake's
substituters will be picked up:

```
build-users-group = nixbld
trusted-users = root pascal
```

</details>

Then `nix build` will ask you about using our caches and download or build the
`hydra-node` into `result/bin/hydra-node`. Other things you might want to build:

- Build the `hydra-node` using nix: `nix build .#hydra-node`
- Build a statically linked `hydra-node` using nix: `nix build .#hydra-node-static`
- Build the `hydra-node` docker image: `nix build .#docker-hydra-node`
- Build the specification as pdf: `nix build .#spec`

For development, a call to `nix develop` will put everything in place so that
you can:

- Build & run the `hydra-node`: `cabal build hydra-node && cabal exec hydra-node -- --version`
- Build & run all tests: `cabal test all`
- Build & run all benchmarks: `cabal bench all`
- Format code as enforced by CI: `treefmt`
- Run `haskell-language-server` for an IDE experience
- Run `hoogle` for symbol & documentation lookup

Also, some of us use [direnv](https://direnv.net/) and
[nix-direnv](https://github.com/nix-community/nix-direnv) to automatically
import the nix environment into our favorite shell or editor and not need
explicit call to enter the nix shell.

Besides these general build instructions, some components might document
additional steps and useful tools in their `README.md` files, e.g. the
[docs](./docs/README.md) or the [hydra-cluster](./hydra-cluster/README.md)

While warnings are not treated as errors during builds, CI will check for it
before we merge any contributions.

### Coding standards

Make sure to follow our [Coding
Standards](https://github.com/input-output-hk/hydra/wiki/Coding-Standards).
It includes guidelines on Haskell code style, but also on Git commit messages
and some processes (TODO: clarify separation or unify with these guidelines). To
propose new standards or changes to the existing standards, file an issue.

### Creating a pull request

Thank you for contributing your changes by opening a pull requests!

On top of the aforementioned _Coding standards_, to get something merged would usually require:
+ Description of the changes - if your commit messages are great, this is less important
+ Quality of changes is ensured - through new or updated automated tests
+ Change is related to an issue, feature (idea) or bug report - ideally discussed beforehand
+ Well-scoped - we prefer multiple PRs, rather than a big one
+ Correctness - a PR should pass all the build and test steps in the CI

Merging a PR requires approval from a majority of reviewers from the
core _Hydra engineering_ team. This implies that:
+ Contributions from outside this core team would need 3 approvals,
+ Contributions from a single core team member would need a single approval,
+ PRs developed in pair would need a single approval,
+ and of course PRs developed in ensembles would not need any approval.

The reason behind this policy is to increase the [bus
factor](https://en.wikipedia.org/wiki/Bus_factor) on the project,
disseminating information about contributions and changes in order to
prevent creation of a bottleneck.

### Updating dependencies

#### From Hackage

Updating package dependencies from Hackage should work like normal in a Haskell
project. The most important thing to note is that we pin the `index-state` of
the Hackage package index in `cabal.project`. This means that cabal will always
see Hackage “as if” it was that time, ensuring reproducibility. But it also
means that if you need a package version that was released *after* that time,
you need to bump the `index-state` (and to run ``cabal update`` locally).

Because of how we use Nix to manage our Haskell build, whenever you do this you
will also need to pull in the Nix equivalent of the newer `index-state`. You can
do this by doing a `nix flake lock --update-input haskellNix/hackage`.

#### From Cardano Haskell Packages (CHaP)

Many Cardano packages are not on Hackage and are instead in the [Cardano Haskell
Packages (CHaP)](https://github.com/input-output-hk/cardano-haskell-packages)
see the README for (lots) more information. Getting new packages from there
works much like getting them from Hackage. The differences are that it has an
independent `index-state`, and that there is the `CHaP` flake input which you
need to bump with `nix flake lock --update-input CHaP`.

#### Using unreleased versions of dependencies

Sometimes we need to use an unreleased version of one of our dependencies,
either to fix an issue in a package that is not under our control, or to
experiment with a pre-release version of one of our own packages. You can use a
`source-repository-package` stanza to pull in the unreleased version. Do this
**only** for a short period of time, and try to get your changes released
upstream. If that is (really) not possible, we can also release a patched
version into CHap, see their
[README](https://github.com/IntersectMBO/cardano-haskell-packages) for
instructions.

### Versioning & Changelog

During development
+ Make sure `CHANGELOG.md` is kept up-to-date with high-level, technical, but user-focused list of changes according to [keepachangelog](https://keepachangelog.com/en/1.0.0/)
+ Bump `UNRELEASED` version in `CHANGELOG.md` according to [semver](https://semver.org/)
+ Ensure `unstable` version of docker images is used in demo
  - `sed -i.bak -e "s,\(ghcr.io/input-output-hk/hydra-[^:]*\):[^[:space:]]*,\1:unstable," demo/*`
+ All `hydra-` packages are versioned the same, at latest on release their versions are aligned.
+ Other packages are versioned independently of `hydra-` packages and keep a dedicated changelog.

### Releasing

To perform a release of next `<version>`:

1. Make sure all tests are passing.
2. Publish hydra scripts onto `preview`, `preprod`, and `mainnet` using the
   [smoke test][smoke-test] and put the transaction ids as new `<version>`
   entries into [networks.json](./networks.json).
3. Update CHANGELOG.md by replacing `UNRELEASED` with a date in
   [ISO8601](https://en.wikipedia.org/wiki/ISO_8601) and prepare contents.
4. Run `./release.sh <version>`
5. Check if all the bumped versions are correct. In particular, whether the demo
   and tutorial would still work given the changelog. If everything is fine,
   push the branches `master`, `release` and the `<version>` tag.
6. Create a github release page containing
   * The released changes (formatted) and giving credit where credit is due
   * Built hydra (and cardano-node) binaries to the release using naming scheme:
     `hydra-<platform>-<version>.zip` where `platform` is `x86_64-linux` or
     `aarch64-darwin` (the same for `cardano-node` instead of `hydra`)
   * The just published `hydra-scripts-tx-id` from step 1

[smoke-test]: https://github.com/input-output-hk/hydra/actions/workflows/smoke-test.yaml

## Communication

We have several reporting and communication practices within the Hydra project.
Becoming one of the core contributors of the project includes participation in
core communication processes.

### Weekly update

At the end of each week, we provide updates to
[cardano-updates](https://IntersectMBO.github.io/cardano-updates/tags/hydra),
which also serve as a basis for our monthly reports.

To write such an update:

1. Use the contributors tactical meeting agenda to collect bullet points on
   - **What did the team achieve this week**: past tense summary of done things
     (good: "Implemented..", bad: "Started working on.."); use the backlog and
     calendars to collect

   - **What are the goals of next week**: a short look-out onto the backlog and/or
     roadmap; ask each contributor what they would like to get done next week; always start from scratch and don't carry over things from last week.

2. Enrich the content with useful links and write a high-level summary. This
   should use a passive or "they"-style of writing. Tip: check out older updates
   for examples and/or use an AI language model to write it.

3. Create a entry blog and pull request it on
   [cardano-updates](https://github.com/IntersectMBO/cardano-updates),
   putting other Hydra contributors as reviewers.

   - Don't forget to add yourself as author in the `authors.yml`.

### Monthly review & report

To keep our users and stakeholders up-to-date regularly, we organize a review meeting and publish a report every month to [the project website](https://cardano-scaling.github.io/website/monthly).

To conduct this meeting and write this report:

1. Monthly meeting slides & report preparation:
    - Ensure the Google Meet invitation exists for this month and check stakeholder attendance

    - Copy monthly slides, change title and template accordingly to `<Month> <Year>`
    - Remove old "content" slides and mark the regular slides to be updated

    - Decide what to demonstrate (this is not optional!), either from the core
      development or an invited demo/showcase from the community

    - Update delivered / current / next features
    - Expand some of those into interesting "Development" topics
    - Summarize what happened in the Hydra "Community"
      + Reach out to individuals from the community whether they want to contribute something to the monthly

    - Update roadmap for this month
      + Use the grooming/planning session before latest monthly to get an updated [roadmap](https://github.com/orgs/input-output-hk/projects/21)
      + Store latest roadmap screenshot in slides and on [miro](https://miro.com/app/board/uXjVMA4bSao=/), with and without 💭 **idea** items
      + Check milestones and versions are consistent
      + Mark up and summarize notable changes in the roadmap to last month to support slides
    - Update repository status in slides using <https://github.com/input-output-hk/hydra/pulse/monthly>

    - Prepare invitation, by creating an event in discord, update the invitation used last month with it.
    - Send invitation to Hydra and Mithril #announcements channels on our discord server.

2. Conduct the meeting
    - Do not forget to record the meeting!
    - Ask for sharing rights after the meeting, by requesting sharing in Google Drive with yourself, and/or ask for making it publicly available.

3. Write the report in [this repository](https://github.com/cardano-scaling/website)
  - Copy monthly report from last month, update dates, links to slides and closed issues
  - Use slides as starting point, to provide a short and sweet written account of what happened
    + The previous reports should serve as a guideline
    + Distribute write-up between relevant contributors
  - Cross-link slides, recording and report
  - Pull request, review and merge to publish the report

NOTE: Drafting the report (step 3) is also a good preparation for presenting the individual sections.
