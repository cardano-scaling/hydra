# Contributing to Hydra

Thank you for considering contributing and helping us create the Hydra protocols!

The best way to contribute right now is to try things out and provide feedback. We also welcome contributions to the documentation and, of course, the code itself.

This document contains guidelines to help you get started and ensure your contribution gets accepted, making you our newest Hydra contributor!

## Ask for help

See [`SUPPORT.md`](SUPPORT.md) if you have any questions or need help setting up.

## Your first contribution

Contributing to the documentation, translating it, reporting bugs, or proposing features are excellent ways to get started.

### Documentation

Documentation and a user manual are available on [this website](https://hydra.family/head-protocol/).

Each page has an 'Edit this page' button that takes you to the source file containing the markup. If you want to extend the documentation or find errors, please file an issue pointing to the mistake or, even better, create a pull request with the changes directly.

Please follow this [technical writing style guide](./docs/standalone/writing-style-guide.md) when contributing changes to documentation.

### Bug reports

[Submit an issue](https://github.com/cardano-scaling/hydra/issues/new/choose)
using the 'Bug report :bug:' template. It's very important that you fill the
template as thoroughly as possible.

### Feature ideas

Feature ideas are precursors to high-level feature items, which will be
discussed and fleshed out to ideally become items on our feature roadmap.

We use the ['Ideas' discussions
category](https://github.com/cardano-scaling/hydra/discussions/categories/ideas)
to discuss and vote on feature ideas. You can also [submit an
issue](https://github.com/cardano-scaling/hydra/issues/new/choose) using the
'Feature idea :thought_balloon:' template, and we will convert that to a discussion.

We expect a description of:

* why you (or the user) need/want something (eg, problem, challenge, pain, benefit)
* what this is roughly about (eg, description of a new API endpoint or message format).

Note that we do NOT require a detailed technical description, but are much more interested in *why* a feature is needed. This helps us understand the relevance and, ultimately, the priority of such an item.

## Making changes

When contributing code, it is helpful to have discussed the rationale and (ideally) the implementation details in a feature idea or a bug ticket beforehand.

### Building and testing

We use and require [Nix](https://nixos.org/download.html) to provide a
consistent development environment via our `flake.nix`.

Make sure that you have activated flakes in your `nix.conf`:

```
substituters = https://cache.iog.io https://cache.nixos.org
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
experimental-features = nix-command flakes
```

<details>
<summary>For macOS users</summary>

Daemon mode is the recommended way to install and run Nix on macOS. Therefore, you might want to add yourself as a _trusted user_ to ensure that Flake's substitutes will be picked up:

```
build-users-group = nixbld
trusted-users = root pascal
```

</details>

Then `nix build` will ask you about using our caches and download or build the
`hydra-node` into `result/bin/hydra-node`. You might also be interested in the following:

- Build the `hydra-node` using Nix: `nix build .#hydra-node`
- Build a statically linked `hydra-node` using Nix: `nix build .#hydra-node-static`
- Build the `hydra-node` Docker image `nix build .#docker-hydra-node` and load it with `docker load < result`
- Build the specification as a PDF: `nix build .#spec`.

For development, a call to `nix develop` will set up the environment so you can:

- Build and run the `hydra-node`: `cabal build hydra-node && cabal exec hydra-node -- --version`
- Build and run all tests: `cabal test all`
- Build and run all benchmarks: `cabal bench all`
- Format code as enforced by CI: `treefmt`
- Use `haskell-language-server` for an IDE experience
- Use `hoogle` for symbol and documentation lookup.

Some of us use [direnv](https://direnv.net/) and [nix-direnv](https://github.com/nix-community/nix-direnv) to automatically import the Nix environment into our preferred shell or editor, eliminating the need for an explicit call to enter the Nix shell.

In addition to these general build instructions, individual components might have additional steps and useful tools documented in their `README.md` files. For example, see the documentation for [docs](./docs/README.md) or [hydra-cluster](./hydra-cluster/README.md).

While warnings are not treated as errors during builds, CI will check for them before we merge any contributions.

### Coding standards

Be sure to follow our [coding standards](https://github.com/cardano-scaling/hydra/wiki/Coding-Standards), which include guidelines on Haskell code style, Git commit messages, and various processes. (TODO: clarify separation or unify with these guidelines). To propose new standards or suggest changes to existing ones, please file an issue.

### Creating a pull request

Thank you for contributing your changes by opening a pull request!

In addition to adhering to the _coding standards_, the following criteria are generally required to get a pull request merged:

+ **Description of the changes**: providing a clear summary of the changes is beneficial
+ **Quality of changes**: ensure that new or updated automated tests validate your changes
+ **Issue or feature relevance**: the change should be related to an existing issue, feature idea, or bug report, ideally discussed beforehand
+ **Scope**: we prefer multiple pull requests that are well-scoped rather than a single large one
+ **Correctness**: the pull request should pass all build and test steps in the continuous integration (CI) pipeline.

Merging a pull request requires approval from a majority of reviewers from the core _Hydra engineering_ team. This means:

+ Contributions from outside the core team need *three* approvals
+ Contributions from a single core team member need *one* approval
+ Pull requests developed in pairs need *one* approval
+ Pull requests developed in ensembles do not require additional approvals.

This policy aims to increase the [bus factor](https://en.wikipedia.org/wiki/Bus_factor) of the project by distributing knowledge about contributions and changes, thereby preventing the creation of bottlenecks.

### Updating dependencies

#### Update Early And Often

Update dependencies as part of routine maintenance because It's stressful to have to do it under time pressure. Updating frequently reveals blockers and incompatibilities early as well as prepares the build cache.

#### From Hackage

Updating package dependencies from Hackage should work like normal in a Haskell project. It’s important to note that we pin the `index-state` of the Hackage package index in `cabal.project`. This ensures that Cabal always sees Hackage 'as if' it were at that specific time, providing reproducibility. However, if you need a package version released *after* that time, you must update the `index-state` and run `cabal update` locally.

Since we use Nix to manage our Haskell build, you will also need to update the Nix equivalent of the newer `index-state`. You can do this by running `nix flake lock --update-input haskellNix/hackage`.

#### From Cardano Haskell packages (CHaP)

Many Cardano packages are not available on Hackage but are instead hosted in the [Cardano Haskell packages (CHaP)](https://github.com/input-output-hk/cardano-haskell-packages). For more information, refer to the README there. Adding new packages from CHaP is similar to adding them from Hackage. However, there are a few differences: CHaP has its own independent `index-state`, and you will need to update the `CHaP` flake input using `nix flake lock --update-input CHaP`.

#### Using unreleased versions of dependencies

Sometimes, we need to use an unreleased version of a dependency, either to address an issue in a package not under our control or to experiment with a pre-release version of one of our own packages. You can use a `source-repository-package` stanza to include the unreleased version. **Only** use this approach temporarily and aim to have your changes released upstream. If that is not possible, we can also release a patched version into CHaP. For instructions, refer to their [README](https://github.com/IntersectMBO/cardano-haskell-packages).

### Versioning and changelog

During development:
- Ensure `CHANGELOG.md` is updated with a high-level, technical, yet user-focused list of changes, following the [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) guidelines.
- Bump the `UNRELEASED` version in `CHANGELOG.md` according to [Semantic Versioning](https://semver.org/).
- Use the `unstable` version of Docker images for demos:
  - Update the Docker image tag using: `sed -i.bak -e "s,\(ghcr.io/cardano-scaling/hydra-[^:]*\):[^[:space:]]*,\1:unstable," demo/*`.
- Ensure all `hydra-` packages are versioned identically and that their versions are aligned with the latest release.
- Version other packages independently from `hydra-` packages, and maintain a separate changelog for them.

### Releasing

To perform a release of the next `<version>`:

1. Make sure all tests are passing.
2. Publish Hydra scripts onto `preview`, `preprod`, and `mainnet` using the
   [smoke test][smoke-test] and put the transaction IDs as new `<version>`
   entries into [networks.json](./hydra-node/networks.json).
3. Update CHANGELOG.md by replacing `UNRELEASED` with a date in
   [ISO8601](https://en.wikipedia.org/wiki/ISO_8601) and prepare contents.
4. Run `./release.sh <version>`.
5. Check if all the bumped versions are correct. In particular, whether the demo
   and tutorial would still work given the changelog. If everything is fine,
   push the branches `master`, `release` and the `<version>` tag.
6. Create a GitHub release page containing:
   * The released changes (formatted) and giving credit where credit is due
   * Built Hydra (and `cardano-node`) binaries to the release using the naming scheme:
     `hydra-<platform>-<version>.zip` where `platform` is `x86_64-linux` or
     `aarch64-darwin` (the same for `cardano-node` instead of `hydra`)
   * The just published `hydra-scripts-tx-id` from step 1.

[smoke-test]: https://github.com/cardano-scaling/hydra/actions/workflows/smoke-test.yaml

## Communication

We have several reporting and communication practices within the Hydra project.
Becoming one of the core contributors to the project includes participation in
core communication processes.

### Bi-weekly update

At the end of every two weeks, we provide updates to
[cardano-updates](https://IntersectMBO.github.io/cardano-updates/tags/hydra),
which also serve as a basis for our monthly reports.

To write such an update:

1. Use the contributors' tactical meeting agenda to collect bullet points on:
   - **What did the team achieve this sprint?**: past tense summary of done things
     (good: 'Implemented..', bad: 'Started working on..'); use the backlog and
     calendars to collect updates.

   - **What are the goals of the next sprint?**: a short look-out onto the backlog and/or
     roadmap; ask each contributor what they would like to get done next week; always start from scratch and don't carry over things from last week.

2. Enrich the content with useful links and write a high-level summary. This
   should use a passive or 'they'-style of writing. Tip: check out older updates
   for examples and/or use an AI language model to write it.

3. Create an entry blog and raise a pull request on
   [cardano-updates](https://github.com/IntersectMBO/cardano-updates),
   putting other Hydra contributors as reviewers.

   - Don't forget to add yourself as an author in the `authors.yml`.

### Monthly review and report

To keep our users and stakeholders up-to-date regularly, we organize a review meeting and publish a report every month on [the project website](https://cardano-scaling.github.io/website/monthly).

To conduct this meeting and write this report:

1. Monthly meeting slides and report preparation:
    - Ensure the Google Meet invitation exists for this month and check stakeholder attendance

    - Copy monthly slides, change the title and template accordingly to `<Month> <Year>`
    - Remove old 'content' slides and mark the regular slides to be updated

    - Decide what to demonstrate (this is not optional!), either from the core
      development or an invited demo/showcase from the community

    - Update delivered/current/next features
    - Expand some of those into interesting 'Development' topics
    - Summarize what happened in the Hydra 'Community'
      + Reach out to individuals from the community whether they want to contribute something to the monthly report

    - Update the roadmap for the current month:
      + Use the grooming/planning session before the latest monthly report to get an updated [roadmap](https://github.com/orgs/input-output-hk/projects/21)
      + Store the latest roadmap screenshot in slides and on [Miro](https://miro.com/app/board/uXjVMA4bSao=/), with and without 💭 **idea** items
      + Check milestones and versions are consistent
      + Mark up and summarize notable changes in the roadmap from the last month to support slides
    - Update the repository status in slides using <https://github.com/cardano-scaling/hydra/pulse/monthly>

    - Prepare an invitation by creating an event in Discord, and update the invitation used last month with it
    - Send an invitation to Hydra and Mithril #announcements channels on our Discord server

2. Conduct the meeting:
    - Do not forget to record the meeting!
    - Ask for sharing rights after the meeting by requesting sharing in Google Drive with yourself and/or asking for making it publicly available

3. Write the report in [this repository](https://github.com/cardano-scaling/website):
    - Copy the previous month's report, then update the dates, links to slides, and closed issues
    - Use the slides as a starting point to provide a concise written account of what happened
      + Previous reports should serve as a guideline
      + Distribute the write-up among relevant contributors
    - Cross-link the slides, recording, and report
    - Submit a pull request, review it, and merge it to publish the report

NOTE: Drafting the report (step 3) is also a good preparation for presenting the individual sections.
