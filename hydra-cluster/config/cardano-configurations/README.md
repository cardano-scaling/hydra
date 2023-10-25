# Cardano Configurations

This repository holds the latest configurations for various core Cardano components (cardano-node, cardano-db-sync...), as well as the genesis configurations of well-known networks (i.e. mainnet and testnet...). It's **updated automatically, when required, by a cron-job once a day**, using the [The Cardano Book](https://book.world.dev.cardano.org/environments.html) as a source, and, since they're a Git repository, can be added to a project as a Git submodule and specific configurations can be pinned via a commit reference. The folder structure is network-centric and works well for setup where the network is fixed via a command-line option or environment variable.

---

<p align="center">
  <a href='https://github.com/input-output-hk/cardano-configurations/actions/workflows/refresh-configurations.yaml'><img src="https://img.shields.io/github/actions/workflow/status/input-output-hk/cardano-configurations/refresh-configurations.yaml?label=CRON%20JOB&style=for-the-badge"/></a>
</p>
