# 13. Plutus Contracts Testing Strategy

Date: 2022-01-19

## Status

:hammer_and_wrench:

## Context

* We are implementing our custom ([Direct](./0010-use-direct-chain.md)) interaction w/ Cardano blockchain and not using the PAB nor the `Contract` monad to define off-chain contract code
* This implies we cannot use the [official](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/Test.hs) testing framework for Contracts which relies on `Contract` monad and emulator traces nor the [QuickCheck based framework](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-testing.html)
* We want to follow our [Test-Driven Development](./0012-testing-strategy.md) approach for contracts as this is a critical part of Hydra
* On-Chain Validators need not only to be correct and functional, but also secure and hardened against malicious parties

## Decision

_Therefore_

* We test-drive single contracts code using _Mutation-Based Property Testing_
* Contracts are tested through the construction of actual _transactions_ and running phase-2 ledger validation process
* We start from a "healthy" transaction, that's expected to be correct and stay so
* Contract code is initially `const True` function that validates any transaction
* We flesh the contract's code piecemeal through the introduction of _Mutations_ that turn a healthy transaction into an expectedly invalid one
* We gradually build a set of combinators and generators that make it easier to mutate arbitrarily transactions, and combine those mutations

## Consequences

* We make the contracts' _Threat model_  explicit through the tests we write, which should help future auditors' work
* We'll need an additional layer of tests to exercise the Hydra OCV State Machine through _sequence of transactions_. This could be implemented using [quickcheck-dynamic](https://github.com/input-output-hk/plutus-apps/tree/main/quickcheck-dynamic) library, or other tools that are currently being developed by the Cardano community
