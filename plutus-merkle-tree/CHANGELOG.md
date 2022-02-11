# Changelog

All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

> **IMPORTANT**
>
> This package is released independently of other `hydra-` packages using `plutus-merkle-tree-x.y.z` tags.

## [1.0.0] - 2022-02-11

### Added

- Provides an initial implementation of Merkle trees which works in script validator on-chain. In particular, this allows on-chain scripts to verify merkle tree proofs built off-chain on large trees. 

- Membership verification is in log(n) where `n` is the size (i.e. number of leaves) of the tree.

- The current implementation currently always assumes SHA-256 as a hash algorithm.

- While highly inefficient, the current implementation also allows for _constructing_ merkle trees on-chain. Due to current execution limits, this is only possible for small (< 20) trees.
