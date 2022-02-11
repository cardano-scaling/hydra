# Changelog

All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

> **IMPORTANT** 
>
> This package is released independently of other `hydra-` packages
using `plutus-cbor-x.y.z` tags.

## [1.0.0] - 2022-02-11

### Added

- Initial release defining an opaque `Encoding` type as a "naive" bytestring builder that works on-chain. The builder however requires an execution cost linear in the size of the encoded data (both memory and CPU).

- Provides support for _most_ CBOR major types; all except floating number since there's no floating number in Plutus and, it is probably a bad idea anyway to offer serialization support for floating numbers in financial contracts. 
