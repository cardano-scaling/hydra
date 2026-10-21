---
sidebar_label: SDK for delegated voucher auctions
sidebar_position: 3
---

# SDK for delegated voucher auctions

This proposal describes an SDK for applications using delegated voucher
auctions. It is not a claim that a release-compatible SDK is available.
Consult [hydra-auction](https://github.com/mlabs-haskell/hydra-auction) for the
reference project's current status and supported Hydra versions.

## Features and scope

An integration needs:

- Auction contracts and reference scripts with documented spending rules.
- Discovery of auctions, bidders, sellers, delegates, and their heads.
- Event subscriptions that distinguish submission, approval, and settlement.
- A wallet interface for deposits, bids, and withdrawals.
- Recovery paths for expired auctions, unavailable delegates, and head closure.

Contract reuse, indexing, and a browser interface are application work. The
Hydra API already supports script deposits and decommits; the SDK must handle
their current request formats, activation delays, and finalization events.

## Limitations

A design may use one head per auction or a persistent head hosting several
auctions. Neither arrangement prevents operators from censoring bids or
stopping progress. Signed snapshots record operator agreement; the auction
contracts and recovery rules must define what bidders can enforce on L1.
