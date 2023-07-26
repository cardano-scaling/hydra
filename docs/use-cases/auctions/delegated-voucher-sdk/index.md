---
sidebar_label: SDK for Delegated Voucher Auctions
sidebar_position: 3
---

# SDK for Delegated Voucher Auctions

> Modular SDK for App developers supporting delegated voucher auctions

## Overview

The first two milestones result in a hydra-based auction implementation that contains the essential features necessary for a viable launch on Cardano. However, in order for it actually be usable in a commercial product, we will need to:

- Improve performance and robustness to production-ready levels.
- Develop a browser-based frontend UI that provides a pleasant UX.
- Upgrade backend services to support a vibrant ecosystem of sellers, bidders, and delegates creating and interacting with multiple auctions at the same time.

As the next step, we will explore building an open-source SDK for delegated voucher auctions that is ready to be used by any dApp that needs auction features. Furthermore, the SDK should be composed from modular components that can themselves be reused in other hydra-based applications that have similar smart contract architectures.

## Features and Scope

For the auction smart contracts, we will need to implement the following optimizations:

- Use reference scripts for auction validators and minting policies.
- Explore ways to make validators and minting policies non-parametric, so that the same reference scripts can be re-used in multiple auctions – possibly as a standardized modular component across different dApps.

The reference implementation from the first two milestones can handle only a handful of auctions and has a rudimentary system for discovering auctions, bidders, sellers, delegates, and hydra heads. For a commercial product, this would have to be significantly improved with a properly scalable and dynamic directory and state indexer for the auctions ecosystem:

- Sellers, bidders, delegates, and other people can discover which auctions are currently active, who the relevant participants are in those auctions, and how to interact with them.
- Lightweight subscriptions are available for events of specific auctions.
- Aggregate metrics for current and historic auctions are available for general users.

Most crucially, we will need to develop frontend UI code to provide an ergonomic and pleasant UX for auctions. For this purpose, it is highly desirable to partner-up with existing projects in Cardano that have developed significant expertise on marketplace and auction UX.

## Remaining limitations

1. A new Hydra Head must be opened for every auction.
2. Delegates can censor bidders from submitting bids to the auction.
3. Delegates are responsible on L2 for ensuring that the standing bid of the auction can only be replaced by a new bid that exceeds it by the minimum increment defined in the auction terms. However, bidders receive a multi-signed proof for every confirmed bid, which can be used as 4. incontrovertible proof against delegates if there’s any foul play.

## Prerequisites from Hydra technical roadmap

To be determined.
