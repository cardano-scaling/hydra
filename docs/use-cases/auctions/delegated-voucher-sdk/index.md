---
sidebar_label: SDK for delegated voucher auctions
sidebar_position: 3
---

# SDK for delegated voucher auctions

This is a modular SDK for application developers supporting delegated voucher auctions.

## Overview

The initial milestones have resulted in a Hydra-based auction implementation that includes the essential features necessary for a viable launch on Cardano. To make this solution commercially usable, we need to:

- Enhance performance and robustness to production-ready levels
- Develop a browser-based frontend UI that provides a user-friendly experience
- Upgrade backend services to support a vibrant ecosystem of sellers, bidders, and delegates engaging with multiple auctions simultaneously.

The next step involves creating an open-source SDK for delegated voucher auctions, designed for easy integration by any DApp requiring auction functionalities. This SDK will be built from modular components that can also be utilized in other Hydra-based applications with similar smart contract architectures.

## Features and scope

For the auction smart contracts, optimizations will include:

- Implementing reference scripts for auction validators and minting policies
- Exploring ways to make validators and minting policies non-parametric, allowing the same reference scripts to be reused across multiple auctions – potentially as a standardized modular component across different DApps.

The reference implementation from the initial milestones is limited to handling only a few auctions and offers a basic system for discovering auctions, bidders, sellers, delegates, and Hydra heads. For a commercial product, enhancements are necessary to create a scalable and dynamic directory and state indexer for the auction ecosystem:

- Enabling sellers, bidders, delegates, and others to discover active auctions, identify relevant participants, and learn how to engage with them
- Providing lightweight subscriptions for specific auction events
- Offering aggregate metrics for current and historical auctions to general users.

A crucial aspect will be developing frontend UI code to ensure an ergonomic and appealing user experience for auctions. Collaborating with existing projects within the Cardano ecosystem, which possess significant expertise in marketplace and auction UX, is highly desirable.

## Remaining limitations

1. A new Hydra head must be opened for every auction
2. Delegates can censor bidders from submitting bids to the auction
3. Delegates are responsible on layer 2 for ensuring that any new bid exceeds the standing bid by the minimum increment specified in the auction terms. However, bidders receive multi-signed proof for every confirmed bid, providing incontrovertible evidence against potential delegate misconduct.

## Prerequisites from Hydra technical roadmap

To be determined.
