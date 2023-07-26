---
sidebar_label: Auctions-as-a-service Multi
sidebar_position: 5
---

# Always-On Delegated Auction Service (Multi-Head)

> A persistent service for DApps, offering auctinos as-a-service over a a choice of multiple Hydra heads to host delegated auctions.

## Overview

The always-on single-head service is suitable for a wide range of dApps, but for some use cases, we want to be able to increase the decentralization of the L2 host beyond the group of delegates in a single Hydra Head. We can do this by adapting Hydra-enabled dApps to be able to use more than one Hydra Head to host their L2 processes. This dilutes the power of any one Head’s delegates over the L2 processes. It also makes the L2 bidding process more robust to L2 downtime – if one Hydra Head goes down, bidders can continue bidding on L2 via the other Hydra Heads.

## Features and Scope

To be determined.

## Remaining limitations

1. Delegates can censor bidders from submitting bids to the auction, but if bidders are censored by the delegates of one Hydra Head then they can go to another Hydra Head to submit their bids for the auction.
2. Delegates are responsible on L2 for ensuring that the standing bid of the auction can only be replaced by a new bid that exceeds it by the minimum increment defined in the auction terms. However, bidders receive a multi-signed proof for every confirmed bid, which can be used as incontrovertible proof against delegates if there’s any foul play

## Prerequisites from Hydra technical roadmap

To be determined.
