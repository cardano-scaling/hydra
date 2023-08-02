---
sidebar_label: Auctions-as-a-service Single
sidebar_position: 4
---

# Always-On Delegated Auction Service (Single-Head)

> A persistent service for DApps which runs multipls auctions on L2, using a single Hydra head. Enables an as-a-service business model.

## Overview

Looking beyond a single application, we want to seed the growth of a whole ecosystem of Hydra-enabled dApps on Cardano. To do so, we need to establish a viable business model for delegates that provide Hydra Heads to host dApp processes as a service. The delegated voucher auction can be adapted to this model by switching its previous single-use Hydra head into a persistent Hydra head that can host multiple auctions without closing.

## Features and Scope

To be determined.

## Remaining limitations

1. Delegates can censor bidders from submitting bids to the auction.
2. Delegates are responsible on L2 for ensuring that the standing bid of the auction can only be replaced by a new bid that exceeds it by the minimum increment defined in the auction terms. However, bidders receive a multi-signed proof for every confirmed bid, which can be used as incontrovertible proof against delegates if there’s any foul play.

## Prerequisites from Hydra technical roadmap

To be determined.
