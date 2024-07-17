---
sidebar_label: Auctions-as-a-service multi
sidebar_position: 5
---

# Always-on delegated auction service (multi-head)

> A persistent service for DApps, offering auctions-as-a-service over a choice of multiple Hydra heads to host delegated auctions.

## Overview

The always-on single-head service is suitable for a wide range of DApps, but some use cases demand an increased level of decentralization beyond the group of delegates in a single Hydra head. We can achieve this by enabling DApps to utilize more than one Hydra head to host their layer 2 processes. This approach dilutes the power of any one head’s delegates over the layer 2 processes. It also makes the layer 2 bidding process more robust to layer 2 downtime—if one Hydra head goes down, bidders can continue bidding on layer 2 via the other Hydra heads.

## Features and scope

To be determined.

## Remaining limitations

1. Delegates can censor bidders from submitting bids to the auction. However, if bidders are censored by the delegates of one Hydra head, they can go to another Hydra head to submit their bids.
2. Delegates on layer 2 are responsible for ensuring that the standing bid of the auction can only be replaced by a new bid that exceeds it by the minimum increment defined in the auction terms. However, bidders receive a multi-signed proof for every confirmed bid, which can be used as incontrovertible proof against delegates if there’s any foul play.

## Prerequisites from Hydra technical roadmap

To be determined.
