---
sidebar_label: Auctions-as-a-service: multi
sidebar_position: 5
---

# Always-on delegated auction service (multi-head)

The always-on delegated auction service is a persistent service for DApps, providing auctions-as-a-service across multiple Hydra heads to facilitate delegated auctions.

## Overview

The always-on single-head service is suitable for a wide range of DApps, but some use cases demand an increased level of decentralization beyond the group of delegates in a single Hydra head. By allowing DApps to use multiple Hydra heads for their layer 2 processes, we can distribute the influence of any one head’s delegates. This approach also enhances the robustness of the layer 2 bidding process — if one Hydra head goes down, bidders can continue on the other Hydra heads.

## Features and scope

To be determined.

## Remaining limitations

1. Delegates can censor bidders from submitting bids to the auction. However, if bidders are censored by the delegates of one Hydra head, they can participate in another Hydra head to submit their bids.
2. Delegates on layer 2 are responsible for ensuring that the standing bid of the auction can only be replaced by a new bid that exceeds it by the minimum increment defined in the auction terms. However, bidders receive a multi-signed proof for every confirmed bid, which can be used as incontrovertible proof against delegates if there’s any foul play.

## Prerequisites from Hydra technical roadmap

To be determined.
