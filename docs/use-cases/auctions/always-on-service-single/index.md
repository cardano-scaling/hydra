---
sidebar_label: Auctions-as-a-service single
sidebar_position: 4
---

# Always-on delegated auction service (single-head)

> A persistent service for DApps that runs multiple auctions on layer 2, using a single Hydra head. This enables an as-a-service business model.

## Overview

Looking beyond a single application, our goal is to foster the development of an ecosystem of Hydra-enabled DApps on Cardano. To achieve this, we need to establish a viable business model for delegates that provide Hydra heads to host DApp processes as a service. The delegated voucher auction can be adapted to this model by transitioning from its previous single-use Hydra head to a persistent Hydra head that can host multiple auctions continuously without closing.

## Features and scope

To be determined.

## Remaining limitations

1. Delegates have the ability to censor bidders from submitting bids to the auction.
2. Delegates on layer 2 are tasked with ensuring that the standing bid of an auction can only be replaced by a new bid that exceeds it by the minimum increment specified in the auction terms. However, bidders receive a multi-signed proof for every confirmed bid, providing incontrovertible evidence against delegates in cases of any foul play.

## Prerequisites from Hydra technical roadmap

To be determined.