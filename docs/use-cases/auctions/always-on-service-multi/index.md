---
sidebar_label: 'Auctions-as-a-service: multi'
sidebar_position: 5
---

# Always-on delegated auction service (multi-head)

The always-on delegated auction service is a persistent service for DApps, providing auctions-as-a-service across multiple Hydra heads to facilitate delegated auctions.

:::note Application design

This proposal extends the [single-head service](../always-on-service-single/index.md).
Independent heads do not automatically share bids, liquidity, or an auction's
winning state. Hydra does not provide cross-head routing or atomic settlement.

:::

## Design requirements

- Assign each auction and its assets to a head, or define an explicit protocol
  for reconciling bids across heads.
- Fund each head independently and retain its signed snapshots.
- Specify how a bidder changes heads without duplicating a bid or spending
  the same funds twice.
- Define deadlines and L1 recovery when one head stops progressing.
- Explain the operators' trust assumptions and how the application chooses
  and enforces a single auction result.

Using more heads can distribute load and operator responsibility. It does not
by itself let bidders safely continue the same auction elsewhere after a
failure; that behavior requires application-level coordination.
