---
slug: 21
title: |
  21. Bounded transaction validity on Hydra protocol transactions
authors: [ch1bo, pgrange, ffakenz, abailly, v0d1ch]
tags: [Accepted]
---

## Status

Accepted

## Context

- The HydraHeadV1 formal specification contains a _bounded confirmation window_:

  ```
  // Deadline

  T_max <= T_min + L // Bounded confirmation window
  DL’ = T_max + L    // The latest possible deadline is 2*L

  ```

  with `T_min` and `T_max` being the tx validity bounds and `L` being the
  contestation period.

  - This is to avoid attacks with specified upper validity bound being too far
    in the future and denial of service the head with this (e.g. 10 years).

#### Current state of things:

- The contestation period and upper tx validity is used for computing the
  contestation deadline.

- There is a `closeGraceTime` currently hard-coded (to `100` slots) to set some
  upper bound on the `closeTx`. This was also required so far to compute the
  contestation deadline.

- Different networks (chains) have different slot lenghts, e.g. the preview
  network has a slot every `1s`, while our local devnets use `0.1s`. This means
  hardcoded values like `closeGraceTime` need to be _in sync_ with the
  underlying network.

- The `contestationPeriod` can be configured by users via the `Init` client
  input. For example, the hydra-cluster test suite uses a hardcoded `cperiod` on
  the client side.

- Default value for `T_Min` is negative infinity.

- Lower tx validity being in the future does not pose a problem since other
  participant is able to close a head.

#### What we want to achieve:

- We want to enforce topmost formula in this file in our code on-chain.

- Introduce `maxGraceTime` expressed in seconds in place of `closeGraceTime` and adjust to
  appropriate value.

- The contestation period is to be used to create bounded close transaction
  (together with `maxGraceTime`). Before it was only used for computing the
  contestation deadline.

- [x] If contestation period is higher than `maxGraceTime` we will pick the
      latter. We still need `maxGraceTime` since if `contestationPeriod` is low for
      the current network our txs reach the upper bound fast and become invalid.
      That is why we set the upper tx bound to be minimum between
      `contestationPeriod` and `maxGraceTime` so that txs have high enough upper
      bound.

- Make sure all head participants use the same value for `contestationPeriod`.

- Attack vector has a corresponding mutation test.

## Decision

- Use the specification formula on-chain.

- Configure the contestation period (number of seconds) on the `hydra-node`,
  e.g. via a `--contestation-period` command line option.

- Lower tx bound should be the last known slot as reported by the
  `cardano-node`.

- Upper tx bound is the current time + minimum between `contestationPeriod` and
  `maxGraceTime`.

- When submitting the `InitTx` make sure to use `--contestation-period` value
  from our node's flag.

- If other nodes observe `OnInitTx` and the `contestationPeriod` value does not
  match with their `--contestation-period` setting - ignore `InitTx`.

- Rename `closeGraceTime` to `maxGraceTime` since we are using it also for upper
  bound of a contest tx.

## Consequences

- Not any positive number of seconds is a valid contestation period any more!

- Upper tx validity of close transaction is the minimum between `maxGraceTime`
  and `contestationPeriod` and this needs to be _good enough_ value with respect
  to running network. This is a consequence required by the ledger when
  constructing transactions since we cannot convert arbitrary point in times to
  slots.

- All parties need to aggree on contestation period before trying to run a Head
  protocol otherwise InitTx will be ignored.
