---
slug: 21
title: |
  21. Revisit handling time
authors: [Sasha Bogicevic]
tags: [Proposed]
---

## Status

Proposed

## Context

* The HydraHeadV1 formal specification contains now a *bounded confirmation window*:

    ```
    // Deadline

    T_max <= T_min + L // Bounded confirmation window
    DL’ = T_max + L    // The latest possible deadline is 2*L

    ```

    with `T_min` and `T_max` being the tx validity bounds and `L` being the contestation period.

    + This is to avoid attacks with specified upper validity bound being too far in the future and denial of service the head with this (e.g. 10 years).

* The contestation period is to be used to create this bounded confirmation window. Before it was only used for computing the contestation deadline.

* There is a `closeGraceTime` currently hard-coded (`100` slots) to set some upper bound on the `closeTx`. This was also required so far to compute the contestation deadline.

* Different networks (chains) have different slot lenghts, e.g. the preview network has a slot every `1s`, while our local devnets use `0.1s`.

* The `contestationPeriod` can be configured by users via the `Init` client input. For example, the hydra-cluster test suite uses a hardcoded `cperiod` on the client side.

## Decision

* Make the `closeGraceTime` (number of slots) configurable at the `hydra-node`, e.g. via a `--close-grace-time` command line option.
* Also configure the contestation period (number of seconds) on the `hydra-node`, e.g. via a `--contestation-period` command line option.
* The `closeTx` is constructed by
  - upper bound is `closeGraceTime`
  - lower bound is `closeGraceTime` minus the `contestationPeriod` in slots
  
### Alternative

* A) Different strategy to construct the `closeTx` validity range:
  - lower bound is the last known slot as reported by the `cardano-node`
  - upper bound is the current slot + `closeGraceTime` (slots)
  
* B) Different strategy to construct the `closeTx` validity range:
  - lower bound is the last known slot as reported by the `cardano-node`
  - upper bound is the current time + `min(closeGraceTime, contestationPeriod)`
  
  
  -> The time between close transaction and when a fanout transaction can be posted is *at least* `2*contestationPeriod`. Users need to know this.

## Consequences

* Not any positive number of seconds is a valid contestation period any more!
* Need to check consistency of `--close-grace-time` and `--contestation-period` taking the slot length of the connected network into account.
  - That is, the `closeGraceTime` slots converted to seconds must be smaller than `contestationPeriod`.
* Need to check the contestation period of an `InitTx` and error out if it does not match.
  - This is super hard in the current code! The chain layer can only decide to observe or not (`Maybe`) and informing the user / taking action on this is messy.
* Remove the `contestationPeriod` from the `Init` client input. That is, not make it configurable via the API.
