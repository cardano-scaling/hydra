---
slug: 21
title: |
  21. Bounded transaction validity on Hydra protocol transactions
authors: [Sasha Bogicevic]
tags: [Proposed]
---

## Status

Proposed

## Context

* The HydraHeadV1 formal specification contains a *bounded confirmation window*:

    ```
    // Deadline

    T_max <= T_min + L // Bounded confirmation window
    DL’ = T_max + L    // The latest possible deadline is 2*L

    ```

    with `T_min` and `T_max` being the tx validity bounds and `L` being the contestation period.

    + This is to avoid attacks with specified upper validity bound being too far in the future and denial of service the head with this (e.g. 10 years).

#### Current state of things:

* The contestation period and upper tx validity is used for computing the contestation deadline.

* There is a `closeGraceTime` currently hard-coded (to `100` slots) to set some upper bound on the `closeTx`. This was also required so far to compute the contestation deadline.

* Different networks (chains) have different slot lenghts, e.g. the preview network has a slot every `1s`, while our local devnets use `0.1s`. This means hardcoded values
  like `closeGraceTime` need to be _in sync_ with the underlying network.

* The `contestationPeriod` can be configured by users via the `Init` client input. For example, the hydra-cluster test suite uses a hardcoded `cperiod` on the client side.

* Default value for `T_Min` is negative infinity.

* Lower tx validity in the future does not pose a problem since other participant is able to close a head.

#### What we want to achieve:

* We want to enforce topmost formula in this file in our code on-chain.

* The time between close transaction and when a fanout transaction can be posted should be *at most* `2*contestationPeriod`. Users need to know this.

* The contestation period is to be used to create bounded close transaction. Before it was only used for computing the contestation deadline.

* Make sure all head participants use the same value for `contestationPeriod`.

* Attack vector has a corresponding mutation test.

## Decision

* Use the specification formula on-chain.
* Configure the contestation period (number of seconds) on the `hydra-node`, e.g. via a `--contestation-period` command line option.
* Lower tx bound should be the last known slot as reported by the `cardano-node`.
* Upper tx bound is the current slot + `contestationPeriod`.
* When submitting the `InitTx` make sure to use `--contestation-period` value.
* If other nodes observe `OnInitTx` and the `contestationPeriod` value does not match with their `--contestation-period` setting - ignore `InitTx` and issue a warning.
* _Optionally_: Check if `--contestation-period` value makes sense with the current network and issue a warning when starting `hydra-node`.

## Consequences

* Not any positive number of seconds is a valid contestation period any more!
* Need to check consistency of `--contestation-period` taking the slot length of the connected network into account.
* Need to check the contestation period of an `InitTx` and warn the user if it does not match.
  - Ideally we would error out not issue a warning but: This is super hard in the current code!
    The chain layer can only decide to observe or not (`Maybe`) and informing the user / taking action on this is messy.
* Make the `contestationPeriod` in the `Init` client input not configurable via the API.
