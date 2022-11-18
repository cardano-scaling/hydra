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

Origin of this adr is the HydraHeadV1 formal specification and addressing the gaps between this spec and our implementation.

When working on specification we found a _gap_ where we don't use the provided formula in the deadline check for `Close` tx:

```
 // Deadline

T_max <= T_min + L // Bounded confirmation window
DL’ = T_max + L    // The latest possible deadline is 2*L

```

(_here T_max/T_min are the tx validity bounds_)

This is a possible attack vector where one could construct a tx with specified upper validity bound too far in the
future. This would affect our `Close` transaction since we add the contestation period on top of the tx upper bound in the
on-chain code and this can cause the `Close` tx to hang for possibly very long period of time.

This is the relevant ([PR](https://github.com/input-output-hk/hydra/pull/615)) that started as an attempt to fix this specification _gap_.

Incorporating spec formula was easy on the validator side of things and to detect if tx has upper validity too far in the future we used
the contestation period. So if the `upper bound - lower bound` is `<=` to the contestation period we say that validator succeeds. So contestation
period plays a valuable role here and is not being used for a single purpose anymore.

Seems like a valid change to do but this is when we ran into problems when trying to make all of the tests pass.

#### Here is some context:

- What we currently do in `fromPostChainTx` when posting a `close` tx is use `closeGraceTime` (which is hardcoded to 100 slots)
  to construct the upper bound validity of a tx.

- In hydra-cluster test suite we use hardcoded `cperiod` as a fixed contestation period when posting the `Init` tx and this value
  is what gets set as the contestation period on-chain.

- On top of these hardcoded values there is also chain slots to consider since different chains have different slot lenghts so preview
  network is not the same as devnet for example.

In the presence of new changes we added to #615 pr when two values are not _in sync_ we start to see test failures like

```
   OutsideValidityIntervalUTxO (ValidityInterval {invalidBefore = SJust (SlotNo 275), invalidHereafter = SJust (SlotNo 295)}) (SlotNo 196)

```

So it seems like the chain is moving slower than what we expect when setting the tx validity bounds.
Chain is at slot 196 while the tx validity is between 275 and 295.

#### So how to go about fixing this problem?

One clue is even in the `closeGraceTime` todo which says `-- TODO: make it a node configuration parameter` and this is the initial idea we had.

If we make the contestation deadline a flag when running the hydra-node we could use it when constructing transactions.

Important thing is to rememeber when setting this parameter is to consider the network we are currently running on because of the slot lengths.

Perhaps we could detect wrongly configured values for this flag at runtime and warn the users/stop the node from starting?

Another addition is - why don't we use the flag to determine what is the correct difference between lower/upper tx bounds?

With these two flags in place we would be able to construct correct tx validity bounds and we would use the contestation period flag only to
set the valid contestation period not to prevent txs having upper validity too far into the future.

## Decision

## Consequences
