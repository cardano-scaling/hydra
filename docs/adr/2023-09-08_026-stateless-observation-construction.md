---
slug: 26
title: |
  26. Stateless transaction observaion & construction
authors: []
tags: [Accepted]
---

## Status

Accepted

## Context

- [ADR 18](/adr/18) merged both `headState` and `chainState` into one single
  state in the Hydra node, giving the chain layer a way to _fetch_ and update
  the `chainState` when observing a chain event.

- [ADR 23](/adr/23) outlined the need for a local chain state in the chain layer
  again to correctly handle correct observation of multiple relevant
  transactions and the resulting `chainState` updates.

- The `ChainStateType tx` for our "actual" Cardano chain layer is currently:

  ```haskell
  data ChainStateAt = ChainStateAt
    { chainState :: ChainState
    , recordedAt :: Maybe ChainPoint
    }

  data ChainState
    = Idle
    | Initial InitialState
    | Open OpenState
    | Closed ClosedState
  ```

  where `InitialState`, `OpenState` and `ClosedState` hold elaborate information
  about the currently tracked Hydra head.

- We face [difficulties](https://github.com/input-output-hk/hydra/issues/529) to
  provide sufficient user feedback when an `initTx` was observed but (for
  example) keys do not match our expectation.

  - Core problem is, that `observeInit` is required to take a decision whether
    it wants to "adopt" the Head by returning an `InitialState` or not.
  - This makes it impossible to provide user feedback through the `HeadLogic`
    and `API` layers.

- We want to build a [Hydra head
  explorer](https://github.com/input-output-hk/hydra/issues/696), which should
  be able to keep track and discover Hydra heads and their state changes even
  when the heads were initialized before starting the explorer.

## Decision

- We supersede [ADR 18](/adr/18) with the current ADR.

### Changes internal to Direct chain layer

- Introduce a `ResolvedTx` type that has its inputs resolved. Where a normal
  `Tx` will only contain `TxIn` information of its inputs, a `ResolvedTx` also
  includes the `TxOut` for each input.

- Change `ChainSyncHandler` signature to `onRollForward :: BlockHeader -> [ResolvedTx] -> m ()`

- Change observing function signature to `observeSomeTx :: ChainContext ->
  ResolvedTx -> Maybe (OnChainTx Tx)`. Notably there is no `ChainState`
  involved.

- Do not guard observation by `HeadId` in the chain layer and instead do it in the `HeadLogic` layer.

- Define a `SpendableUTxO` type that is a `UTxO` with potentially needed datums included.

  - TBD: instead we could decide to use inline datums and rely on `UTxO` containing them

- Change transaction creation functions `initialize`, `commit`, `abort`,
  `collect`, `close`, `contest` and `fanout` in `Hydra.Direct.Chain.State` to
  take `SpendableUTxO` and `HeadId`/`HeadParameters` as needed.

- Extend `IsChainState` type class to enforce that it can be updated by
  concurrent transactions `update :: ChainStateType tx -> [tx] -> ChainStateType tx`.

  - While this is not strictly needed "outside" of the chain layer, it will have
    us not fall into the same pit again.

- Change `ChainStateAt` to only hold a `spendableUTxO` and the `recordedAt`.

- Update the `LocalChainState` in `onRollForward` by using `update` and pushing
  a new `ChainStateAt` generically.

TBD:

- Impact on generators

### Chain interface changes

- Add `HeadId` and `HeadParameters` to `PostChainTx`.
- Add `HeadId` to all `OnChainTx` constructors.
- Extend `OnInitTx` with observed chain participants.

  - TBD: How are _cardano_ verification keys generically represented in `HeadLogic`?

- Extend `OnContestTx` with new deadline and a list of contesters.
- Move off-chain checks for what makes a "proper head" to `HeadLogic`

TBD:

- Merge `HeadSeed` and `HeadId`? How to abstract?

## Consequences

- All logic is kept in the logic layer and no protocol decisions (i.e. whether
  to adopt or ignore a head initialization) are taken in the chain layer.

  - The `HeadLogic` gets informed of any proper `initTx` and can log that it is
    ignored and for what reason.

- The transaction observation and construction functions can be moved into a
  dedicated package that is cardano-specific, but not requires special state
  knowledge of the "direct chain following" and can be re-used as a library.

- All transaction observation functions used by `observeSomeTx` will need to be
  able to identify a Hydra Head transaction from only the `ResolvedTx` and the
  `ChainContext`

- Any `Chain Tx` implementation wanting to re-use existing transaction
  observation functions must be able to resolve transaction inputs (against some
  ledger state) and produce `ResolvedTx`.

  - A chain-following implementation (as `Hydra.Chain.Direct`) can keep previous
    transactions around.
  - A chain indexer on "interesting" protocol addresses can be used to
    efficiently query most inputs.

- We can get rid of the `Hydra.Chain.Direct.State` glue code altogether.

- While this does not directly supersede [ADR23](/adr/23), it paves the way to
  remove `LocalChainState` again as the `ChainStateAt` is now combinable from
  multiple transactions (see `update` above) and we can keep the state (again)
  only in the `HeadState` aggregate. Note that this would shift the rollback
  handling back into the logic layer.
