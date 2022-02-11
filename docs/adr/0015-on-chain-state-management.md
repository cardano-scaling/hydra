# 15. On-Chain State Management

Date: 2022-02-04

## Status

:hammer_and_wrench:

## Context

The Hydra head protocol requires a few on-chain interactions. Those are governed by on-chain smart validator scripts which ensures the correct execution of the Hydra head state-machine. Each transition of the state machine takes the form of a transaction on-chain. As part of the Hydra nodes, we have to observe the chain and react to transitions accordingly, while maintaining a state across all distributed Hydra nodes.

Our current approach involves maintaining a so-called 'OnChainHeadState' sum-type containing aggregated information and capturing the state-machine's state. Beside, we identify transactions to be observed using traits of those transactions and extract for each informations relevant to the next transitions. 

We've been bothered with maintaining an 'OnChainHeadState' ever since we went for this approach. Indeed, it is a partially redundant piece of information which overlaps with the core of the Hydra node which is managing the application logic (and also maintains a folded state). Plus, with the introduction of head identification and multi-head support, we are exploring ways to simplify and make less cumbersome our on-chain state management.

## Decision

A first observation we made is that we can actually observe all transactions involved in the head life-cycle by solely looking at those transactions and some credentials known of the node (i.e. a verification key). From each transaction, we want to be able to derive _some identifier_ that uniquely identifies a head in order to tell head apart. Before anything else, let's recap how we currently observe each transaction on-chain. 

### Head Life-cycle Summary

To ease notation, here below is a table which summarizes the name of each transition and the states they transition from and to. 

| Name       | From    | To      |
| ---        | ---     | ---     |
| Init       | -       | Initial |
| Commit     | Initial | Initial |
| Abort      | Initial | -       |
| CollectCom | Initial | Open    |
| Close      | Open    | Closed  |
| Fanout     | Closed  | -       |

We assume that all nodes are configured with:

- A Cardano verification key `vk_cardano` 
- An ephemeral verification key `vk_ephemeral`
- A `νHead`, `νInitial`, `νCommit` scripts (for which one can compute static hashes)

#### Observing an 'Init' transaction

A transaction qualifies as an `Init` if and only if:

- It produces exactly one output to an address locked by the `νHead` script, with a datum `δHead` pointing at an `Initial` state. In particular, `δHead` contains a list of `n` verification key hashes identifying participating parties.

- Our verification key `vk_ephemeral` is a member of the list of ephemeral keys stored in `δHead`.

- For each key hash in `δHead`, it mints a participation token with a unique policy id and such that its asset name is an injective function of the corresponding key hash it's minted for. 

- It produces exactly `n` outputs to addresses locked by `νInitial` scripts where; each output has a datum `δInitial` representing one of the `vk_cardano` of a participant and carries one of the minted participation tokens. 

> Note that the `Init` transaction is the most complicated transition to observe for there's no previous state. For other transitions such as `CollectCom` or `Abort`, one can get away with solely observing that the head script `νHead` is executing the expected transition; the script validator is already checking any other details. 

From observing an `Init` transaction, observers may infer the following pieces of information:

- The head parameters, such as the contestation period;
- Ephemeral verification keys of other parties;
- A UTXO entry locked by the `νHead` script;
- Some UTXO entries locked by `νInitial` scripts.

#### Observing a 'Commit' transaction

A transaction qualifies as a `Commit` if and only if

- There's exactly one input spending funds locked by a script `νInitial` with a `Commit` redeemer.

From observing a `Commit` transaction, observers may infer the following pieces of information:

- A UTXO entry locked by a `νCommit` script;
- A now-spent-UTXO entries which was locked by a `νInitial` script.

#### Observing an 'Abort' transaction

A transaction qualifies as an `Abort` if and only if:

- There's exactly one input spending funds locked by a script `νHead` in an `Initial` state, using an `Abort` redeemer. 

From observing an `Abort` transaction, observers may infer the following pieces of information:

- A now-spent-UTXO entry which were locked by `νHead` script;
- Some now-spent-UTXO entries which were locked by `νInitial` scripts;
- Some now-spent-UTXO entries which were locked by `νCommit` scripts.

#### Observing a 'CollectCom' transaction

A transaction qualifies as a `CollectCom` if and only if:

- There's exactly one input spending funds locked by a script `νHead` in an `Initial` state, using a `CollectCom` redeemer. 

From observing a `CollectCom` transaction, observers may infer the following pieces of information:

- A new UTXO entry locked by a `νHead` script;
- Some now-spent-UTXO entries which were locked by `νCommit` scripts;
- An initial UTXO set (and a corresponding root hash for that UTXO set).

#### Observing a 'Close' transaction

A transaction qualifies as a `Close` if and only if:

- There's exactly one input spending funds locked by a script `νHead` in an `Open` state, using a `Close` redeemer. 

From observing a `Commit` transaction, observers may infer the following pieces of information:

- A new UTXO entry locked by a `νHead` script;
- A candidate final UTXO set
- A candidate snapshot number 

#### Observing a 'Fanout' transaction

A transaction qualifies as a `Fanout` if and only if:

- There's exactly one input spending funds locked by a script `νHead` in a `Closed` state, using a `Fanout` redeemer. 

From observing a `Commit` transaction, observers may infer the following pieces of information:

- A candidate final UTXO set
- A candidate snapshot number 

### Observing Transitions

One interesting detail to notice is how any transition can be observed _by itself_, out of any context. This is actually consequence of Cardano's smart-contract design which forces transactions validations to be deterministic and solely dependent on the surrounding transaction. This means that it is possible to write a pure function `observeTransition` for observing transitions as such:

```hs
observeTransition ::
  Configuration ->
  Tx ->
  Maybe Transition

data Configuration = Configuration
  { knownScripts :: KnownScripts
  , ourCardanoKey :: SigningKey PaymentKey 
  , ourEphemeralKey :: SigningKey EphemeralKey
  }

data KnownScripts = KnownScripts
  { νHead :: Script 
  , νInitial :: Script 
  , νCommit :: Script 
  }

data Transition 
  = Init 
      { headParameters :: HeadParameters
      , ephemeralKeyHashes :: [VerificationKeyHash]
      , initials :: Map TxOutRef (TxOut CtxUTxO)
      }
  | Commit
      { initialConsumed :: TxOutRef
      , commit :: (TxOutRef, TxOut CtxUTxO)
      }
  | TODO...
```

In particular, we can also imagine splitting that function into sub-functions each dedicated to each transition. A corollary of this means that it is possible to represent our current state as a list of transaction `[Tx]` from which we can extract on-demand the required information for the application logic by folding over the sequence of transactions with the `observeTransition` function.

More exactly, in the light of upcoming work stream, we would want to also keep track of the slots at which transactions were found in blocks (i.e. `[(SlotNo, Tx)]`) as this allows for easy rollback later on. 

This approach is very similar to [event-sourcing](https://docs.microsoft.com/en-us/azure/architecture/patterns/event-sourcing) where transactions play the role of events. 

## Consequence

TODO.
