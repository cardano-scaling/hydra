---
sidebar_position: 9
---

# Commit/Deposit from a dApp

- Developers building decentralized applications (dApps) on the Hydra protocol
would greatly benefit from clear guidance and examples detailing the validator
checks required to ensure that committed/deposited funds are accurately directed to the
intended Hydra Head instance.

- This document aims to outline essential validator checks needed for
user-written validators, providing concrete examples implemented using PlutusTx
and briefly explaining how the commit/deposit transaction looks like.

## Committing to a Hydra Head

- To begin, let us examine the diagram illustrating the on-chain commit process in detail:
![](./commit-process.jpg)

<sub> Here rectangulars represent transactions and you can see the UTxO (with rounded corners) depicting transaction inputs and outputs together with their contents (datums, redeemers and assets)</sub>

The commit transaction utilizes an initial output specifically crafted for each Hydra Head participant. This initial output includes the `HeadId` in its datum, while the redeemer contains details about the specific `TxOutRef` values to be committed, which are pre-sorted.

The initial validator is parameterized by the commit validator hash. In the user-defined validator, it is sufficient to verify that:

- There is only one output at commit script address containing the PT (participation) token + the change output.

dApp developers can leverage the script redeemer to convey the necessary information.

Let us now proceed with constructing the validator from the user’s perspective!


## Depositing to a Hydra Head

- The process that serves the purpose of bringing in more funds to L2 Hydra network once the Head is already open is referred to as **depositing**.

- This is the diagram explaining the on-chain deposit process:

![](./deposit-process.jpg)

The deposit transaction takes a provided UTxO and _locks_ it into a deposit address where the datum contains serialized Plutus `TxOutRef`'s.
Deposit transaction has its upper validity slot set so the whole deposit process has a deadline. After hydra-node observes the deposit transaction
it will post a `increment` transaction which will actually bring the funds locked at the deposit address to L2 after deposit validator check that
makes sure the Head currency symbol is the same in both the datum and redeemer and that the deposit deadline slot is not surpassed.

Unclaimed deposits can be recovered and this works in any Hydra Head state (even after closing the Head). The only validator check that needs to
be satisfied is that the current slot is after the deposit deadline slot.

:::info
Please make sure to use [_correct_](./../configuration#deposit-period) `--deposit-period` value.
:::

### Building a secure validator

To develop decentralized applications (dApps) on the Hydra protocol, users must commit/deposit their scripts to a Hydra Head.

This enables custom programmability within the Head protocol, unlocking the potential for a wide range of dApp implementations.

As an initial step, we will focus on constructing a validator along with its essential checks.

To begin, we will examine a basic exampleValidator that currently performs no operations:

```Haskell
exampleValidator ::
  () ->
  () ->
  ScriptContext ->
  Bool
exampleValidator _ _ _ = True

exampleSecureValidatorScript :: PlutusScript
exampleSecureValidatorScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||wrap exampleValidator||]
        )
 where
  wrap = wrapValidator @() @()
```
:::warning
Code examples here are just explanatory and are not suitable for production use!
They serve the purpose of giving dApp developers a general idea on how to check that commit or deposit goes to the right Head instance.
:::

:::info
If you need to get the information on Hydra scripts hashes you can use hydra-node:

```
./hydra-node -- --hydra-script-catalogue
{
 "commitScriptHash": "61458bc2f297fff3cc5df6ac7ab57cefd87763b0b7bd722146a1035c",
 "commitScriptSize": 685,
 "depositScriptHash": "ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c",
 "depositScriptSize": 1102,
 "headScriptHash": "a1442faf26d4ec409e2f62a685c1d4893f8d6bcbaf7bcb59d6fa1340",
 "headScriptSize": 14599,
 "initialScriptHash": "c8a101a5c8ac4816b0dceb59ce31fc2258e387de828f02961d2f2045",
 "initialScriptSize": 2652,
 "mintingScriptHash": "fd173b993e12103cd734ca6710d364e17120a5eb37a224c64ab2b188",
 "mintingScriptSize": 5284
}
```
:::

To get the information on `HeadId` easiest is to look at the persistence
folder of your hydra-node and in the `state` file you should be able to find
the headId. (NOTE: You need to initialize the Head first)

Now we are able to start working on our validator checks.


### Commit validator checks

To ensure correct dApp validator checks we must verify that an output exists containing a single PT token with the correct policy ID (head ID).
This condition implicitly confirms that the initial validator has executed, as its output has been consumed,
and the commit transaction now includes an output with the PT token bearing the expected policy ID.

```Haskell
  checkCorrectHeadId =
    let outputValue = foldMap txOutValue (txInfoOutputs (scriptContextTxInfo ctx))
        pts = findParticipationToken expectedHeadId outputValue
     in L.length pts == 1

  findParticipationToken :: CurrencySymbol -> Value -> [TokenName]
  findParticipationToken headCurrency (Value val) =
    case AssocMap.toList <$> AssocMap.lookup headCurrency val of
      Just tokens ->
        mapMaybe (\(tokenName, n) -> if n == 1 then Just tokenName else Nothing) tokens
      _ ->
        []
  {-# INLINEABLE findParticipationTokens #-}

  R{expectedHeadId} = redeemer

```
This check is all we need in order to add some security to our validators and make sure our commit will end up in the correct Head instance.


### Deposit validator checks

To ensure correct dApp validator checks we must make sure the deposit datum contains the correct Head id. Since hydra-node is the one building a deposit
transaction from the user provided UTxO we can feel safe the datum is correct if we are using official release of `hydra-node` binary.
This means that it would be enough to check that there is a output present at the **correct** deposit script address.

We mentioned before how to get the correct script hashes from your hydra-node (`./hydra-node -- --hydra-script-catalogue`). With this information
it is sufficient to make sure the deposit script has the expected hash.


```Haskell

  depositOutput = findDepositOutput expectedDepositValidator

  findDepositOutput :: ScriptHash -> List.List TxOut
  findDepositOutput depositScriptHash =
     let allOutputs = List.fromSOP $ txInfoOutputs info
       in List.filter (isScriptAddress depositScriptHash . txOutAddress) allOutputs

  isScriptAddress :: ScriptHash -> Address -> Bool
  isScriptAddress expectedHash addr =
    case addressCredential addr of
      ScriptCredential vh -> vh == expectedHash
      PubKeyCredential _ -> False
```

Following step is to build a _blueprint_ transaction as a recipe for committing/depositing your script UTxO into a Head. This is only needed in case
you want to introduce some transaction context (like validity range, extra signers etc). We already have some guides on that
[here](./commit-script-utxo#step-5-prepare-the-blueprint) and [here](./incremental-commit) but you would of course need to make some changes
which is left as an exercise to the reader.

<details>
  <summary>Complete commit validator example </summary>

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Example where

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import Hydra.Plutus.Extras (wrapValidator)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  ScriptContext (..),
  ScriptInfo (..),
  TokenName,
  Value (..),
  serialiseCompiledCode,
  txInfoOutputs,
  txOutValue,
  unsafeFromBuiltinData,
 )
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Eq ((==))
import PlutusTx.Foldable (foldMap)
import PlutusTx.Functor ((<$>))
import PlutusTx.List qualified as L
import PlutusTx.Prelude (check, traceIfFalse)

newtype R = R
  { expectedHeadId :: CurrencySymbol
  }
  deriving stock (Show, Generic)

unstableMakeIsData ''R

exampleCommitValidator ::
  () ->
  R ->
  ScriptContext ->
  Bool
exampleCommitValidator _ redeemer ctx =
  checkCorrectHeadId
 where
  checkCorrectHeadId =
    let outputValue = foldMap txOutValue (txInfoOutputs (scriptContextTxInfo ctx))
        pts = findParticipationToken expectedHeadId outputValue
     in traceIfFalse "HeadId is not correct" (L.length pts == 1)

  findParticipationToken :: CurrencySymbol -> Value -> [(TokenName, Integer)]
  findParticipationToken headCurrency (Value val) =
    case AssocMap.toList <$> AssocMap.lookup headCurrency val of
      Just tokens ->
        L.filter (\(_, n) -> n == 1) tokens
      _ ->
        []
  {-# INLINEABLE findParticipationToken #-}

  R{expectedHeadId} = redeemer

exampleSecureValidatorScript :: PlutusScript
exampleSecureValidatorScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||wrap exampleCommitValidator||]
        )
 where
  wrap = wrapValidator @() @R
```

</details>

<details>
  <summary>Complete deposit validator example </summary>

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Example where

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import Hydra.Plutus.Extras (wrapValidator)
import PlutusLedgerApi.V3 (
  Address (addressCredential),
  Credential (..),
  ScriptContext (..),
  ScriptInfo (..),
  TxOut,
  serialiseCompiledCode,
  txInfoOutputs,
  txOutAddress,
  unsafeFromBuiltinData,
 )
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.Eq ((==))
+import PlutusTx.Data.List qualified as List
import PlutusTx.Prelude (check, traceIfFalse)

newtype R = R
  { expectedDepositValidator :: ScriptHash
  }
  deriving stock (Show, Generic)

unstableMakeIsData ''R

exampleDepositValidator ::
  () ->
  R ->
  ScriptContext ->
  Bool
exampleDepositValidator _ redeemer ctx =
  checkDepositHash
 where
  checkDepositHash =
    traceIfFalse "Invalid deposit script hash" (List.length depositOutput == 1)

  depositOutput = findDepositOutput expectedDepositValidator

  findDepositOutput :: ScriptHash -> List.List TxOut
  findDepositOutput depositScriptHash =
    let allOutputs = List.fromSOP $ txInfoOutputs info
     in List.filter (isScriptAddress depositScriptHash . txOutAddress) allOutputs

  isScriptAddress :: ScriptHash -> Address -> Bool
  isScriptAddress expectedHash addr =
    case addressCredential addr of
      ScriptCredential vh -> vh == expectedHash
      PubKeyCredential _ -> False

  R{expectedDepositValidator} = redeemer

  info = scriptContextTxInfo ctx

exampleSecureValidatorScript :: PlutusScript
exampleSecureValidatorScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||wrap exampleDepositValidator||]
        )
 where
  wrap = wrapValidator @() @R
```

</details>
