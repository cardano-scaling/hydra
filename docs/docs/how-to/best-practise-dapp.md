---
sidebar_position: 9
---

# Best practices for dApp developers


- Developers building decentralized applications (dApps) on the Hydra protocol
would greatly benefit from clear guidance and examples detailing the validator
checks required to ensure that committed funds are accurately directed to the
intended Hydra Head instance.

- This document aims to outline the essential validator checks for user-written
validators, providing concrete examples implemented using PlutusTx.


- To begin, let us examine the diagram illustrating the commit process in detail:
![](./commit-process.jpg)

<sub> Here rectangulars represent transactions and you can see the UTxO (with rounded corners) depicting transaction inputs and outputs together with their contents (datums, redeemers and assets)</sub>

The commit transaction utilizes an initial output specifically crafted for each Hydra Head participant. This initial output includes the HeadId in its datum, while the redeemer contains details about the specific TxOutRef values to be committed, which are pre-sorted.

The initial validator is parameterized by the commit validator hash. In the user-defined validator, it is sufficient to verify that:
    - The initial input with the correct hash is consumed in the commit transaction.
    - The initial input’s datum contains the correct HeadId.
    - There is only one output at commit script address + the change output.

dApp developers can leverage the script redeemer to convey the necessary information.

Let us now proceed with constructing the validator from the user’s perspective!


### Building a secure commit validator

To develop decentralized applications (dApps) on the Hydra protocol, users must commit their scripts to a Hydra Head.

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
They serve the purpose of giving dApp developers a general idea on how to check that commit goes to the right Head instance.
:::

We mentioned we will use redeemer in our validator to carry information we need to do the actual checks. Let's define the redeemer first:

```Haskell
data R =
  R
   { expectedHeadId :: CurrencySymbol
   , expectedInitialValidator :: ScriptHash
   , expectedCommitValidator :: ScriptHash
   } deriving stock (Show, Generic)

unstableMakeIsData ''R

```

To get the information on Hydra scripts hashes you can use hydra-node:

```
hydra-node -- --hydra-script-catalogue
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

To get the information on `HeadId` easiest is to look at the persistence folder of your hydra-node and in the `state` file
you should be able to find the headId. (NOTE: You need to initialize the Head first)

Now we are able to start working on validator checks.

First, let's make sure correct initial input is spent. In order to do that we could grab all inputs and
make sure there is exactly one script input which `Address` corresponds to the initial validator hash we have set in our own script redeemer:

```Haskell
  initialInput = findInitialInput expectedInitialValidator

  findInitialInput :: ScriptHash -> Maybe TxInInfo
  findInitialInput initialScriptHash =
    let allInputs = List.fromSOP $ txInfoInputs info
     in List.find (isScriptAddress initialScriptHash . txOutAddress . txInInfoResolved) allInputs

  -- Check if an address is a script address with the specified script hash
  isScriptAddress :: ScriptHash -> Address -> Bool
  isScriptAddress expectedHash addr =
    case addressCredential addr of
      ScriptCredential vh -> vh == expectedHash
      PubKeyCredential _ -> False
```

To check the correctness of the Head ID we can try to decode the initial datum which holds the `CurrencySymbol` of a Head and compare it with what is set in our
own script redeemer:

```Haskell
  extractDatum =
    case initialInput of
      Nothing -> traceError "Initial input not found, cannot decode datum"
      Just i -> Just =<< decodeDatum (txInInfoResolved i)

  decodeDatum :: TxOut -> Maybe CurrencySymbol
  decodeDatum txOut = case txOutDatum txOut of
    OutputDatum d -> fromBuiltinData (getDatum d)
    _ -> Nothing

```

To make sure there is only one output at commit validator address we need to filter all outputs and check that there is only one
with the correct commit `Address`:

```
  checkCommitOutput =
    traceIfFalse "There should be only one commit output" (List.length commitOutput == 1)

  commitOutput = findCommitOutput expectedCommitValidator

  findCommitOutput :: ScriptHash -> List.List TxOut
  findCommitOutput commitScriptHash =
    let allOutputs = List.fromSOP $ txInfoOutputs info
     in List.filter (isScriptAddress commitScriptHash . txOutAddress) allOutputs

```
These checks are all we need in order to add some security to our validators and make sure our commit will end up in the correct Head instance.

Following step is to build a _blueprint_ transaction as a recipe for committing your script UTxO into a Head. We already have a guide
on that [here](./commit-script-utxo#step-5-prepare-the-blueprint) but you would need to make some changes since now we use different script,
there is no datum and our redeemer is `R`.

The hash or our validator script is `c4438c8c41e405aaa9349f1455364261c1006991ead68be459353fa8`. This information is important to be able to build
the _blueprint_ transaction from the guide and we leave the rest as an exercise to the user. It should not be hard to build this transaction since you
only need to worry about the transaction inputs. Even if outputs are defined in the transaction they would be ignored since all inputs end up
on the L2 ledger owned by our script.

<details>
  <summary>Complete validator example </summary>
```

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Example where

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import Hydra.Plutus.Extras (wrapValidator)
import PlutusLedgerApi.V3 (
  Address,
  Credential (..),
  CurrencySymbol,
  OutputDatum (..),
  ScriptContext (..),
  ScriptHash,
  ScriptInfo (..),
  TxInInfo,
  TxOut,
  addressCredential,
  fromBuiltinData,
  getDatum,
  serialiseCompiledCode,
  txInInfoResolved,
  txInfoOutputs,
  txInfoInputs,
  txOutAddress,
  txOutDatum,
 )
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.Data.List qualified as List
import PlutusTx.Eq ((==))
import PlutusTx.Prelude (check, traceError, traceIfFalse)

data R = R
  { expectedHeadId :: CurrencySymbol
  , expectedInitialValidator :: ScriptHash
  , expectedCommitValidator :: ScriptHash
  }
  deriving stock (Show, Generic)

unstableMakeIsData ''R

exampleValidator ::
  () ->
  R ->
  ScriptContext ->
  Bool
exampleValidator _ redeemer ctx =
  checkInitialInputIsSpent
    && checkCorrectHeadId
    && checkCommitOutput
 where
  checkInitialInputIsSpent =
    traceIfFalse "Initial input not found" (isJust initialInput)

  checkCommitOutput =
    traceIfFalse "There should be only one commit output" (List.length commitOutput == 1)

  checkCorrectHeadId =
    case extractDatum of
      Nothing -> traceError "Could not decode initial datum"
      Just headId -> traceIfFalse "HeadId is not correct" $ headId == expectedHeadId

  extractDatum =
    case initialInput of
      Nothing -> traceError "Initial input not found, cannot decode datum"
      Just i -> Just =<< decodeDatum (txInInfoResolved i)

  decodeDatum :: TxOut -> Maybe CurrencySymbol
  decodeDatum txOut = case txOutDatum txOut of
    OutputDatum d -> fromBuiltinData (getDatum d)
    _ -> Nothing

  initialInput = findInitialInput expectedInitialValidator

  commitOutput = findCommitOutput expectedCommitValidator

  findCommitOutput :: ScriptHash -> List.List TxOut
  findCommitOutput commitScriptHash =
    let allOutputs = List.fromSOP $ txInfoOutputs info
     in List.filter (isScriptAddress commitScriptHash . txOutAddress) allOutputs

  findInitialInput :: ScriptHash -> Maybe TxInInfo
  findInitialInput initialScriptHash =
    let allInputs = List.fromSOP $ txInfoInputs info
     in List.find (isScriptAddress initialScriptHash . txOutAddress . txInInfoResolved) allInputs

  -- Check if an address is a script address with the specified script hash
  isScriptAddress :: ScriptHash -> Address -> Bool
  isScriptAddress expectedHash addr =
    case addressCredential addr of
      ScriptCredential vh -> vh == expectedHash
      PubKeyCredential _ -> False

  info = scriptContextTxInfo ctx

  R{expectedHeadId, expectedInitialValidator, expectedCommitValidator} = redeemer

exampleSecureValidatorScript :: PlutusScript
exampleSecureValidatorScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||wrap exampleValidator||]
        )
 where
  wrap = wrapValidator @() @R
```
</details>

