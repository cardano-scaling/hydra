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
     in List.find (isInitialAddress initialScriptHash . txOutAddress . txInInfoResolved) allInputs

  -- Check if an address is a script address with the specified script hash
  isInitialAddress :: ScriptHash -> Address -> Bool
  isInitialAddress expectedHash addr =
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

These checks are all we need in order to add some security and make sure our commit will end up in the correct Head instance.

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
  txInfoInputs,
  txOutAddress,
  txOutDatum,
 )
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.Data.List qualified as List
import PlutusTx.Eq ((==))
import PlutusTx.Prelude (check, traceError, traceIfFalse)

data R
  = R
  { expectedHeadId :: CurrencySymbol
  , expectedInitialValidator :: ScriptHash
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
 where
  checkInitialInputIsSpent =
    traceIfFalse "Initial input not found" (isJust initialInput)

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

  findInitialInput :: ScriptHash -> Maybe TxInInfo
  findInitialInput initialScriptHash =
    let allInputs = List.fromSOP $ txInfoInputs info
     in List.find (isInitialAddress initialScriptHash . txOutAddress . txInInfoResolved) allInputs

  -- Check if an address is a script address with the specified script hash
  isInitialAddress :: ScriptHash -> Address -> Bool
  isInitialAddress expectedHash addr =
    case addressCredential addr of
      ScriptCredential vh -> vh == expectedHash
      PubKeyCredential _ -> False

  info = scriptContextTxInfo ctx

  R{expectedHeadId, expectedInitialValidator} = redeemer

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

