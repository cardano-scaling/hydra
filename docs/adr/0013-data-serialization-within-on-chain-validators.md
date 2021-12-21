# 13. Data Serialization Within On-Chain Validators

Date: 2021-12-21

## Status

Proposed

## Context

In Hydra, during the `Close` and `Contest` transitions, one must verify, within
on-chain validators, that a certain piece of data has been multi-signed by all
head participants. While verifying a multi-signature performed via [MuSig2][]
(which can be made Schnorr-compatible) is relatively easy and can rely on
existing Plutus built-ins; producing the payload / pre-image that was signed is
problematic for there's no Plutus built-ins regarding data serialization. 

Incidentally, event though there exists quite simple and compact
(implementation-wise) serialization algorithms (e.g. CBOR), this is path we do
not want to follow as there's a high chance to increase the validator size far
above an acceptable limit. 

Hence, how to obtain arbitrary serialized data within an on-chain validator?

## Decision

#### Overview

In Cardano, transactions may carry information in various ways and in particular, one must provide Plutus data as part of a transaction witness set. Those data are made available to the underlying validator script context as a (key, value) list where keys are data hashes and value the data. It's important to note that the correspondence between a hash and its data is 
verified by the ledger during phase-1 validations; 

We want to leverage this data lookup table to pass arbitrary data and their corresponding hashes to a validator. This effectively means that we introduce an extra indirection in the redeemer of the `Close` and `Context` transition. Indeed, instead of passing the full data as the redeemer, we can only give a hash which can be looked up from the script context to obtain its corresponding data. This can be achieved with the following on-chain function:

```hs
import Ledger
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap

reifyData :: PlutusTx.FromData a => ScriptContext -> DatumHash -> Maybe a
reifyData (ScriptContext info _) h =
  AssocMap.lookup h (AssocMap.fromList (txInfoData info))
  >>= 
  PlutusTx.fromBuiltinData . getDatum
{-# INLINEABLE reifyData #-}
```

#### Obstacles

There's a little quirk with this approach unfortunately: the ledger does not allow the presence of extraneous datum in the witness set. In fact, the ledger will fail phase-1 with a `NonOutputSupplimentaryDatums` error if a transaction include any datum that is neither 

a. Required by an input associated to a script address
b. Referenced by an output

Thus, without requiring a hard-fork, we must be careful including an extra output carrying the required datum hash. In the context where we control the underlying wallet, we can rather easily adds this to a change output already fueling the transaction. Note that this barely change anything for a vk output;the datum will simply be ignored by the ledger and not required for spending. 

## Consequence

- We can actually write `Close` and `Contest` validator 

[MuSig2]: https://eprint.iacr.org/2020/1261
