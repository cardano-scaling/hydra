{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Terms where

import           Plutus.V1.Ledger.Api       (TxOut)
import qualified PlutusTx                   as Tx
import qualified PlutusTx.Builtins          as Tx
import qualified TxOutGen
import qualified UntypedPlutusCore          as UPLC

import           EncodeTxOut
import           Plutus.Codec.CBOR.Encoding (Encoding, encodeByteString,
                                             encodeInteger, encodeList,
                                             encodeListLen, encodeMap,
                                             encodeMaybe,
                                             encodingToBuiltinByteString)

-- Code for creating PLC terms which serialise a single TxOut object or a list
-- of them using various methods.

type Term name = UPLC.Term name UPLC.DefaultUni UPLC.DefaultFun ()
type Program name = UPLC.Program name UPLC.DefaultUni UPLC.DefaultFun ()

bodyOf :: Program name -> Term name
bodyOf (UPLC.Program _ _ term) = term


-- Take a TxOut (or a list of them), lift it to a Scott object, then create a
-- script which serialises that using the plutus-cbor library.

serialiseSingleScottTxOutUsingLibrary :: TxOut -> Term UPLC.NamedDeBruijn
serialiseSingleScottTxOutUsingLibrary txOut =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \x ->
                               let bytes = encodingToBuiltinByteString (encodeTxOut x)  -- !!!!!!!
                               in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOut)

serialiseMultipleScottTxOutsUsingLibrary :: [TxOut] -> Term UPLC.NamedDeBruijn
serialiseMultipleScottTxOutsUsingLibrary txOuts =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \xs ->
                               let bytes = encodingToBuiltinByteString (encodeList encodeTxOut xs)
                               in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOuts)


-- Take a TxOut (or a list of them), lift it to a Scott object, then create a
-- script which serialises that using the serialiseData builtin.

serialiseSingleScottTxOutUsingBuiltin :: TxOut -> Term UPLC.NamedDeBruijn
serialiseSingleScottTxOutUsingBuiltin txOut =
    bodyOf . Tx.getPlc $
               $$(Tx.compile
                        [||
                         \(x::TxOut) ->
                             let bytes = Tx.serialiseData (Tx.toBuiltinData x)
                             in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                        ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOut)

serialiseMultipleScottTxOutsUsingBuiltin :: [TxOut] -> Term UPLC.NamedDeBruijn
serialiseMultipleScottTxOutsUsingBuiltin txOuts =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \(xs::[TxOut]) ->
                               let bytes = Tx.serialiseData (Tx.toBuiltinData xs)
                               in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode txOuts)


-- Take a TxOut (or a list of them), convert to Data, lift that to a Scott
-- object, then create a script which serialises that using the serialiseData
-- builtin.

serialiseUsingBuiltin_after_toDataOffChain :: Tx.ToData a => a -> Term UPLC.NamedDeBruijn
serialiseUsingBuiltin_after_toDataOffChain x =
      bodyOf . Tx.getPlc $
                 $$(Tx.compile
                          [||
                           \d ->
                               let bytes = Tx.serialiseData d
                               in Tx.lengthOfByteString bytes `Tx.greaterThanInteger` 0
                          ||]
                   )
                 `Tx.applyCode` (Tx.liftCode $ Tx.toBuiltinData x)

