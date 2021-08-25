{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.EndToEndSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api (
  Address,
  AsType (AsPaymentKey),
  BuildTxWith (BuildTxWith),
  Key (VerificationKey),
  KeyWitnessInCtx (KeyWitnessForSpending),
  MaryEra,
  MultiAssetSupportedInEra (MultiAssetInMaryEra),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  ShelleyWitnessSigningKey (WitnessPaymentKey),
  SigningKey,
  StakeAddressReference (NoStakeAddress),
  Tx,
  TxAuxScripts (TxAuxScriptsNone),
  TxBody (TxBody),
  TxBodyContent (..),
  TxCertificates (TxCertificatesNone),
  TxExtraKeyWitnesses (TxExtraKeyWitnessesNone),
  TxExtraScriptData (TxExtraScriptDataNone),
  TxFee (TxFeeExplicit),
  TxFeesExplicitInEra (TxFeesExplicitInMaryEra),
  TxId,
  TxIn (..),
  TxInsCollateral (TxInsCollateralNone),
  TxMetadataInEra (TxMetadataNone),
  TxMintValue (TxMintNone),
  TxOut (TxOut),
  TxOutDatumHash (TxOutDatumHashNone),
  TxOutValue (TxOutValue),
  TxUpdateProposal (TxUpdateProposalNone),
  TxValidityLowerBound (TxValidityNoLowerBound),
  TxValidityUpperBound (TxValidityNoUpperBound),
  TxWithdrawals (TxWithdrawalsNone),
  ValidityNoUpperBoundSupportedInEra (ValidityNoUpperBoundInMaryEra),
  Witness (KeyWitness),
  deterministicSigningKey,
  deterministicSigningKeySeedSize,
  getTxBody,
  getTxId,
  getTxWitnesses,
  getVerificationKey,
  lovelaceToValue,
  makeShelleyAddress,
  makeTransactionBody,
  serialiseAddress,
  serialiseToCBOR,
  shelleyAddressInEra,
  signShelleyTransaction,
  verificationKeyHash,
 )
import Cardano.Api.Shelley (ShelleyAddr)
import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Data.Aeson (Value (Null, Object, String), object, (.=))
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import qualified Data.Map as Map
import Hydra.Logging (showLogsOnFailure)
import HydraNode (
  getMetrics,
  hydraNodeProcess,
  input,
  output,
  readCreateProcess,
  send,
  waitFor,
  waitForNodesConnected,
  withHydraNode,
  withMockChain,
  withTempDir,
 )
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

spec :: Spec
spec = around showLogsOnFailure $
  describe "End-to-end test using a mocked chain though" $ do
    describe "three hydra nodes scenario" $ do
      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer -> do
        failAfter 30 $
          withTempDir "end-to-end-inits-and-closes" $ \tmpDir ->
            withMockChain $ \chainPorts ->
              withHydraNode tracer tmpDir chainPorts 1 aliceSk [bobVk, carolVk] $ \n1 ->
                withHydraNode tracer tmpDir chainPorts 2 bobSk [aliceVk, carolVk] $ \n2 ->
                  withHydraNode tracer tmpDir chainPorts 3 carolSk [aliceVk, bobVk] $ \n3 -> do
                    waitForNodesConnected tracer [n1, n2, n3]
                    let contestationPeriod = 10 :: Natural
                    send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
                    waitFor tracer 3 [n1, n2, n3] $
                      output "ReadyToCommit" ["parties" .= [int 10, 20, 30]]

                    let someUtxo =
                          Map.singleton
                            (TxIn someTxId $ toEnum 0)
                            $ object
                              [ "address" .= String (serialiseAddress inHeadAliceAddress)
                              , "value"
                                  .= object
                                    [ "lovelace" .= int 14
                                    ]
                              ]
                    send n1 $ input "Commit" ["utxo" .= someUtxo]
                    send n2 $ input "Commit" ["utxo" .= Object mempty]
                    send n3 $ input "Commit" ["utxo" .= Object mempty]
                    waitFor tracer 3 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= someUtxo]

                    let tx = txToJson txAlicePaysHerself
                    send n1 $ input "NewTx" ["transaction" .= tx]

                    let newTxId = getTxId (getTxBody txAlicePaysHerself)
                        newUtxo =
                          Map.singleton
                            (TxIn newTxId $ toEnum 0)
                            $ object
                              [ "address" .= String (serialiseAddress inHeadAliceAddress)
                              , "value"
                                  .= object
                                    [ "lovelace" .= int 14
                                    ]
                              ]

                    waitFor tracer 10 [n1, n2, n3] $
                      output "TxSeen" ["transaction" .= tx]
                    waitFor tracer 10 [n1, n2, n3] $
                      output
                        "SnapshotConfirmed"
                        [ "snapshot"
                            .= object
                              [ "confirmedTransactions" .= [tx]
                              , "snapshotNumber" .= int 1
                              , "utxo" .= newUtxo
                              ]
                        ]

                    send n1 $ input "GetUtxo" []
                    waitFor tracer 10 [n1] $ output "Utxo" ["utxo" .= newUtxo]

                    send n1 $ input "Close" []
                    waitFor tracer 3 [n1] $
                      output
                        "HeadIsClosed"
                        [ "contestationPeriod" .= contestationPeriod
                        , "latestSnapshot"
                            .= object
                              [ "snapshotNumber" .= int 1
                              , "utxo" .= newUtxo
                              , "confirmedTransactions" .= [tx]
                              ]
                        ]
                    waitFor tracer (contestationPeriod + 3) [n1] $
                      output "HeadIsFinalized" ["utxo" .= newUtxo]

    describe "Monitoring" $ do
      it "Node exposes Prometheus metrics on port 6001" $ \tracer -> do
        withTempDir "end-to-end-prometheus-metrics" $ \tmpDir ->
          failAfter 20 $
            withMockChain $ \mockPorts ->
              withHydraNode tracer tmpDir mockPorts 1 aliceSk [bobVk, carolVk] $ \n1 ->
                withHydraNode tracer tmpDir mockPorts 2 bobSk [aliceVk, carolVk] $ \_n2 ->
                  withHydraNode tracer tmpDir mockPorts 3 carolSk [aliceVk, bobVk] $ \_n3 -> do
                    waitForNodesConnected tracer [n1]
                    send n1 $ input "Init" ["contestationPeriod" .= int 10]
                    waitFor tracer 3 [n1] $ output "ReadyToCommit" ["parties" .= [int 10, 20, 30]]
                    metrics <- getMetrics n1
                    metrics `shouldSatisfy` ("hydra_head_events  4" `BS.isInfixOf`)

    describe "hydra-node executable" $ do
      it "display proper semantic version given it is passed --version argument" $ \_ -> do
        failAfter 5 $ do
          version <- readCreateProcess (hydraNodeProcess ["--version"]) ""
          version `shouldSatisfy` (=~ ("[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9]+)?" :: String))

--
-- Fixtures
--

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

someTxId :: IsString s => s
someTxId = "9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903"

-- | Signing key used by alice "in head". This is distinct from the keys used to
-- do multi signatures for the Head protocol.
inHeadAliceSk :: SigningKey PaymentKey
inHeadAliceSk =
  deterministicSigningKey AsPaymentKey $ mkSeedFromBytes $ BS.replicate seedBytes 65
 where
  seedBytes = fromIntegral $ deterministicSigningKeySeedSize AsPaymentKey

inHeadAliceVk :: VerificationKey PaymentKey
inHeadAliceVk = getVerificationKey inHeadAliceSk

-- | Pay to pubkey address of alice using her "in head" credential.
inHeadAliceAddress :: Address ShelleyAddr
inHeadAliceAddress =
  makeShelleyAddress network credential reference
 where
  network = Testnet (NetworkMagic 14) -- Magic is ignored in Shelley and Ledger aPI but required in cardano-api
  credential = PaymentCredentialByKey $ verificationKeyHash inHeadAliceVk
  reference = NoStakeAddress

txAlicePaysHerself :: Tx MaryEra
txAlicePaysHerself =
  signShelleyTransaction body [WitnessPaymentKey inHeadAliceSk]
 where
  Right body =
    makeTransactionBody $
      TxBodyContent
        { txIns = [(txIn, BuildTxWith txInWitness)]
        , txInsCollateral = TxInsCollateralNone
        , txOuts = [txOut]
        , txFee = TxFeeExplicit TxFeesExplicitInMaryEra 0
        , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra)
        , txMetadata = TxMetadataNone
        , txAuxScripts = TxAuxScriptsNone
        , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
        , txExtraKeyWits = TxExtraKeyWitnessesNone
        , txProtocolParams = BuildTxWith Nothing
        , txWithdrawals = TxWithdrawalsNone
        , txCertificates = TxCertificatesNone
        , txUpdateProposal = TxUpdateProposalNone
        , txMintValue = TxMintNone
        }

  txIn = TxIn someTxId (toEnum 0)

  txInWitness = KeyWitness KeyWitnessForSpending

  txOut =
    TxOut
      (shelleyAddressInEra inHeadAliceAddress)
      (TxOutValue MultiAssetInMaryEra (lovelaceToValue 14))
      TxOutDatumHashNone

--
-- Helpers
--

int :: Int -> Int
int = id

outputRef :: TxId -> Natural -> Value
outputRef txId txIx =
  object
    [ "txId" .= txId
    , "index" .= txIx
    ]

txToJson :: Tx MaryEra -> Value
txToJson tx =
  object
    [ "id" .= getTxId txBody
    , "body"
        .= object
          [ "inputs" .= map fst (txIns content)
          , "outputs" .= txOuts content
          ]
    , "witnesses"
        .= object
          [ "keys" .= map (encodeBase16 . serialiseToCBOR) txWitnesses
          , "scripts" .= object []
          ]
    , "auxiliaryData" .= Null
    ]
 where
  txBody = getTxBody tx

  TxBody content = txBody

  txWitnesses = getTxWitnesses tx
