{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.LogFilterSpec where

import Control.Lens ((^?))
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Lens (key)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Hydra.Ledger.Cardano (Tx)
import Hydra.LogFilter (filterLog, tracePerformance)
import Hydra.Logging (Envelope)
import Hydra.Logging.Messages (HydraLog)
import Hydra.Prelude
import Test.Hydra.Prelude
import Test.QuickCheck (checkCoverage, cover, tabulate)
import Test.QuickCheck.Modifiers (NonEmptyList (NonEmpty))
import qualified Prelude

entry :: LBS.ByteString
entry = "{\"message\":{\"node\":{\"by\":10,\"event\":{\"message\":{\"transactions\":[{\"witnesses\":{\"scripts\":{},\"keys\":[\"820082582028c440059807b853b2d44ee8358086a10837b9418efab69aaef4c9e981d2fd3f5840c4c1a02d2ecb38bc0e4efa93ea9147ed30132437b8f2d89f47f7e84723ea5969c43b62c1043eb20e35f7ddef4334cb093ece1d5d0ecee9358a21f44937ed6005\"]},\"body\":{\"outputs\":[{\"address\":\"addr_test1qryc674js99w50kjf30heds8eqqe0vre3d8487swgrmd7q5a8uwp3k06h9vg32z7lrnzjvpey9eymx7zq8atvz755sjqcguqss\",\"value\":{\"lovelace\":31288501}},{\"address\":\"addr_test1qzzdm3uyvzgutwzkqfv23d92gkksgt03ywknqqcxdg6r830dtlu6gds4r96mejvaa93439tl0rhrwj0vp4kujxuprhuqszkmcs\",\"value\":{\"lovelace\":0}}],\"mint\":{\"lovelace\":0},\"auxiliaryDataHash\":null,\"withdrawals\":[],\"certificates\":[],\"fees\":0,\"inputs\":[\"03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314#22\"],\"validity\":{\"notBefore\":1,\"notAfter\":2}},\"id\":\"6cba5394ec8a1a1161758a33089661383143283d0121e4a293ed51a0272cfbc4\",\"auxiliaryData\":null}],\"snapshotNumber\":3,\"party\":30,\"tag\":\"ReqSn\"},\"tag\":\"NetworkEvent\"},\"tag\":\"EndEvent\"},\"tag\":\"Node\"},\"timestamp\":\"2021-09-08T10:05:05.919304349Z\",\"namespace\":\"HydraNode-1\",\"threadId\":18}"

newTxEvent :: LBS.ByteString
newTxEvent =
  LBS.intercalate
    "\n"
    [ "{\"timestamp\":\"2023-06-06T16:42:23.153630815Z\",\"threadId\":340,\"namespace\":\"0\",\"message\":{\"api\":{\"receivedInput\":{\"tag\":\"NewTx\",\"transaction\":{\"body\":{\"fees\":0,\"inputs\":[\"7530768ed9b5752c6f6979351a88c72253c199cea2b2d656e59385ae26b043f7#0\"],\"outputs\":[{\"address\":\"addr_test1vr6jlh98jjweeqpe4dnsetcn48323q89vm508vgs7rfl2qcslpnka\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"lovelace\":610286507302}}]},\"id\":\"f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d\",\"isValid\":true,\"witnesses\":{\"keys\":[\"8200825820ea1e117a06ca414271bdb5f305d6c40abea2959a182337ed71f26b749d10fae9584070295ad7116d11432ef5bb30c269b4423b00ae9cb3946655e1202b4301bc7063ca426eab497f59a4d7c858090e1dbab602c30f16a62728d32267a575a981fe01\"]}}},\"tag\":\"APIInputReceived\"},\"tag\":\"APIServer\"}}"
    , "{\"timestamp\":\"2023-06-06T16:42:23.154153279Z\",\"threadId\":73,\"namespace\":\"0\",\"message\":{\"node\":{\"by\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"event\":{\"clientInput\":{\"tag\":\"NewTx\",\"transaction\":{\"body\":{\"fees\":0,\"inputs\":[\"7530768ed9b5752c6f6979351a88c72253c199cea2b2d656e59385ae26b043f7#0\"],\"outputs\":[{\"address\":\"addr_test1vr6jlh98jjweeqpe4dnsetcn48323q89vm508vgs7rfl2qcslpnka\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"lovelace\":610286507302}}]},\"id\":\"f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d\",\"isValid\":true,\"witnesses\":{\"keys\":[\"8200825820ea1e117a06ca414271bdb5f305d6c40abea2959a182337ed71f26b749d10fae9584070295ad7116d11432ef5bb30c269b4423b00ae9cb3946655e1202b4301bc7063ca426eab497f59a4d7c858090e1dbab602c30f16a62728d32267a575a981fe01\"]}}},\"tag\":\"ClientEvent\"},\"eventId\":11,\"tag\":\"BeginEvent\"},\"tag\":\"Node\"}}"
    , "{\"timestamp\":\"2023-06-06T16:42:23.154158309Z\",\"threadId\":73,\"namespace\":\"0\",\"message\":{\"node\":{\"by\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"outcome\":{\"effects\":[{\"message\":{\"party\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"tag\":\"ReqTx\",\"transaction\":{\"body\":{\"fees\":0,\"inputs\":[\"7530768ed9b5752c6f6979351a88c72253c199cea2b2d656e59385ae26b043f7#0\"],\"outputs\":[{\"address\":\"addr_test1vr6jlh98jjweeqpe4dnsetcn48323q89vm508vgs7rfl2qcslpnka\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"lovelace\":610286507302}}]},\"id\":\"f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d\",\"isValid\":true,\"witnesses\":{\"keys\":[\"8200825820ea1e117a06ca414271bdb5f305d6c40abea2959a182337ed71f26b749d10fae9584070295ad7116d11432ef5bb30c269b4423b00ae9cb3946655e1202b4301bc7063ca426eab497f59a4d7c858090e1dbab602c30f16a62728d32267a575a981fe01\"]}}},\"tag\":\"NetworkEffect\"}],\"tag\":\"OnlyEffects\"},\"tag\":\"LogicOutcome\"},\"tag\":\"Node\"}}"
    , "{\"timestamp\":\"2023-06-06T16:42:23.154163359Z\",\"threadId\":73,\"namespace\":\"0\",\"message\":{\"node\":{\"by\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"effect\":{\"message\":{\"party\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"tag\":\"ReqTx\",\"transaction\":{\"body\":{\"fees\":0,\"inputs\":[\"7530768ed9b5752c6f6979351a88c72253c199cea2b2d656e59385ae26b043f7#0\"],\"outputs\":[{\"address\":\"addr_test1vr6jlh98jjweeqpe4dnsetcn48323q89vm508vgs7rfl2qcslpnka\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"lovelace\":610286507302}}]},\"id\":\"f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d\",\"isValid\":true,\"witnesses\":{\"keys\":[\"8200825820ea1e117a06ca414271bdb5f305d6c40abea2959a182337ed71f26b749d10fae9584070295ad7116d11432ef5bb30c269b4423b00ae9cb3946655e1202b4301bc7063ca426eab497f59a4d7c858090e1dbab602c30f16a62728d32267a575a981fe01\"]}}},\"tag\":\"NetworkEffect\"},\"effectId\":0,\"eventId\":11,\"tag\":\"BeginEffect\"},\"tag\":\"Node\"}}"
    , "{\"timestamp\":\"2023-06-06T16:42:23.154170212Z\",\"threadId\":73,\"namespace\":\"0\",\"message\":{\"node\":{\"by\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"effectId\":0,\"eventId\":11,\"tag\":\"EndEffect\"},\"tag\":\"Node\"}}"
    , "{\"timestamp\":\"2023-06-06T16:42:23.154172246Z\",\"threadId\":73,\"namespace\":\"0\",\"message\":{\"node\":{\"by\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"eventId\":11,\"tag\":\"EndEvent\"},\"tag\":\"Node\"}}"
    ]

spec :: Spec
spec = parallel $ do
  it "keeps basic structure of entry" $ do
    let filtered = decode entry >>= filterLog
    (filtered >>= (^? key "timestamp"))
      `shouldBe` Just (String "2021-09-08T10:05:05.919304349Z")

    (filtered >>= (^? key "threadId"))
      `shouldBe` Just (Number 18)

    (filtered >>= (^? key "namespace"))
      `shouldBe` Just (String "HydraNode-1")

  it "replaces transactions by their ids in ReqSn" $ do
    (decode entry >>= filterLog >>= (^? key "message" . key "event" . key "message" . key "transactions"))
      `shouldBe` Just (Array [String "6cba5394ec8a1a1161758a33089661383143283d0121e4a293ed51a0272cfbc4"])

  prop "significantly reduces standard log messages size" $ \(ReasonablySized (NonEmpty logs)) ->
    let jsonLogs = map toJSON (logs :: [Envelope (HydraLog Tx Text)])
        bytes = mconcat $ map encode jsonLogs
        filtered = encode $ mapMaybe filterLog jsonLogs
        sizeRatio = fromIntegral (LBS.length filtered) * (100.0 :: Double) / fromIntegral (LBS.length bytes)
     in LBS.length filtered < LBS.length bytes && sizeRatio > 0
          & cover 40 (sizeRatio < 10.0) "reduces size by 90%"
          & tabulate "Ratios" [show (floor (sizeRatio / 10) * 10 :: Int) <> " %"]
          & checkCoverage

  describe "Performance traces" $ do
    it "generate trace for NewTx" $ do
      let result = tracePerformance newTxEvent
      let timestamp :: UTCTime = fromJust $ iso8601ParseM "2023-06-06T16:42:23.154153279Z"

      result
        `shouldBe` [ object
                      [ "timestamp" .= timestamp
                      , "event" .= ("NewTx" :: Text)
                      , "id" .= ("f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d" :: Text)
                      , "us" .= (18.967 :: Double)
                      ]
                   ]
