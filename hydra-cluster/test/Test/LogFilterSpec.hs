{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.LogFilterSpec where

import Data.Aeson (decode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Hydra.Ledger.Cardano (Tx)
import Hydra.LogFilter (tracePerformance)
import Hydra.Prelude
import Test.Hydra.Prelude

logEntries :: [LBS.ByteString]
logEntries =
  [ "{\"timestamp\":\"2023-06-06T16:42:23.153630815Z\",\"threadId\":340,\"namespace\":\"0\",\"message\":{\"api\":{\"receivedInput\":{\"tag\":\"NewTx\",\"transaction\":{\"body\":{\"fees\":0,\"inputs\":[\"7530768ed9b5752c6f6979351a88c72253c199cea2b2d656e59385ae26b043f7#0\"],\"outputs\":[{\"address\":\"addr_test1vr6jlh98jjweeqpe4dnsetcn48323q89vm508vgs7rfl2qcslpnka\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"lovelace\":610286507302}}]},\"id\":\"f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d\",\"isValid\":true,\"witnesses\":{\"keys\":[\"8200825820ea1e117a06ca414271bdb5f305d6c40abea2959a182337ed71f26b749d10fae9584070295ad7116d11432ef5bb30c269b4423b00ae9cb3946655e1202b4301bc7063ca426eab497f59a4d7c858090e1dbab602c30f16a62728d32267a575a981fe01\"]}}},\"tag\":\"APIInputReceived\"},\"tag\":\"APIServer\"}}"
  , "{\"timestamp\":\"2023-06-06T16:42:23.154153279Z\",\"threadId\":73,\"namespace\":\"0\",\"message\":{\"node\":{\"by\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"event\":{\"clientInput\":{\"tag\":\"NewTx\",\"transaction\":{\"body\":{\"fees\":0,\"inputs\":[\"7530768ed9b5752c6f6979351a88c72253c199cea2b2d656e59385ae26b043f7#0\"],\"outputs\":[{\"address\":\"addr_test1vr6jlh98jjweeqpe4dnsetcn48323q89vm508vgs7rfl2qcslpnka\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"lovelace\":610286507302}}]},\"id\":\"f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d\",\"isValid\":true,\"witnesses\":{\"keys\":[\"8200825820ea1e117a06ca414271bdb5f305d6c40abea2959a182337ed71f26b749d10fae9584070295ad7116d11432ef5bb30c269b4423b00ae9cb3946655e1202b4301bc7063ca426eab497f59a4d7c858090e1dbab602c30f16a62728d32267a575a981fe01\"]}}},\"tag\":\"ClientEvent\"},\"eventId\":11,\"tag\":\"BeginEvent\"},\"tag\":\"Node\"}}"
  , "{\"timestamp\":\"2023-06-06T16:42:23.154158309Z\",\"threadId\":73,\"namespace\":\"0\",\"message\":{\"node\":{\"by\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"outcome\":{\"effects\":[{\"message\":{\"party\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"tag\":\"ReqTx\",\"transaction\":{\"body\":{\"fees\":0,\"inputs\":[\"7530768ed9b5752c6f6979351a88c72253c199cea2b2d656e59385ae26b043f7#0\"],\"outputs\":[{\"address\":\"addr_test1vr6jlh98jjweeqpe4dnsetcn48323q89vm508vgs7rfl2qcslpnka\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"lovelace\":610286507302}}]},\"id\":\"f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d\",\"isValid\":true,\"witnesses\":{\"keys\":[\"8200825820ea1e117a06ca414271bdb5f305d6c40abea2959a182337ed71f26b749d10fae9584070295ad7116d11432ef5bb30c269b4423b00ae9cb3946655e1202b4301bc7063ca426eab497f59a4d7c858090e1dbab602c30f16a62728d32267a575a981fe01\"]}}},\"tag\":\"NetworkEffect\"}],\"tag\":\"Effects\"},\"tag\":\"LogicOutcome\"},\"tag\":\"Node\"}}"
  , "{\"timestamp\":\"2023-06-06T16:42:23.154163359Z\",\"threadId\":73,\"namespace\":\"0\",\"message\":{\"node\":{\"by\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"effect\":{\"message\":{\"party\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"tag\":\"ReqTx\",\"transaction\":{\"body\":{\"fees\":0,\"inputs\":[\"7530768ed9b5752c6f6979351a88c72253c199cea2b2d656e59385ae26b043f7#0\"],\"outputs\":[{\"address\":\"addr_test1vr6jlh98jjweeqpe4dnsetcn48323q89vm508vgs7rfl2qcslpnka\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"lovelace\":610286507302}}]},\"id\":\"f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d\",\"isValid\":true,\"witnesses\":{\"keys\":[\"8200825820ea1e117a06ca414271bdb5f305d6c40abea2959a182337ed71f26b749d10fae9584070295ad7116d11432ef5bb30c269b4423b00ae9cb3946655e1202b4301bc7063ca426eab497f59a4d7c858090e1dbab602c30f16a62728d32267a575a981fe01\"]}}},\"tag\":\"NetworkEffect\"},\"effectId\":0,\"eventId\":11,\"tag\":\"BeginEffect\"},\"tag\":\"Node\"}}"
  , "{\"timestamp\":\"2023-06-06T16:42:23.154170212Z\",\"threadId\":73,\"namespace\":\"0\",\"message\":{\"node\":{\"by\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"effectId\":0,\"eventId\":11,\"tag\":\"EndEffect\"},\"tag\":\"Node\"}}"
  , "{\"timestamp\":\"2023-06-06T16:42:23.154172246Z\",\"threadId\":73,\"namespace\":\"0\",\"message\":{\"node\":{\"by\":{\"vkey\":\"cd03fbddcaaa2703c251656d5ccdd99f5635b1e0653c0636b951a3a3db21dad4\"},\"eventId\":11,\"tag\":\"EndEvent\"},\"tag\":\"Node\"}}"
  ]

spec :: Spec
spec = parallel $ do
  describe "Performance traces" $ do
    it "generate trace for NewTx" $ do
      let analyseEvents = map (tracePerformance @Tx . fromJust . decode) logEntries

          result = map toJSON $ concat $ evalState (sequence analyseEvents) mempty

      result
        `shouldBe` [ object
                      [ "timestamp" .= (fromJust $ iso8601ParseM "2023-06-06T16:42:23.154163359Z" :: UTCTime)
                      , "effect" .= ("ReqTx" :: Text)
                      , "txid" .= ("f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d" :: Text)
                      , "us" .= (6.853 :: Double)
                      , "tag" .= ("TraceEffect" :: Text)
                      ]
                   , object
                      [ "timestamp" .= (fromJust $ iso8601ParseM "2023-06-06T16:42:23.154153279Z" :: UTCTime)
                      , "event" .= ("NewTx" :: Text)
                      , "txid" .= ("f415a0346fe5ff938ef42b538b4a178e120baaa873dbc0edd693490001477e7d" :: Text)
                      , "us" .= (18.967 :: Double)
                      , "tag" .= ("TraceEvent" :: Text)
                      ]
                   ]
