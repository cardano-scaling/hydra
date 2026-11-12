module Hydra.Network.PolicySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Control.Concurrent.Class.MonadSTM (modifyTVar', readTVarIO, retry, writeTVar)
import Control.Tracer (Tracer (..), emit, nullTracer)
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Secret (Secret)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import Hydra.Network.Authenticate (Signed (..))
import Hydra.Network.Policy
import Hydra.Tx (Party)
import Hydra.Tx.Crypto (HydraKey, SigningKey, sign)
import Hydra.UserValidationRules (MintingOnL2Rule (..), PolicyRule (..), UserValidationRules (..), parsePolicyFile)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)
import Test.Util (shouldRunInSim)

spec :: Spec
spec = do
  describe "policy fingerprint" $ do
    it "is a 32-byte hash rendered as lowercase hexadecimal" $ do
      let PolicyFingerprint digest = policyFingerprint unrestricted
      Text.length digest `shouldBe` 64
      digest `shouldSatisfy` Text.all (`elem` ("0123456789abcdef" :: String))

    it "includes every policy field" $ do
      let variants =
            [ unrestricted{policyRule = Just ForbidAnyPolicy}
            , unrestricted{l2MintingRule = ForbidMinting}
            , unrestricted{maxUTxOCount = Just 1}
            , unrestricted{maxOutputValueAssets = Just 1}
            , unrestricted{maxDatumSize = Just 1}
            , unrestricted{allowedScriptAddresses = Just Set.empty}
            , unrestricted{allowedDeposits = Just Set.empty}
            ]
      forM_ variants $ \rules ->
        policyFingerprint rules `shouldNotBe` policyFingerprint unrestricted

    it "normalizes the two ways to forbid policy IDs" $
      policyFingerprint unrestricted{policyRule = Just ForbidAnyPolicy}
        `shouldBe` policyFingerprint unrestricted{policyRule = Just $ AllowOnlyPolicies Set.empty}

    it "ignores YAML formatting, set order, and explicit defaults" $ do
      let a = Text.replicate 56 "a"
          b = Text.replicate 56 "b"
          firstPolicy = decodePolicy $ encodeUtf8 $ "head-policy:\n  max-utxo-count: 2\n  allowed-assets: [" <> a <> ", " <> b <> "]\n"
          secondPolicy = decodePolicy $ encodeUtf8 $ "# Same policy\nhead-policy: {allowed-assets: [" <> b <> ", " <> a <> "], l2-minting: allowed, max-utxo-count: 2}\n"
      firstPolicy `shouldSatisfy` isRight
      secondPolicy `shouldSatisfy` isRight
      (policyFingerprint <$> firstPolicy) `shouldBe` (policyFingerprint <$> secondPolicy)

    it "changes when a configured limit changes" $
      policyFingerprint unrestricted{maxUTxOCount = Just 1}
        `shouldNotBe` policyFingerprint unrestricted{maxUTxOCount = Just 2}

  it "roundtrips signed declarations through CBOR" $ do
    let declaration = response bobSk bob policy "alice-new"
    decodeFull' (serialize' declaration) `shouldBe` Right declaration

  describe "startup agreement" $ do
    it "starts without reading or publishing policies when none is configured" $ do
      (elapsed, accesses) <- shouldRunInSim $ do
        accessed <- newLabelledTVarIO "policy-store-accesses" (0 :: Int)
        let recordAccess = atomically $ modifyTVar' accessed (+ 1)
            store = PolicyStore{publish = const recordAccess, load = recordAccess >> pure []}
        start <- getMonotonicTime
        started <- newLabelledTVarIO "started" Nothing
        withPolicyAgreement nullTracer aliceSk [bob] Nothing "alice-new" store $ do
          now <- getMonotonicTime
          atomically $ writeTVar started (Just $ diffTime now start)
          threadDelay 2
        (,) <$> readTVarIO started <*> readTVarIO accessed
      elapsed `shouldBe` Just 0
      accesses `shouldBe` 0

    it "starts a single-party node immediately" $ do
      elapsed <- shouldRunInSim $ do
        store <- newStore []
        start <- getMonotonicTime
        withPolicyAgreement nullTracer aliceSk [] (Just policy) "alice-new" store $ pure ()
        end <- getMonotonicTime
        pure $ diffTime end start
      elapsed `shouldBe` 0

    it "waits for every party, then runs the action" $ do
      (early, completed) <- shouldRunInSim $ do
        store <- newStore []
        started <- newLabelledTVarIO "started" False
        concurrentlyLabelled
          ( "alice"
          , withPolicyAgreement nullTracer aliceSk [bob, carol] (Just policy) "alice-new" store $
              atomically $
                writeTVar started True
          )
          ( "peers"
          , do
              publish store $ response bobSk bob policy "alice-new"
              threadDelay 10
              early <- readTVarIO started
              publish store $ response carolSk carol policy "alice-new"
              pure early
          )
          >>= \(_, early) -> (early,) <$> readTVarIO started
      early `shouldBe` False
      completed `shouldBe` True

    it "logs the missing party and stops polling after exactly five minutes" $ do
      (logs, elapsed, started, accessesAtTimeout, accessesLater) <- shouldRunInSim $ do
        (tracer, readLogs) <- newPolicyTracer
        store <- newStore []
        started <- newLabelledTVarIO "started" False
        accessed <- newLabelledTVarIO "policy-store-accesses" (0 :: Int)
        let recordAccess = atomically $ modifyTVar' accessed (+ 1)
            countedStore =
              store
                { publish = \declaration -> recordAccess >> publish store declaration
                , load = recordAccess >> load store
                }
        start <- getMonotonicTime
        withPolicyAgreement tracer aliceSk [bob] (Just policy) "alice-new" countedStore $
          atomically $
            writeTVar started True
        end <- getMonotonicTime
        accessesAtTimeout <- readTVarIO accessed
        threadDelay 10
        (,,,,) <$> readLogs <*> pure (diffTime end start) <*> readTVarIO started <*> pure accessesAtTimeout <*> readTVarIO accessed
      logs `shouldBe` [WaitingForPolicyAgreement policy [bob], PolicyAgreementTimedOut policy [bob] []]
      elapsed `shouldBe` 300
      started `shouldBe` False
      accessesAtTimeout `shouldSatisfy` (> 0)
      accessesLater `shouldBe` accessesAtTimeout

    it "reports a different policy without running the action" $ do
      (logs, elapsed, started) <- shouldRunInSim $ do
        (tracer, readLogs) <- newPolicyTracer
        store <- newStore [response bobSk bob otherPolicy "alice-new"]
        started <- newLabelledTVarIO "started" False
        start <- getMonotonicTime
        withPolicyAgreement tracer aliceSk [bob] (Just policy) "alice-new" store $
          atomically $
            writeTVar started True
        end <- getMonotonicTime
        (,,) <$> readLogs <*> pure (diffTime end start) <*> readTVarIO started
      logs `shouldBe` [WaitingForPolicyAgreement policy [bob], PolicyAgreementTimedOut policy [] [(bob, otherPolicy)]]
      elapsed `shouldBe` 300
      started `shouldBe` False

    it "keeps the five-minute deadline when the store is unreachable" $ do
      (logs, elapsed, started) <- shouldRunInSim $ do
        (tracer, readLogs) <- newPolicyTracer
        store <- newStore []
        started <- newLabelledTVarIO "started" False
        start <- getMonotonicTime
        withPolicyAgreement tracer aliceSk [bob] (Just policy) "alice-new" store{publish = const $ atomically retry} $
          atomically $
            writeTVar started True
        end <- getMonotonicTime
        (,,) <$> readLogs <*> pure (diffTime end start) <*> readTVarIO started
      logs `shouldBe` [WaitingForPolicyAgreement policy [bob], PolicyAgreementTimedOut policy [bob] []]
      elapsed `shouldBe` 300
      started `shouldBe` False

    it "allows a mismatched peer to align before the deadline" $ do
      elapsed <- shouldRunInSim $ do
        store <- newStore [response bobSk bob otherPolicy "alice-new"]
        start <- getMonotonicTime
        concurrentlyLabelled_
          ("align", threadDelay 290 >> publish store (response bobSk bob policy "alice-new"))
          ("alice", withPolicyAgreement nullTracer aliceSk [bob] (Just policy) "alice-new" store $ pure ())
        end <- getMonotonicTime
        pure $ diffTime end start
      elapsed `shouldSatisfy` (>= 290)
      elapsed `shouldSatisfy` (< 300)

    forM_ ["stale session", "unknown party", "forged signature"] $ \reason ->
      it ("does not accept a " <> reason) $ do
        (logs, started) <- shouldRunInSim $ do
          (tracer, readLogs) <- newPolicyTracer
          let declaration = case reason of
                "stale session" -> response bobSk bob policy "alice-old"
                "unknown party" -> response carolSk carol policy "alice-new"
                _ -> response carolSk bob policy "alice-new"
          store <- newStore [declaration]
          started <- newLabelledTVarIO "started" False
          withPolicyAgreement tracer aliceSk [bob] (Just policy) "alice-new" store $
            atomically $
              writeTVar started True
          (,) <$> readLogs <*> readTVarIO started
        logs `shouldBe` [WaitingForPolicyAgreement policy [bob], PolicyAgreementTimedOut policy [bob] []]
        started `shouldBe` False

    it "allows all parties to restart with a new policy" $ do
      completed <- shouldRunInSim $ do
        store <- newStore [response bobSk bob policy "alice-old"]
        started <- newLabelledTVarIO "started" Set.empty
        let mark party = atomically $ modifyTVar' started (Set.insert party)
        void $
          concurrentlyLabelled
            ("alice", withPolicyAgreement nullTracer aliceSk [bob] (Just otherPolicy) "alice-new" store $ mark alice >> threadDelay 3)
            ("bob", withPolicyAgreement nullTracer bobSk [alice] (Just otherPolicy) "bob-new" store $ mark bob >> threadDelay 3)
        readTVarIO started
      completed `shouldBe` Set.fromList [alice, bob]

    it "keeps acknowledging peers that restart after this node is ready" $ do
      completed <- shouldRunInSim $ do
        store <- newStore []
        started <- newLabelledTVarIO "started" False
        void $
          concurrentlyLabelled
            ("alice", withPolicyAgreement nullTracer aliceSk [bob] (Just policy) "alice-new" store $ threadDelay 20)
            ( "bob"
            , do
                withPolicyAgreement nullTracer bobSk [alice] (Just policy) "bob-first" store $ threadDelay 3
                withPolicyAgreement nullTracer bobSk [alice] (Just policy) "bob-second" store $
                  atomically $
                    writeTVar started True
            )
        readTVarIO started
      completed `shouldBe` True

newPolicyTracer :: MonadLabelledSTM m => m (Tracer m PolicyLog, m [PolicyLog])
newPolicyTracer = do
  logs <- newLabelledTVarIO "policy-logs" []
  pure (Tracer . emit $ \entry -> atomically $ modifyTVar' logs (entry :), reverse <$> readTVarIO logs)

newStore :: MonadLabelledSTM m => [Signed PolicyAnnouncement] -> m (PolicyStore m)
newStore initial = do
  entries <- newLabelledTVarIO "policy-store" $ Map.fromList [(party, declaration) | declaration@Signed{party} <- initial]
  pure
    PolicyStore
      { publish = \declaration@Signed{party} -> atomically $ modifyTVar' entries (Map.insert party declaration)
      , load = Map.elems <$> readTVarIO entries
      }

response :: Secret (SigningKey HydraKey) -> Party -> PolicyFingerprint -> Text -> Signed PolicyAnnouncement
response key party fingerprint challenge =
  let declaration = PolicyAnnouncement "peer-session" fingerprint (Map.singleton alice challenge)
   in Signed declaration (sign key declaration) party

policy, otherPolicy :: PolicyFingerprint
policy = PolicyFingerprint $ Text.replicate 64 "a"
otherPolicy = PolicyFingerprint $ Text.replicate 64 "b"

unrestricted :: UserValidationRules
unrestricted = UserValidationRules Nothing AllowMinting Nothing Nothing Nothing Nothing Nothing

decodePolicy :: ByteString -> Either String UserValidationRules
decodePolicy bytes = first show (Yaml.decodeEither' bytes) >>= parseEither parsePolicyFile
