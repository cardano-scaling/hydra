{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Version evidence established at protocol validation boundaries. Constructors
-- stay private so a version index always agrees with its runtime value.
module Hydra.HeadLogic.Version (
  RequestVersion,
  SomeRequestVersion (..),
  matchRequestVersion,
  requestVersion,
  SnapshotAt,
  snapshotForRequest,
  SignedSnapshot,
  signSnapshot,
  signedSnapshot,
  snapshotSignature,
  SettledSnapshot,
  matchSettlement,
  settlementSnapshot,
  settlementVersion,
) where

import Hydra.Prelude

import GHC.TypeNats (Nat, type (+))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)

type role Version nominal
newtype Version (v :: Nat) = Version SnapshotVersion

withVersion :: SnapshotVersion -> (forall v. Version v -> r) -> r
withVersion value use = case someNatVal (fromIntegral value) of
  SomeNat (_ :: Proxy v) -> use (Version value :: Version v)

successor :: Version v -> Version (v + 1)
successor (Version value) = Version (value + 1)

-- | The request and local head versions have matched. The index is local to
-- this validation; it does not claim that all stored snapshots share it.
type role RequestVersion nominal

newtype RequestVersion v = RequestVersion (Version v)

data SomeRequestVersion where
  SomeRequestVersion :: RequestVersion v -> SomeRequestVersion

matchRequestVersion :: SnapshotVersion -> SnapshotVersion -> Maybe SomeRequestVersion
matchRequestVersion headVersion requested
  | headVersion == requested = withVersion headVersion $ \version -> Just $ SomeRequestVersion $ RequestVersion version
  | otherwise = Nothing

requestVersion :: RequestVersion v -> SnapshotVersion
requestVersion (RequestVersion (Version value)) = value

type role SnapshotAt nominal nominal
newtype SnapshotAt tx (v :: Nat) = SnapshotAt (Snapshot tx)

-- | Construct an unsigned snapshot at the validated request version. Assigning
-- the version here prevents a caller's raw record from contradicting the index.
snapshotForRequest :: RequestVersion v -> Snapshot tx -> SnapshotAt tx v
snapshotForRequest checked snapshot = SnapshotAt snapshot{version = requestVersion checked}

-- | A snapshot paired with the signature produced for that exact snapshot.
type role SignedSnapshot nominal nominal

data SignedSnapshot tx v = SignedSnapshot (SnapshotAt tx v) (Signature (Snapshot tx))

signSnapshot :: SnapshotAt tx v -> Program tx ledger 'True (SignedSnapshot tx v)
signSnapshot snapshot@(SnapshotAt value) = SignedSnapshot snapshot <$> Program.signSnapshot value

signedSnapshot :: SignedSnapshot tx v -> Snapshot tx
signedSnapshot (SignedSnapshot (SnapshotAt snapshot) _) = snapshot

snapshotSignature :: SignedSnapshot tx v -> Signature (Snapshot tx)
snapshotSignature (SignedSnapshot _ signature) = signature

-- | The observation names this snapshot and advances its version exactly once.
-- Action identity and multisignature checks remain the caller's responsibility.
data SettledSnapshot tx where
  SettledSnapshot :: SnapshotAt tx v -> Version (v + 1) -> SettledSnapshot tx

matchSettlement :: forall tx. SnapshotNumber -> SnapshotVersion -> Snapshot tx -> Maybe (SettledSnapshot tx)
matchSettlement observedNumber newVersion snapshot@Snapshot{number, version}
  | observedNumber == number && version + 1 == newVersion =
      withVersion version $ \before ->
        Just $ SettledSnapshot (atVersion before) (successor before)
  | otherwise = Nothing
 where
  atVersion :: Version v -> SnapshotAt tx v
  atVersion _ = SnapshotAt snapshot

settlementSnapshot :: SettledSnapshot tx -> Snapshot tx
settlementSnapshot (SettledSnapshot (SnapshotAt snapshot) _) = snapshot

settlementVersion :: SettledSnapshot tx -> SnapshotVersion
settlementVersion (SettledSnapshot _ (Version value)) = value
