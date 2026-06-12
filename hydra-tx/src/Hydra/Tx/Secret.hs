{-# LANGUAGE UndecidableInstances #-}

-- | A type-level barrier around values that must never be shown, logged,
-- or serialised.
--
-- Wrapping a value in 'Secret' makes the following a compile-time error
-- (via 'GHC.TypeError') with a custom message:
--
--   * 'ToJSON' / 'FromJSON'
--   * 'ToCBOR' / 'FromCBOR'
--   * 'Codec.Serialise.Serialise'
--
-- 'Show' is provided but renders as a redacted placeholder
-- (@\"\<Secret field of type \<typename\>\>\"@), so enclosing records can
-- still @deriving stock (Show)@ for free.
--
-- The only way to read the wrapped value is 'withSecret', a continuation-
-- style accessor: there is intentionally no @revealSecret :: Secret a -> a@.
-- That forces every consumption site to be a small lexical scope and keeps
-- raw values from outliving the use site.
module Hydra.Tx.Secret (
  Secret,
  mkSecret,
  withSecret,
  Forbid,
) where

import Hydra.Prelude hiding (show)

import Codec.Serialise (Serialise (..))
import Control.Exception (TypeError (..), throw)
import Data.Typeable (typeRep)
import GHC.TypeError (ErrorMessage (..))
import GHC.TypeError qualified as TE
import Text.Show (Show (..))

-- | A value the type system refuses to show or serialise.
newtype Secret a = Secret a
  deriving stock (Eq, Ord)

mkSecret :: a -> Secret a
mkSecret = Secret

-- | The only escape hatch. Continuation-style so the raw value is never
-- bound at the call site outside the supplied function.
withSecret :: Secret a -> (a -> r) -> r
withSecret (Secret a) f = f a

-- The GHC.TypeError mechanism: declaring the instance with a 'TypeError'
-- constraint makes the instance visible to the solver (so deriving clauses
-- on records containing a 'Secret' field still resolve to it) while
-- emitting the supplied message whenever GHC tries to select it.
type Forbid op =
  TE.TypeError
    ( 'Text "Refusing to "
        ':<>: 'Text op
        ':<>: 'Text " a value marked as secret."
        ':$$: 'Text "Secret values (e.g. signing keys) must not be"
        ':$$: 'Text "shown, logged, or serialised. Use `withSecret` from"
        ':$$: 'Text "Hydra.Tx.Secret to consume the inner value at its"
        ':$$: 'Text "point of use (e.g. signing). If you got here from a"
        ':$$: 'Text "derived `Show` / `ToJSON` on an enclosing record,"
        ':$$: 'Text "either drop that deriving clause or write a"
        ':$$: 'Text "hand-rolled instance that omits the secret field."
    )

-- | Renders as @\"\<Secret field of type \<typename\>\>\"@. The
-- 'Typeable' constraint lets the instance name the wrapped type without
-- ever touching the value. Enclosing records can keep using
-- 'deriving stock (Show)' and get a redacted rendering for free.
instance Typeable a => Show (Secret a) where
  show _ = "<Secret field of type " <> show (typeRep (Proxy :: Proxy a)) <> ">"

-- The bodies below should be unreachable at runtime: the 'Forbid'
-- 'TypeError' constraint fires at normal compile time. They throw a
-- 'Control.Exception.TypeError' so that under '-fdefer-type-errors'
-- (where the compile error is converted to a runtime exception) the
-- exception type matches what 'shouldNotTypecheck' looks for.
instance Forbid "encode to JSON" => ToJSON (Secret a) where
  toJSON _ = throw (TypeError "Refusing to encode Secret to JSON")

instance Forbid "decode from JSON" => FromJSON (Secret a) where
  parseJSON _ = throw (TypeError "Refusing to decode Secret from JSON")

instance (Typeable a, Forbid "CBOR-encode") => ToCBOR (Secret a) where
  toCBOR _ = throw (TypeError "Refusing to CBOR-encode Secret")

instance (Typeable a, Forbid "CBOR-decode") => FromCBOR (Secret a) where
  fromCBOR = throw (TypeError "Refusing to CBOR-decode Secret")

instance Forbid "Serialise-encode" => Serialise (Secret a) where
  encode _ = throw (TypeError "Refusing to Serialise-encode Secret")
  decode = throw (TypeError "Refusing to Serialise-decode Secret")
