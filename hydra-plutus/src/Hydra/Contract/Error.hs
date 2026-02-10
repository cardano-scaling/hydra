-- | Error codes to be used in plutus scripts.
--
-- Define a new type and instantiate 'ToErrorCode' for error cases you want to
-- use in scripts.
--
-- @
-- data MyError = CaseA | CaseB deriving Show
--
-- instance ToErrorCode MyError where
--   toErrorCode = \case
--     CaseA -> "CA"
--     CaseB -> "CB"
-- @
--
-- In plutus-tx, you can then use template haskell to inline the error codes
-- using the '$(errorCode ..)' splice.
--
-- @
-- validator = traceError $(errorCode CaseA)
-- @
--
-- This example will have your validator fail with user error "CA", which you
-- can match for using 'toErrorCode CaseA' in Haskell.
module Hydra.Contract.Error where

import "hydra-prelude" Hydra.Prelude
import "template-haskell" Language.Haskell.TH (Exp (..), Lit (StringL), Q)

-- | Types which are used to describe errors as short error codes in scripts.
class ToErrorCode a where
  -- | Get the short error code used in a script for given type.
  toErrorCode :: a -> Text

-- | Get the string literal from given error 'e'. Use this with template haskell
-- splices, e.g. $(errorCode MyError)
errorCode :: ToErrorCode e => e -> Q Exp
errorCode =
  pure . LitE . StringL . toString . toErrorCode
