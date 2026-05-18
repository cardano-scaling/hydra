{-# LANGUAGE TemplateHaskell #-}

module Hydra.TUI.Logging.Types where

import Hydra.Prelude
import Lens.Micro.TH (makeLensesFor)

type Severity :: Type
data Severity
  = Success
  | Info
  | Error
  deriving stock (Eq, Show, Generic)

type LogMessage :: Type
data LogMessage = LogMessage
  { severity :: Severity
  , message :: Text
  , detail :: Text
  , time :: UTCTime
  , rawJson :: Text
  }
  deriving stock (Eq, Show, Generic)

type LogVerbosity :: Type
data LogVerbosity = Short | Full
  deriving stock (Eq, Show, Generic)

type EventHistoryFilter :: Type
data EventHistoryFilter = ShowAll | ErrorsOnly
  deriving stock (Eq, Show, Generic)

type LogState :: Type
data LogState = LogState
  { logMessages :: [LogMessage]
  , logVerbosity :: LogVerbosity
  }

makeLensesFor
  [ ("logMessages", "logMessagesL")
  , ("logVerbosity", "logVerbosityL")
  ]
  ''LogState
