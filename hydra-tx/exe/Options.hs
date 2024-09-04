module Options where

import Hydra.Cardano.Api
import Hydra.Prelude

import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString.Char8 qualified as BSC
import Data.Char qualified as Char
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Tx.HeadId (HeadId (..))
import Options.Applicative

data Command
  = Deposit DepositOptions
  | Recover RecoverOptions
  deriving stock (Show, Eq)

data DepositOptions = DepositOptions
  { utxoFilePath :: FilePath
  , headId :: HeadId
  , outFile :: FilePath
  , networkId :: NetworkId
  , depositDeadline :: UTCTime
  }
  deriving stock (Show, Eq)

data RecoverOptions = RecoverOptions
  { recoverTxIn :: TxIn
  , utxoFilePath :: FilePath
  , outFile :: FilePath
  , networkId :: NetworkId
  , recoverSlotNo :: SlotNo
  }
  deriving stock (Show, Eq)

commandParser :: Parser Command
commandParser =
  subcommands
 where
  subcommands =
    depositCommand <|> recoverCommand
  depositCommand =
    hsubparser $
      command
        "deposit"
        ( info
            (Deposit <$> depositOptionsParser)
            ( progDesc $
                mconcat
                  [ "Request a deposit transaction. "
                  ]
            )
        )
  recoverCommand =
    hsubparser $
      command
        "recover"
        ( info
            (Recover <$> recoverOptionsParser)
            (progDesc "Request a recover transaction")
        )

depositOptionsParser :: Parser DepositOptions
depositOptionsParser =
  DepositOptions
    <$> utxoParser
    <*> headIdParser
    <*> outputFileParser
    <*> networkIdParser
    <*> deadlineParser

recoverOptionsParser :: Parser RecoverOptions
recoverOptionsParser =
  RecoverOptions
    <$> txInParser
    <*> utxoParser
    <*> outputFileParser
    <*> networkIdParser
    <*> lowerBoundSlotParser

txInParser :: Parser TxIn
txInParser =
  option
    ( eitherReader
        (Atto.parseOnly (parseTxInAtto <* Atto.endOfInput) . BSC.pack)
    )
    $ long "recover-tx-in"
      <> metavar "TXID#INDEX"
      <> help "Transaction input is UTxO TxId and a zero-based output index separated by a hash #."

parseTxInAtto :: Atto.Parser TxIn
parseTxInAtto =
  TxIn
    <$> parseTxIdAtto <* Atto.char '#'
    <*> parseTxIxAtto

parseTxIdAtto :: Atto.Parser TxId
parseTxIdAtto = (Atto.<?> "Transaction ID (hexadecimal)") $ do
  bstr <- Atto.takeWhile1 Char.isHexDigit
  case deserialiseFromRawBytesHex AsTxId bstr of
    Right addr -> return addr
    Left e -> fail $ docToString $ "Incorrect transaction id format: " <> prettyError e

parseTxIxAtto :: Atto.Parser TxIx
parseTxIxAtto = toEnum <$> Atto.decimal

outputFileParser :: Parser FilePath
outputFileParser =
  strOption
    ( long "output-file"
        <> metavar "FILE"
        <> value "tx.json"
        <> help "Output file path to write the transaction to."
    )

utxoParser :: Parser FilePath
utxoParser =
  strOption
    ( long "utxo-file"
        <> metavar "UTXO"
        <> help
          ( mconcat
              [ "Provide a path to a file containing the UTxO to deposit/recover. "
              , "You can use cardano-cli to query the UTxO set of an address and put the json output into a file directly. "
              , "cardano-cli query utxo --address $(cat path-to-address.addr) --mainnet --out-file utxo.json "
              ]
          )
    )

headIdParser :: Parser HeadId
headIdParser =
  UnsafeHeadId
    <$> strOption
      ( long "head-id"
          <> metavar "HEAD_ID"
          <> help "Unique Head id identifier."
      )

networkIdParser :: Parser NetworkId
networkIdParser = pMainnet <|> fmap Testnet pTestnetMagic
 where
  pMainnet :: Parser NetworkId
  pMainnet =
    flag'
      Mainnet
      ( long "mainnet"
          <> help "Use the mainnet magic id."
      )

  pTestnetMagic :: Parser NetworkMagic
  pTestnetMagic =
    NetworkMagic
      <$> option
        auto
        ( long "testnet-magic"
            <> metavar "NATURAL"
            <> value 42
            <> showDefault
            <> completer (listCompleter ["1", "2", "42"])
            <> help
              "Network identifier for a testnet to connect to. We only need to \
              \provide the magic number here. For example: '2' is the 'preview' \
              \network. See https://book.world.dev.cardano.org/environments.html for available networks."
        )

deadlineParser :: Parser UTCTime
deadlineParser =
  option
    (posixSecondsToUTCTime . fromInteger <$> auto)
    $ long "deadline"
      <> metavar "DEADLINE"
      <> help
        ( mconcat
            [ "Provide a deadline for the deposit transaction. "
            , "Deposit can only be included in L2 before this deadline or reclaimed after it passes. "
            , "Deadline is expressed as seconds since the Epoch (1970-01-01 00:00 UTC) "
            , "To easily obtain the deadline value you can use `date` command on linux. "
            , "`date --date '5 hours' +'%s'` -> 1725021571"
            ]
        )

lowerBoundSlotParser :: Parser SlotNo
lowerBoundSlotParser =
  option
    (SlotNo . fromInteger <$> auto)
    $ long "slot"
      <> metavar "LOWER_BOUND_SLOT"
      <> help
        ( mconcat
            [ "Provide a starting slot for the recover transaction. "
            , "This value could be obtained by querying the current slot number of the target network."
            , "The slot needs to be after the deadline which was set in the deposit transaction "
            , "in order for a recover transaction to be valid."
            ]
        )

-- | Parse command-line arguments into a `Option` or exit with failure and error message.
parseHydraCommand :: IO Command
parseHydraCommand = getArgs <&> parseHydraCommandFromArgs >>= handleParseResult

-- | Pure parsing of `Option` from a list of arguments.
parseHydraCommandFromArgs :: [String] -> ParserResult Command
parseHydraCommandFromArgs = execParserPure defaultPrefs hydraTxCommand

hydraTxCommand :: ParserInfo Command
hydraTxCommand =
  info
    ( commandParser
        <**> helper
    )
    ( fullDesc
        <> header "Hydra Tx"
        <> progDesc
          ( mconcat
              [ "Hydra Tx provides means of constructing the deposit/recover transactions on behalf of the user. "
              , "The deposit transaction is used to kick-off the incremental commit protocol."
              , "In case of any problems the recover transaction provides a way to unlock already locked UTxO"
              ]
          )
    )
