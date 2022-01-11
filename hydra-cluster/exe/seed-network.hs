{-# LANGUAGE TypeApplications #-}

import CardanoClient (
  buildAddress,
  postSeedPayment,
  queryProtocolParameters,
  queryUtxo,
 )
import CardanoCluster (asSigningKey, availableInitialFunds, defaultNetworkId)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Ledger.Cardano (Lovelace, getVerificationKey)
import Hydra.Prelude
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  showDefault,
  strOption,
  value,
 )

data Options = Options
  { cardanoNodeSocket :: FilePath
  , cardanoSigningKey :: FilePath
  , amountLovelace :: Lovelace
  }

seedNetworkOptionsParser :: Parser Options
seedNetworkOptionsParser =
  Options
    <$> strOption
      ( long "node-socket"
          <> metavar "FILE"
          <> help "The path to the Cardano node domain socket for client communication."
          <> value "node.socket"
          <> showDefault
      )
    <*> strOption
      ( long "cardano-signing-key"
          <> metavar "FILE"
          <> help "The path to the signing key file used for committing UTxO. This file used the same 'Envelope' format than cardano-cli."
          <> value "me.sk"
          <> showDefault
      )
    <*> ( fromIntegral @Integer
            <$> option
              auto
              ( long "commit-amount"
                  <> metavar "LOVELACE"
                  <> help "The amount of Lovelaces in the generated UTXO used for committing."
              )
        )

seedNetworkOptions :: ParserInfo Options
seedNetworkOptions =
  info
    (seedNetworkOptionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Post a transaction on a fresh testnet to create  seed payment and some UTXO to commit."
        <> header "seed-network - Seed UTXO on demo chain"
    )

main :: IO ()
main = do
  Options{cardanoNodeSocket, cardanoSigningKey, amountLovelace} <- execParser seedNetworkOptions
  putStrLn $ "Querying node for Protocol Parameters at " <> cardanoNodeSocket
  pparams <- queryProtocolParameters networkId cardanoNodeSocket
  signingKey <- readFileTextEnvelopeThrow asSigningKey cardanoSigningKey
  putStrLn $ "Posting seed payment transaction at " <> cardanoNodeSocket <> ", amount: " <> show amountLovelace <> ", key: " <> cardanoSigningKey
  postSeedPayment networkId pparams initialAmount cardanoNodeSocket signingKey amountLovelace
  let address = buildAddress (getVerificationKey signingKey) networkId
  putStrLn $ "UTXO for address " <> show address
  queryUtxo networkId cardanoNodeSocket [address] >>= putTextLn . decodeUtf8 . encodePretty
 where
  networkId = defaultNetworkId
  initialAmount = availableInitialFunds
