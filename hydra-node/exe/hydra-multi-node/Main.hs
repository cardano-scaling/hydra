{-# LANGUAGE TypeApplications #-}

import Hydra.Prelude

import Hydra.Node.MultiHeaded (runServer, withNode)
import Hydra.Options (RunOptions (..), runOptionsParser)
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)

optionsInfo :: ParserInfo RunOptions
optionsInfo =
  info
    ( runOptionsParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Starts a Hydra Multi-Headed Node"
        <> header "hydra-multi-node - An experimental node to manage multiple heads"
    )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  opts@RunOptions{apiPort} <- execParser optionsInfo
  print @Text $ "Starting " <> show opts
  withNode opts (runServer apiPort)
