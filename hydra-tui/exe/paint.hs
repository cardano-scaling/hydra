data Pixel = Pixel
  { x, y, red, green, blue :: Word8
  }

main :: IO ()
main = do
  key <- lookupEnv "HYDRA_SIGNING_KEY"
  networkId <- lookupEnv "NETWORK_ID"
  args <- getArgs
  case read <$> args of
    [x, y, red, green, blue] -> submitTx key networkId (Pixel{x, y, red, green, blue})
    _ -> error "Expecting 5 word8 arguments: x,y,red/green/blue"

submitTx :: FilePath -> NetworkId -> Host -> Pixel -> IO ()
submitTx key pixel host = do

  case s ^? headStateL of
    Just Open{utxo = UTxO u'} ->
      let myAddress = mkVkAddress networkId vk
       in Map.filter (\(TxOut addr _ _) -> addr == myAddress) u'
    _ ->

  utxo <- liftIO $ queryUTxOByAddress [buildAddress (getVerificationKey sk) networkId]
  let oneUtxoWithoutFuel = head $ Map.filter (not . isMarkedOutput) (UTxO.toMap utxo)
  mkSimpleTx input (recipient, lovelaceToValue $ Lovelace amount) sk


withClient :: HasCallStack => Host -> (Connection -> IO ()) -> IO ()
withClient Host{hostname, portNumber}  action = do
  failAfter 5 retry
 where
  retry = runClient hostname portNumber "/" action `catch` \(_ :: IOException) -> retry
