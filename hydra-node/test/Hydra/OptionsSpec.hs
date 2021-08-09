module Hydra.OptionsSpec where

import Hydra.Network (Host (Host), MockChainPorts (..))
import Hydra.Options (Options (..), ParserResult (..), defaultOptions, parseHydraOptionsFromString)
import Hydra.Prelude
import Hydra.Test.Prelude (failure)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Hydra Node Options" $ do
  it "parses --host option given valid IPv4 and IPv6 addresses" $ do
    ["--host", "127.0.0.1"]
      `shouldParse` defaultOptions{host = "127.0.0.1"}
    ["--host", "2001:db8:11e:c00::101"]
      `shouldParse` defaultOptions{host = "2001:db8:11e:c00::101"}
    ["--host", "0.0.0.0"]
      `shouldParse` defaultOptions{host = "0.0.0.0"}
    shouldNotParse ["--host", "0.0.0"]
    shouldNotParse ["--host", "2001:db8:11e:c00:101"]

  it "parses --port option given valid port number" $ do
    ["--port", "12345"]
      `shouldParse` defaultOptions{port = 12345}
    shouldNotParse ["--port", "123456"]
    ["--port", "0"]
      `shouldParse` defaultOptions{port = 0}
    shouldNotParse ["--port", "-42"]

  it "parses --peer <host>@<port> option" $ do
    ["--peer", "1.2.3.4@4567"]
      `shouldParse` defaultOptions{peers = [Host "1.2.3.4" 4567]}
    ["--peer", ":::1@4567"]
      `shouldParse` defaultOptions{peers = [Host ":::1" 4567]}
    ["--peer", "1.2.3.4@4567", "--peer", "1.2.3.5@4568"]
      `shouldParse` defaultOptions{peers = [Host "1.2.3.4" 4567, Host "1.2.3.5" 4568]}
    ["--peer", "foo.com@4567"]
      `shouldParse` defaultOptions{peers = [Host "foo.com" 4567]}
    shouldNotParse ["--peer", "foo.com@456789"]

  it "parses --monitoring-port option given valid port number" $ do
    []
      `shouldParse` defaultOptions{monitoringPort = Nothing}
    ["--monitoring-port", "12345"]
      `shouldParse` defaultOptions{monitoringPort = Just 12345}

  it "parses --version flag as a parse error" $
    shouldNotParse ["--version"]

  it "parses --party option as a filepath" $ do
    ["--party", "./alice.vk"] `shouldParse` defaultOptions{parties = ["./alice.vk"]}
    ["--party", "/foo"] `shouldParse` defaultOptions{parties = ["/foo"]}
    ["--party", "bar"] `shouldParse` defaultOptions{parties = ["bar"]}
    ["--party", "alice.vk", "--party", "bob.vk"]
      `shouldParse` defaultOptions{parties = ["alice.vk", "bob.vk"]}

  it "parses --me option as a filepath" $
    ["--me", "./alice.sk"] `shouldParse` defaultOptions{me = "./alice.sk"}

  it "parses --mock-chain-ports option as a list of ports to connect to" $
    ["--mock-chain-ports", "(1,2,3)"] `shouldParse` defaultOptions{mockChainPorts = MockChainPorts (1, 2, 3)}

shouldParse :: [String] -> Options -> Expectation
shouldParse args options =
  case parseHydraOptionsFromString args of
    Success a -> a `shouldBe` options
    err -> failure (show err)

shouldNotParse :: [String] -> Expectation
shouldNotParse args =
  case parseHydraOptionsFromString args of
    Success a -> failure $ "Unexpected successful parse to " <> show a
    Failure _ -> pure ()
    CompletionInvoked _ -> failure "Unexpected completion invocation"
