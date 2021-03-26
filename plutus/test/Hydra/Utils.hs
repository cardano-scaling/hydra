module Hydra.Utils where

import Data.Maybe
import Data.String
import Data.Text.Prettyprint.Doc
import PlutusTx
import PlutusTx.Prelude
import Test.Tasty
import Test.Tasty.Golden

checkCompiledContractPIR :: FilePath -> CompiledCode a -> TestTree
checkCompiledContractPIR path code = goldenVsString "PIR" path (return $ fromString $ show $ pretty $ fromJust $ getPir code)
