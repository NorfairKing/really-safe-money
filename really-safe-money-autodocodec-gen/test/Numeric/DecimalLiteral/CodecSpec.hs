{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.DecimalLiteral.CodecSpec (spec) where

import Data.Aeson.Types as JSON
import Money.Autodocodec.Gen
import Numeric.DecimalLiteral (DecimalLiteral (..))
import Numeric.DecimalLiteral.Codec as DecimalLiteral
import Numeric.DecimalLiteral.Gen ()
import Test.Syd

spec :: Spec
spec = do
  -- 2^64 is 18446744073709551616
  describe "DecimalLiteral" $ do
    codecSpec @DecimalLiteral "decimal-literal" "string" DecimalLiteral.codecViaString
    parseFailSpec DecimalLiteral.codecViaString (String "three")
    parseFailMessageSpec @DecimalLiteral DecimalLiteral.codecViaString (String "not-a-number") "Error in $: Could not read string as a DecimalLiteral: not-a-number"
    parseFailSpec DecimalLiteral.codecViaString (String "0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
    parseSuccessSpec DecimalLiteral.codecViaString (String "1") (DecimalLiteral Nothing 1 0)
    parseSuccessSpec DecimalLiteral.codecViaString (String "18446744073709551617") (DecimalLiteral Nothing 18446744073709551617 0)
