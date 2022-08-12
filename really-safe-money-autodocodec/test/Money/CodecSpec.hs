module Money.CodecSpec (spec) where

import Autodocodec
import Autodocodec.Yaml
import Control.DeepSeq
import Control.Exception
import Data.Aeson.Types as JSON
import Money.Amount.Codec
import Money.Amount.Gen ()
import Money.AmountOf.Codec
import Money.AmountOf.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  codecSpec "amount" amountCodecViaString
  codecSpec "amount-of" amountOfCodecViaString

codecSpec :: (Show a, Eq a, GenValid a) => String -> JSONCodec a -> Spec
codecSpec name c = describe name $ do
  it "has the same schema as before" $
    pureGoldenTextFile (concat ["test_resources/", name, ".txt"]) (renderColouredSchemaVia c)

  it "never fails to encode" $
    forAllValid $ \a -> do
      val <- evaluate (force (toJSONVia c a))
      shouldBeValid val

  it "roundtrips to json" $
    forAllValid $ \a ->
      let encoded = toJSONVia c a
          errOrDecoded = JSON.parseEither (parseJSONVia c) encoded
       in case errOrDecoded of
            Left err ->
              expectationFailure $
                unlines
                  [ "Decoding failed with error",
                    err,
                    "instead of decoding to",
                    show a,
                    "'encode' encoded it to the json",
                    show encoded
                  ]
            Right decoded ->
              let ctx =
                    unlines
                      [ "Decoding succeeded, but the decoded value",
                        show decoded,
                        "differs from expected decoded value",
                        show a,
                        "'encode' encoded it to the json",
                        show encoded
                      ]
               in context ctx $ decoded `shouldBe` a
