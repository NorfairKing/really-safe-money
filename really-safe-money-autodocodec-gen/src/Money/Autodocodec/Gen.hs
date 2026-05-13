{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Money.Autodocodec.Gen
  ( codecSpec,
    parseSuccessSpec,
    parseFailSpec,
    parseFailMessageSpec,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.DeepSeq
import Control.Exception
import Data.Aeson.Types as JSON
import GHC.Stack
import Test.Syd
import Test.Syd.Validity

codecSpec ::
  forall a.
  (Show a, Eq a, GenValid a) =>
  String ->
  String ->
  JSONCodec a ->
  Spec
codecSpec name dir c = describe name $ do
  it "has the same schema as before" $
    pureGoldenTextFile (concat ["test_resources/", name, "/", dir, ".txt"]) (renderColouredSchemaVia c)

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

parseSuccessSpec :: (HasCallStack) => (Show a, Eq a) => JSONCodec a -> JSON.Value -> a -> Spec
parseSuccessSpec c v expected = withFrozenCallStack $
  it (unwords ["fails to parse", show v]) $
    case JSON.parseEither (parseJSONVia c) v of
      Left err -> expectationFailure $ unlines ["Failed to parse:", err]
      Right actual -> actual `shouldBe` expected

parseFailSpec :: (HasCallStack) => (Show a) => JSONCodec a -> JSON.Value -> Spec
parseFailSpec c v = withFrozenCallStack $
  it (unwords ["fails to parse", show v]) $
    case JSON.parseEither (parseJSONVia c) v of
      Left _ -> pure ()
      Right a -> expectationFailure $ unlines ["Should have failed to decode, but got", ppShow a]

parseFailMessageSpec :: forall a. (HasCallStack, Show a) => JSONCodec a -> JSON.Value -> String -> Spec
parseFailMessageSpec c v expectedErr =
  withFrozenCallStack $
    it (unwords ["fails to parse", show v, "with the expected error message"]) $
      case JSON.parseEither (parseJSONVia c) v :: Either String a of
        Left err -> err `shouldBe` expectedErr
        Right a -> expectationFailure $ unlines ["Should have failed to decode, but got", ppShow a]
