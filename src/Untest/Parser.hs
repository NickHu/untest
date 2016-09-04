{-# LANGUAGE NoImplicitPrelude #-}
module Untest.Parser (
  Spec(spec),
  Bin(binName, tests),
  Test(testName, input, output),
  Mode(None, Std, File),
  parseSpec
) where

import BasicPrelude
import Data.Yaml 

data Spec = Spec {spec :: [Bin]} deriving Show
data Bin = Bin {binName :: Text, tests :: [Test]} deriving Show
data Test = Test {testName :: Text, input :: Mode, output :: Mode} deriving Show
data Mode = None | Std Text | File Text deriving Show

instance FromJSON Spec where
  parseJSON (Object v) = Spec <$>
                         v .: "spec"
  parseJSON _ = error "Can't parse Spec from YAML"

instance FromJSON Bin where
  parseJSON (Object v) = Bin <$>
                         v .: "bin" <*>
                         v .: "tests"
  parseJSON _ = error "Can't parse Bin from YAML"

instance FromJSON Test where
  parseJSON (Object v) = Test <$>
                         v .: "name" <*>
                         v .: "in" <*>
                         v .: "out"
  parseJSON _ = error "Can't parse Test from YAML"

instance FromJSON Mode where
  parseJSON (Object v) = asum
    [
      Std <$> (convert "stdin" <|> convert "stdout"),
      File <$> v .: "file",
      return None
    ]
    where
      convert :: Text -> Parser Text
      convert str = asum
            [
              v .: str,
              show <$> (v .: str :: Parser Integer),
              show <$> (v .: str :: Parser Double)
            ]
  parseJSON _ = error "Can't parse Mode from YAML"

parseSpec :: ByteString -> Either ParseException Spec
parseSpec = decodeEither'
