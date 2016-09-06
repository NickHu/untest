{-# LANGUAGE NoImplicitPrelude #-}
module Untest.Parser (
  Spec(spec),
  Bin(binName, tests),
  Test(testName, args, input, output),
  InMode(StdIn, FileIn, CmdIn),
  Args(unArgs),
  OutMode(StdOut, FileOut, CmdOut),
  parseSpec
) where

import BasicPrelude
import Data.Yaml 

newtype Spec = Spec {spec :: [Bin]} deriving Show
data Bin = Bin {binName :: Text, tests :: [Test]} deriving Show
data Test = Test {testName :: Text, args :: Args, input :: Maybe InMode, output :: OutMode} deriving Show
data InMode = StdIn Text | FileIn Text | CmdIn Text deriving Show
newtype Args = Args {unArgs :: [Text]} deriving Show
data OutMode = StdOut Text | FileOut Text | CmdOut Text deriving Show

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
                         v .:? "args" .!= Args [] <*>
                         v .:? "in" <*>
                         v .: "out"
  parseJSON _ = error "Can't parse Test from YAML"

instance FromJSON InMode where
  parseJSON (Object v) = asum
    [
      StdIn <$> v `parseObject` "stdin",
      FileIn <$> v .: "file",
      CmdIn <$> v .: "cmd"
    ]
  parseJSON _ = error "Can't parse InMode from YAML"

instance FromJSON Args where
  parseJSON (Array v) = Args <$> parseArray v

instance FromJSON OutMode where
  parseJSON (Object v) = asum
    [
      StdOut <$> v `parseObject` "stdout",
      FileOut <$> v .: "file",
      CmdOut <$> v .: "cmd"
    ]
  parseJSON _ = error "Can't parse OutMode from YAML"

parseObject :: Object -> Text -> Parser Text
parseObject v str = asum
  [
    v .: str,
    show <$> (v .: str :: Parser Integer),
    show <$> (v .: str :: Parser Double)
  ]

parseArray :: Array -> Parser [Text]
parseArray v = asum
  [
    parseJSON (Array v),
    map show <$> (parseJSON (Array v) :: Parser [Integer]),
    map show <$> (parseJSON (Array v) :: Parser [Double])
  ]

parseSpec :: ByteString -> Either ParseException Spec
parseSpec = decodeEither'
