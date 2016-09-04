{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import BasicPrelude
import Data.Text (replace, unpack)

import Untest.Report.Text

main :: IO ()
main = getArgs >>= genReport . unpack . head >>= prettyPrint

prettyPrint :: Text -> IO ()
prettyPrint = putStrLn . replace "[PASS]" "[\x1b[32mPASS\x1b[0m]"
                       . replace "[FAIL]" "[\x1b[31mFAIL\x1b[0m]"
