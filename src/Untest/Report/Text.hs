{-# LANGUAGE NoImplicitPrelude #-}
module Untest.Report.Text (
  genReport,
  printReport
) where

import BasicPrelude
import Control.Monad.STM
import ListT
import qualified STMContainers.Map as HM

import Untest.Test

genReport :: FilePath -> IO Text
genReport f = do
  m <- runSpec f
  atomically $ fold (\acc x -> pure (acc ++ uncurry testReport x ++ "\n")) "" (HM.stream m)

testReport :: Text -> Outcome -> Text
testReport t Pass = "[PASS] " ++ t
testReport t _ = "[FAIL] " ++ t

printReport :: FilePath -> IO ()
printReport f = genReport f >>= putStrLn
