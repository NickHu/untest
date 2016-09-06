{-# LANGUAGE NoImplicitPrelude #-}
module Untest.Test (
  runSpec,
  loadSpec,
  Outcome(Pass, Fail, Timeout, Error)
) where

import BasicPrelude
import Control.Concurrent.Async
import Control.Monad.STM
import qualified STMContainers.Map as HM
import qualified Data.ByteString as BS
import Data.Text
import Data.Yaml (prettyPrintParseException)
import System.FilePath
import System.Process

import Untest.Parser

data Outcome = Pass | Fail | Timeout | Error
data Context = Context { path :: FilePath, verbose :: Bool }

runSpec :: FilePath -> IO (HM.Map Text Outcome)
runSpec f = do
  let ctx = Context (takeDirectory f) True
  s <- loadSpec f
  m <- HM.newIO
  mapConcurrently (runBin ctx m) $ spec s
  return m

loadSpec :: FilePath -> IO Spec
loadSpec f = BS.readFile f >>= either
    (\x -> putStrLn (pack $ prettyPrintParseException x) >> error "parser failed")
    pure
  . parseSpec

runBin :: Context -> HM.Map Text Outcome -> Bin -> IO ()
runBin ctx m bin = mapM_ (runTest m) $ tests bin
  where
    runTest :: HM.Map Text Outcome -> Test -> IO ()
    runTest m t = do
      stdin <- case input t of
                 Just (StdIn i) -> return i
                 Just (FileIn i) -> readFile $ unpack i
                 Just (CmdIn c) -> error "TODO: implement"
                 Nothing -> return ""
      (exit, stdout, stderr) <- readProcessWithExitCode
        (path ctx </> unpack (binName bin))
        (unpack <$> unArgs (args t))
        (unpack stdin)
      expected <- case output t of
                    StdOut o -> return o
                    FileOut o -> readFile $ unpack o
                    CmdOut o -> error "TODO: implement"
      if strip (pack stdout) == expected
        then atomically (HM.insert Pass (testName t) m)
        else atomically (HM.insert Fail (testName t) m)

