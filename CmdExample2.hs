{-# LANGUAGE OverloadedStrings #-}

module CmdExample2 where

import CmdDataSource2
import Haxl.Core

type Haxl w a = GenHaxl () w a

run :: Haxl w a -> IO a
run h = do
  st <- CmdDataSource2.initDataSource
  env <- initEnv (stateSet st stateEmpty) ()
  runHaxl env h

runls :: IO ()
runls = run $ cmd "ls" ["-la"]