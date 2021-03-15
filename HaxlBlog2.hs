{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module HaxlBlog2 (
    PostId, PostContent,
    getPostIds, getPostContent,
    Haxl,
    run,
    runBlog
  ) where


import BlogDataSource2
import Haxl.Core

type Haxl w a = GenHaxl () w a

myEnv :: IO (Env () w)
myEnv = do
  connection <- initDataSource
  initEnv (stateSet connection stateEmpty) ()

run :: Haxl w a -> IO a
run h = do
  env1 <- myEnv
  runHaxl env1 h

runBlog :: IO [PostContent]
runBlog = run $ getPostIds >>= mapM getPostContent