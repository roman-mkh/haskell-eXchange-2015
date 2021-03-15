-- {-# LANGUAGE ApplicativeDo #-}
module BlogExampleCache2 where

import BlogDataSource2
import Haxl.Core
    ( dumpCacheAsHaskell,
      runHaxl,
      stateEmpty,
      stateSet,
      initEnv,
      GenHaxl, Env )

runDump :: GenHaxl () w a -> IO a
runDump h = do
  db <- initDataSource
  env2 <- initEnv (stateSet db stateEmpty) ()
  r <- runHaxl env2 h
  runHaxl env2 dumpCacheAsHaskell >>= putStr
  return r

runDumpExample5 :: IO PostContent
runDumpExample5 = runDump example5

{-- With ApplicativeDO
λ> runDumpExample5
select postid, content from postcontent where postid in ([2,1]);
loadCache :: GenHaxl u w ()
loadCache = do
  cacheRequest (FetchPostContent 1) (Right ("example content 1"))
  cacheRequest (FetchPostContent 2) (Right ("example content 2"))
--}
{-- Without ApplicativeDO
λ> runDumpExample5
select postid, content from postcontent where postid in ([1]);
select postid, content from postcontent where postid in ([2]);
loadCache :: GenHaxl u w ()
loadCache = do
  cacheRequest (FetchPostContent 1) (Right ("example content 1"))
  cacheRequest (FetchPostContent 2) (Right ("example content 2"))
--}


example5 :: GenHaxl u w PostContent
example5 = do
  a <- getPostContent 1
  b <- getPostContent 2
  return (a <> b)

myEnv :: IO (Env () w)
myEnv = do
  connection <- initDataSource
  initEnv (stateSet connection stateEmpty) ()

type Haxl w a = GenHaxl () w a

runExample :: Haxl w a -> IO a
runExample h = do
  env1 <- myEnv
  runHaxl env1 h

runExample5 :: IO PostContent
runExample5 = runExample example5

-- Without ApplicativeDo
{--
λ> runExample5
select postid, content from postcontent where postid in ([1]);
select postid, content from postcontent where postid in ([2]);
--}
-- With ApplicativeDo
-- select postid, content from postcontent where postid in ([2,1]);

example6 :: GenHaxl u w (PostContent, PostContent)
example6 = (,) <$> getPostContent 1 <*> getPostContent 2

example7 :: GenHaxl u w (PostContent, PostContent)
example7 = getPostContent 1 >>= (\x1 -> getPostContent 2 >>= (\x2 -> return (x1, x2)))