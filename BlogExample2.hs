module BlogExample2
  ( runExample1
  , runExample2
  , runExample3
  , runExample4
  ) where

import BlogDataSource2
import Haxl.Core
import qualified Data.Text as T

example1 :: GenHaxl u w (PostContent, PostContent)
example1 = (,) <$> getPostContent 1 <*> getPostContent 2

example2 :: GenHaxl u w T.Text
example2 = do
  a <- getPostContent 1
  b <- getPostContent 2
  return (a <> b)

example3 :: GenHaxl u w T.Text
example3 = do
  a <- getPostContent 1
  b <- if T.length a > 10 then getPostContent 3 else getPostContent 4
  c <- getPostContent 2
  return (mconcat [a,b,c])


example4 :: GenHaxl u w T.Text
example4 = do
  a <- getPostContent 1
  b <- if T.length a > 10 then getPostContent 3 else getPostContent 4
  c <- getPostContent 2
  d <- if T.length c > 10 then getPostContent 5 else getPostContent 6
  return (mconcat [a,b,c,d])

myEnv :: IO (Env () w)
myEnv = do
  connection <- initDataSource
  initEnv (stateSet connection stateEmpty) ()

type Haxl w a = GenHaxl () w a

runExample :: Haxl w a -> IO a
runExample h = do
  env1 <- myEnv
  runHaxl env1 h

runExample1 :: IO (PostContent, PostContent)
runExample1 = runExample example1

runExample2 :: IO T.Text
runExample2 = runExample example2

runExample3 :: IO T.Text
runExample3 = runExample example3

runExample4 :: IO T.Text
runExample4 = runExample example4