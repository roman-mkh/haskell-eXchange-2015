module LogExample where

import LogDataSource2
import BlogDataSource2
import Haxl.Core
import TextShow

type Haxl w a = GenHaxl () w a

run :: Haxl w a -> IO a
run h = do
  log <- LogDataSource2.initDataSource
  db <- BlogDataSource2.initDataSource
  env <- initEnv (stateSet db (stateSet log stateEmpty)) ()
  runHaxl env h

-- runBlogLog :: IO ()
runBlogLog :: IO ()
runBlogLog = run $ getPostIds >>= mapM getPostContent >>= mapM_ (writeLog . showt)
-- runBlogLog = run $ getPostIds >>= mapM getPostContent >>= mapM (writeLog . showt) >>= \_ -> return ()