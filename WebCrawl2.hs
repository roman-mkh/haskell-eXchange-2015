{-# LANGUAGE
    OverloadedStrings
 #-}
module WebCrawl2 where

import HttpDataSource2
import Haxl.Core
import Control.Monad
-- import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as L
import Data.Text.Encoding
import Data.Text.Encoding.Error
import qualified Data.Text as Text
import Data.List
import Network.URI
import Debug.Trace
import qualified Data.HashSet as HashSet

type Haxl w a = GenHaxl () w a

runHttp :: Haxl w a -> IO a
runHttp h = do
  db <- initDataSource
  env <- initEnv (stateSet db stateEmpty) ()
  runHaxl env h

urls :: [Text.Text]
urls =
  [ "http://dev.stephendiehl.com/"
  , "http://bio.acousti.ca/"
  , "http://dev.stephendiehl.com/"
  ]

example1 :: IO [L.ByteString]
example1 = runHttp $ mapM getURL urls
{--
example2 = runHttp $ mapM_ getURL urls >>= \_ -> dumpCacheAsHaskell

crawl :: String -> Haxl ()
crawl root = go HashSet.empty [root]
 where
  go seen [] = return ()
  go seen (url : queue)
    | not (root `isPrefixOf` url) || url `HashSet.member` seen = go seen queue
    | Debug.Trace.trace url False = undefined
    | otherwise = do
        page <- getURL url
        go (HashSet.insert url seen) (hrefs url page ++ queue)

hrefs :: String -> L.ByteString -> [String]
hrefs base page
  | Just base <- parseURI base
  = HashSet.toList $ HashSet.fromList $
    [ show (url{uriQuery="",uriFragment=""} `relativeTo` base)
    | TagOpen "a" attrs <- parseTags (Text.unpack (decodeUtf8With lenientDecode (L.toStrict page))),
      ("href",str) <- attrs,
      Just url <- [parseURIReference str]
      ]
  | otherwise
  = []
--}