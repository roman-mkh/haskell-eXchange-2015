{-# LANGUAGE OverloadedStrings #-}
module GetURL2 where

import Network.HTTP.Client
import Data.List
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.IO
import Data.Char
import Control.Applicative
-- import Text.XML.Light
import System.Environment
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Network.HTTP.Client.TLS

getURL :: T.Text -> IO T.Text
getURL url = do
  let request = parseRequest_ $ T.unpack url
  manager <- newManager tlsManagerSettings -- defaultManagerSettings
  lbs <- responseBody <$> httpLbs request manager
  return $ T.decodeUtf8 (B.concat (L.toChunks lbs))

testGetURL :: IO T.Text
testGetURL = getURL "http://dev.stephendiehl.com/"
