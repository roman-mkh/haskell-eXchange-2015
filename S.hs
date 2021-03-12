{-# LANGUAGE OverloadedStrings #-}

module S where

import Control.Monad.Trans.Reader
import qualified Data.Text as T
import Data.Text.Read as R ( decimal )
import Data.Either ( fromRight )
import Data.Text.IO as TIO
import Text.Printf ( printf )

getHost' :: T.Text -> T.Text
getHost' = T.takeWhile (/= ':')

getPort' :: T.Text -> Int
getPort' hp = either (const 0) fst (decimal port)
    where port = T.takeWhileEnd (/= ':') hp
-- getPort' hp = fromRight 0 $ fmap fst (decimal hp)


getHost :: Reader T.Text T.Text
getHost = getHost' <$> ask

getPort :: Reader T.Text Int
getPort = getPort' <$> ask

myHost :: T.Text
myHost = "myHost:8080"

printConfig :: Reader T.Text (IO ())
printConfig = do
    -- host <- (reader . runReader) getHost
    -- host <- getHost
    host <- asks getHost'
    -- port <- getPort
    printf "Host: '%s' Port: '%i'\n" host <$> getPort

printHost :: T.Text -> IO ()
printHost = runReader printConfig

h1 :: IO ()
h1 = printHost myHost

h2 :: IO ()
h2 = printHost "www.nemesis.ch:8091"