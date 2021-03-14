{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module HttpDataSource2
  ( getURL
  , initDataSource
  , HttpException(..)
  ) where

import Data.Hashable
import Data.Typeable
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Data.List
import Haxl.Core
import Control.Exception
import Control.Concurrent.Async
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Text.Printf
import qualified Data.Text as T

-- -----------------------------------------------------------------------------
-- Request type

data HttpRequest a where
  GetURL :: T.Text -> HttpRequest L.ByteString

deriving instance Show (HttpRequest a)
deriving instance Typeable HttpRequest

instance ShowP HttpRequest where showp = show

deriving instance Eq (HttpRequest a)

instance Hashable (HttpRequest a) where
  hashWithSalt salt (GetURL u) = hashWithSalt salt u

-- -----------------------------------------------------------------------------
-- Requests

getURL :: T.Text  -> GenHaxl u () L.ByteString
getURL = dataFetch . GetURL

instance StateKey HttpRequest where
  data State HttpRequest = HttpState Manager

initDataSource :: IO (State HttpRequest)
initDataSource = HttpState <$> newManager tlsManagerSettings

instance DataSourceName HttpRequest where
  dataSourceName _ = "HttpDataSource"

instance DataSource u HttpRequest where
  -- fetch :: State HttpRequest -> Flags -> u -> PerformFetch HttpRequest
  fetch (HttpState manager) _flags _userEnv =
    SyncFetch $ batchFetch manager

  -- fetch (HttpState mgr) _flags _userEnv blockedFetches = undefined
  -- fetch _state _flags _userEnv blockedFetches = undefined
--    SyncFetch $ do
--       printf "Fetching %d urls.\n" (length blockedFetches)
--       void $ mapConcurrently (fetchURL mgr) blockedFetches

batchFetch :: Manager -> [BlockedFetch HttpRequest] -> IO ()
batchFetch manager blockedFetches = do
  printf "Fetching %d urls.\n" (length blockedFetches)
  void $ mapConcurrently (fetchURL manager) blockedFetches

fetchURL :: Manager -> BlockedFetch HttpRequest -> IO ()
fetchURL mgr (BlockedFetch (GetURL url) var) = do
  e <- Control.Exception.try $ do
    let request = parseRequest_ $ T.unpack url
    responseBody <$> httpLbs request mgr
  either (putFailure var) (putSuccess var)
    (e :: Either SomeException L.ByteString)

