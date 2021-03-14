{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module LogDataSource2
  ( writeLog
  , initDataSource
  , LogRequest(..)
  ) where


import Data.Hashable
import Data.Typeable
import Haxl.Core
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- -----------------------------------------------------------------------------
-- Request type

data LogRequest a where
  WriteLog :: T.Text -> LogRequest ()

deriving instance Show (LogRequest a)
deriving instance Typeable LogRequest

instance ShowP LogRequest where showp = show

deriving instance Eq (LogRequest a)

instance Hashable (LogRequest a) where
  hashWithSalt salt (WriteLog str) = hashWithSalt salt str


writeLog :: T.Text -> GenHaxl u w ()
writeLog = uncachedRequest . WriteLog

instance StateKey LogRequest where
  data State LogRequest  = LogDataState

initDataSource :: IO (State LogRequest)
initDataSource = return LogDataState

instance DataSourceName LogRequest where
  dataSourceName _ = "LogDataSource"

instance DataSource u LogRequest where
  -- State LogRequest -> Flags -> u -> PerformFetch LogRequest
  fetch _state _flags _userEnv = SyncFetch doAll
    where
      doAll :: [BlockedFetch LogRequest] -> IO ()
      doAll requests = mapM_ doOne requests

      doOne :: BlockedFetch LogRequest -> IO ()
      doOne (BlockedFetch (WriteLog str) var) = do
        T.putStrLn $ "LOG: " <> str
        putSuccess var ()