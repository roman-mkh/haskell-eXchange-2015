{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module BlogDataSource2
  ( PostId, PostContent
  , getPostIds
  , getPostContent
  , initDataSource
  , BlogRequest(..)
 -- , BlogDBException(..)
  ) where


import Data.Hashable
import Data.Typeable
import qualified Data.Map as Map
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.List
import Haxl.Core
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
--import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as T
import TextShow
import Text.Printf as T

-- -----------------------------------------------------------------------------
-- Types

type PostId = Int
type PostContent = T.Text

-- type Request req a = (Eq (req a), Hashable (req a), Typeable (req a), Show (req a), Show a)
-- -----------------------------------------------------------------------------
-- Request type

data BlogRequest a where
  FetchPosts       :: BlogRequest [PostId]
  FetchPostContent :: PostId -> BlogRequest PostContent

deriving instance Show (BlogRequest a)
deriving instance Typeable BlogRequest

instance ShowP BlogRequest where showp = show

deriving instance Eq (BlogRequest a)

instance Hashable (BlogRequest a) where
  hashWithSalt salt FetchPosts = hashWithSalt salt (0::Int)
  hashWithSalt salt (FetchPostContent p) = hashWithSalt salt (1::Int, p)

-- -----------------------------------------------------------------------------
-- Requests

getPostIds :: GenHaxl u w [PostId]
getPostIds = dataFetch FetchPosts

getPostContent :: PostId -> GenHaxl u w PostContent
getPostContent = dataFetch . FetchPostContent

-- more operations ...

-- -----------------------------------------------------------------------------
-- Data source implementation

instance StateKey BlogRequest where
  data State BlogRequest = BlogDataState Connection

initDataSource :: IO (State BlogRequest)
initDataSource = BlogDataState <$> open "blog.sqlite"

instance DataSourceName BlogRequest where
  dataSourceName _ = "BlogDataSource"

instance DataSource u BlogRequest where
    -- Current state  Tracing flags User environment Fetch the data; see 'PerformFetch'.
    fetch (BlogDataState connection) _flags _userEnv =
      SyncFetch $ batchFetch connection

-- -----------------------------------------------------------------------------
-- Group requests by type

batchFetch :: Connection -> [BlockedFetch BlogRequest] -> IO ()
batchFetch connection blockedFetches = do
  batchFetchPosts connection batchForFetchPosts
  batchFetchContent connection batchForFetchContent
  where
    (batchForFetchPosts, batchForFetchContent) = mkBatches blockedFetches
  -- (***)


{--
batchFetch connection = mapM_ $ singleFetch connection


singleFetch :: Connection -> BlockedFetch BlogRequest -> IO ()
singleFetch connection (BlockedFetch FetchPosts resultVar) = do
  postIds <- sqlGetPostIds connection
  putSuccess resultVar postIds

singleFetch connection (BlockedFetch (FetchPostContent postId) resultVar) = do
  content <- sqlGetPostContent connection postId
  putSuccess resultVar content
--}

batchFetchPosts :: Connection -> [ResultVar [PostId]] -> IO ()
batchFetchPosts connection [] = return ()
batchFetchPosts connection batches = do
  postIds <- sqlGetPostIds connection
--  mapM_ (\resultVar -> putSuccess resultVar postIds) batches
  mapM_ (`putSuccess` postIds) batches

batchFetchContent :: Connection -> [(PostId, ResultVar PostContent)] -> IO ()
batchFetchContent connection [] = return ()
batchFetchContent connection batches = do
  -- T.putStrLn (showt postIds)
  resMap <- Map.fromList <$> sqlGetPostsContent connection postIds
  mapM_ (\(postId, resultVar) -> putSuccess resultVar (fromMaybe "" (Map.lookup postId resMap)) ) batches
  where postIds = fmap fst batches


type Batches
  = ( [ResultVar [PostId]]              -- FetchPosts
    , [(PostId, ResultVar PostContent)] -- FetchPostContent
    )

collect :: BlockedFetch BlogRequest -> Batches -> Batches
collect (BlockedFetch FetchPosts resultVar) (as,bs) = (resultVar:as,bs)
collect (BlockedFetch (FetchPostContent postId) resultVar) (as,bs) = (as,(postId,resultVar):bs)

mkBatches :: [BlockedFetch BlogRequest] -> Batches
mkBatches = foldr collect emptyBatches
  where
    emptyBatches :: Batches
    emptyBatches = ([],[])
-- -----------------------------------------------------------------------------
-- Fetch data for each batch

sqlGetPostIds :: Connection -> IO [Int]
sqlGetPostIds conn = do
  let q = "select postid from postinfo;" :: Query
  T.putStrLn $ fromQuery q
  r <- query_ conn q :: IO [[Int]]
  return $ mconcat r

sqlGetPostContent :: Connection -> PostId -> IO T.Text
sqlGetPostContent conn postId = do
  let q = "select content from postcontent where postid = ?;" :: Query
  T.putStrLn $ T.replace "?" (showt postId) (fromQuery q)
  [Only r] <- query conn q (Only postId) :: IO [Only T.Text]
  return r

sqlGetPostsContent :: Connection -> [PostId] -> IO [(Int, T.Text)] -- PostId
sqlGetPostsContent conn [] = return []
sqlGetPostsContent conn postIds =
  let qStrTemplate = "select postid, content from postcontent where postid in (X);"
      quests = T.intersperse ',' $  T.replicate (length postIds) "?"
      q = T.replace "X" quests qStrTemplate
      frmtQ = T.replace "X" (showt postIds) qStrTemplate in
  do
    T.putStrLn frmtQ
    query conn (Query q) postIds :: IO [(Int, T.Text)]