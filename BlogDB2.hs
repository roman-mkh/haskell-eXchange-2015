{-# LANGUAGE OverloadedStrings #-}
module BlogDB2 where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (UTCTime, Day)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import TextShow

type Blog a = ReaderT Connection IO a

data PostInfo = PostInfo { postid :: Int, posttopic :: T.Text, postdate::UTCTime}
  deriving (Show)

instance FromRow PostInfo where
  fromRow = PostInfo <$> field <*> field <*> field

run :: Blog a -> IO a
run m = do
  db <- open "blog.sqlite"
  runReaderT m db

readAllContent :: IO [PostContent]
readAllContent = run $ getPostIds >>= mapM getPostContent
-- -----------------------------------------------------------------------------
-- An API

type PostId = Int
type PostContent = T.Text

getPostIds     :: Blog [PostId]
getPostContent :: PostId -> Blog PostContent
-- more operations...


-- -----------------------------------------------------------------------------
getPostIds = do
  conn <- ask
  liftIO $ do
    let q = "select postid from postinfo;" :: Query
    T.putStrLn $ fromQuery q
    r <- query_ conn q :: IO [[Int]]
    return $ mconcat r

getPostContent postId = do
  conn <- ask
  liftIO $ do
    let q = "select content from postcontent where postid = ?;" :: Query
    T.putStrLn $ T.replace "?" (showt postId) (fromQuery q)
    -- r <- query conn "select content from postcontent where postid = ?" (Only (1::Int)) :: IO [Only T.Text]
    -- return $ fromMaybe "" (listToMaybe r)
    [Only r] <- query conn q (Only postId) :: IO [Only T.Text]
    return r