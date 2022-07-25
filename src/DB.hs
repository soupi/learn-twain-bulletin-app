{-# language OverloadedStrings #-}
{-# language OverloadedRecordDot #-}
{-# language LambdaCase #-}

-- | Database interaction
module DB (mkDB, DB(..)) where

import qualified Data.Text as T
import qualified Database.Sqlite.Easy as DB
import Post
import Control.Monad ((>=>))

-- ** Database

data DB
  = DB
    { getPost :: DB.Int64 -> IO (DB.Int64, Post)
    , getPosts :: IO [(DB.Int64, Post)]
    , insertPost :: Post -> IO DB.Int64
    , deletePostById :: DB.Int64 -> IO ()
    }

mkDB :: DB.ConnectionString -> IO DB
mkDB connstr = do
  pool <- DB.createSqlitePool connstr
  -- [] <- DB.withResource pool $ DB.run setupSQL
  pure $ DB
    { getPost = \i ->
      DB.withResource pool $
        fmap (decodeRow . head) . uncurry DB.runWith (getPostSQL i)

    , getPosts =
      DB.withResource pool $
        fmap (fmap decodeRow) . DB.run getPostsSQL

    , insertPost = \post ->
      DB.withResource pool $ \db ->
        DB.asTransaction db $ do
          [] <- uncurry DB.runWith (insertPostSQL post) db
          [[DB.SQLInteger i]] <- DB.run getLastPostIdSQL db
          pure i

    , deletePostById = \i ->
      DB.withResource pool $
        uncurry DB.runWith (deletePostSQL i) >=> \case
          [] -> pure ()
          rs -> error (show rs)
    }

decodeRow :: [DB.SQLData] -> (DB.Int64, Post)
decodeRow row =
  case row of
    [DB.SQLInteger i, DB.SQLText dtbs, DB.SQLText author, DB.SQLText title, DB.SQLText content] ->
      (i, Post (read $ T.unpack dtbs) author title content)
    _ -> error $ show row

insertPostSQL :: Post -> (DB.SQL, [DB.SQLData])
insertPostSQL post =
  ( "insert into posts(author, title, content) values (?,?,?)"
  , [DB.SQLText post.pAuthor, DB.SQLText post.pTitle, DB.SQLText post.pContent]
  )

getLastPostIdSQL :: DB.SQL
getLastPostIdSQL =
  "select id from posts order by id desc limit 1"

getPostsSQL :: DB.SQL
getPostsSQL =
  "select id, time, author, title, content from posts order by time desc"

getPostSQL :: DB.Int64 -> (DB.SQL, [DB.SQLData])
getPostSQL i =
  ("select id, time, author, title, content from posts where id = ?", [DB.SQLInteger i])

deletePostSQL :: DB.Int64 -> (DB.SQL, [DB.SQLData])
deletePostSQL i =
  ("delete from posts where id = ?", [DB.SQLInteger i])
