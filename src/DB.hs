{-# language OverloadedStrings #-}
{-# language OverloadedRecordDot #-}
{-# language LambdaCase #-}

-- | Database interaction
module DB (mkDB, DB(..)) where

import qualified Data.Text as T
import qualified Database.Sqlite.Easy as DB
import Post
import Control.Monad ((>=>))
import GHC.Stack (HasCallStack)

-----------------------
-- * Database handler API

data DB
  = DB
    { getPost :: DB.Int64 -> IO (DB.Int64, Post)
    , getPosts :: IO [(DB.Int64, Post)]
    , insertPost :: Post -> IO DB.Int64
    , deletePostById :: DB.Int64 -> IO ()
    }

-----------------------
-- * Handler smart constructor

mkDB :: DB.ConnectionString -> IO DB
mkDB connstr = do
  pool <- DB.createSqlitePool connstr
  DB.withResource pool runMigrations
  pure $ DB
    { getPost = getPostFromDb pool
    , getPosts = getPostsFromDb pool
    , insertPost = insertPostToDb pool
    , deletePostById = deletePostByIdFromDb pool
    }

-----------------------
-- * Database migrations

runMigrations :: HasCallStack => DB.Database -> IO ()
runMigrations = DB.migrate migrations migrateUp migrateDown

migrations :: [DB.MigrationName]
migrations =
  [ "posts"
  ]

migrateUp :: HasCallStack => DB.MigrationName -> DB.Database -> IO ()
migrateUp name conn =
  case name of
    "posts" -> do
      [] <- DB.run
        "create table posts(id integer primary key autoincrement, author text, title text, content text, time datetime default (datetime('now')))"
        conn
      pure ()

migrateDown :: HasCallStack => DB.MigrationName -> DB.Database -> IO ()
migrateDown name conn =
  case name of
    "posts" -> do
      [] <- DB.run
        "DROP TABLE posts"
        conn
      pure ()

-----------------------
-- * Database actions

getPostFromDb :: DB.Pool DB.Database -> DB.Int64 -> IO (DB.Int64, Post)
getPostFromDb pool id' =
  DB.withResource pool $
    fmap (decodeRow . head) . uncurry DB.runWith (getPostSQL id')

getPostsFromDb :: DB.Pool DB.Database -> IO [(DB.Int64, Post)]
getPostsFromDb pool =
  DB.withResource pool $
    fmap (fmap decodeRow) . DB.run getPostsSQL

insertPostToDb :: DB.Pool DB.Database -> Post -> IO DB.Int64
insertPostToDb pool post =
  DB.withResource pool $ \db ->
    DB.asTransaction db $ do
      [] <- uncurry DB.runWith (insertPostSQL post) db
      [[DB.SQLInteger i]] <- DB.run getLastPostIdSQL db
      pure i

deletePostByIdFromDb :: DB.Pool DB.Database -> DB.Int64 -> IO ()
deletePostByIdFromDb pool id' =
  DB.withResource pool $
    uncurry DB.runWith (deletePostSQL id') >=> \case
      [] -> pure ()
      rs -> error (show rs)

-----------------------
-- ** SQL

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

-----------------------
-- ** Decode row

decodeRow :: [DB.SQLData] -> (DB.Int64, Post)
decodeRow row =
  case row of
    [DB.SQLInteger i, DB.SQLText dtbs, DB.SQLText author, DB.SQLText title, DB.SQLText content] ->
      (i, Post (read $ T.unpack dtbs) author title content)
    _ -> error $ show row
