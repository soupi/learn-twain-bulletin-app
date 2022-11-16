{-# language OverloadedStrings #-}
{-# language OverloadedRecordDot #-}
{-# language LambdaCase #-}

-- | Database interaction
module DB (mkDB, DB(..)) where

import GHC.Stack (HasCallStack)
import qualified Data.Text as T
import qualified Database.Sqlite.Easy as DB
import Post

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
  DB.withPool pool runMigrations
  pure $ DB
    { getPost = getPostFromDb pool
    , getPosts = getPostsFromDb pool
    , insertPost = insertPostToDb pool
    , deletePostById = deletePostByIdFromDb pool
    }

-----------------------
-- * Database migrations

runMigrations :: HasCallStack => DB.SQLite ()
runMigrations = DB.migrate migrations migrateUp migrateDown

migrations :: [DB.MigrationName]
migrations =
  [ "posts"
  ]

migrateUp :: HasCallStack => DB.MigrationName -> DB.SQLite ()
migrateUp = \case
  "posts" -> do
    [] <- DB.run
      "create table posts(id integer primary key autoincrement, author text, title text, content text, time datetime default (datetime('now')))"
    pure ()
  name -> error $ "unexpected migration: " <> show name

migrateDown :: HasCallStack => DB.MigrationName -> DB.SQLite ()
migrateDown = \case
  "posts" -> do
    [] <- DB.run "DROP TABLE posts"
    pure ()
  name -> error $ "unexpected migration: " <> show name

-----------------------
-- * Database actions

getPostFromDb :: DB.Pool DB.Database -> DB.Int64 -> IO (DB.Int64, Post)
getPostFromDb pool id' =
  DB.withPool pool $
    fmap (decodeRow . head) $ uncurry DB.runWith (getPostSQL id')

getPostsFromDb :: DB.Pool DB.Database -> IO [(DB.Int64, Post)]
getPostsFromDb pool =
  DB.withPool pool $
    fmap (fmap decodeRow) $ DB.run getPostsSQL

insertPostToDb :: DB.Pool DB.Database -> Post -> IO DB.Int64
insertPostToDb pool post =
  DB.withPool pool $
    DB.transaction $ do
      [] <- uncurry DB.runWith (insertPostSQL post)
      [[DB.SQLInteger i]] <- DB.run getLastPostIdSQL
      pure i

deletePostByIdFromDb :: DB.Pool DB.Database -> DB.Int64 -> IO ()
deletePostByIdFromDb pool id' =
  DB.withPool pool $
    uncurry DB.runWith (deletePostSQL id') >>= \case
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
