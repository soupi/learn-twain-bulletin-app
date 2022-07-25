{-# language OverloadedStrings #-}
{-# language OverloadedRecordDot #-}

-- | A bulletin board app built with twain.
module Bulletin where

import qualified Web.Twain as Twain
import Network.Wai.Handler.Warp (run, Port)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import qualified Lucid as H
import qualified Network.Wai.Middleware.RequestLogger as Logger
import qualified Database.Sqlite.Easy as DB
import Post
import DB

-- | Entry point. Starts a bulletin-board server at port 3000.
main :: IO ()
main = runServer 3000

-- | Run a bulletin-board server at at specific port.
runServer :: Port -> IO ()
runServer port = do
  app <- mkApp
  putStrLn $ unwords
    [ "Running bulletin board app at"
    , "http://localhost:" <> show port
    , "(ctrl-c to quit)"
    ]
  run port (Logger.logStdoutDev app)

-- ** Application and routing

-- | Bulletin board application description.
mkApp :: IO Twain.Application
mkApp = do
  db <- mkDB "/tmp/sqlitebulletintest.db"
  pure $ foldr ($)
    (Twain.notFound $ Twain.send $ Twain.text "Error: not found.")
    (routes db)

-- | Bulletin board routing.
routes :: DB -> [Twain.Middleware]
routes db =
  -- Our main page, which will display all of the bulletins
  [ Twain.get "/" $ do
    posts <- liftIO $ db.getPosts
    Twain.send (displayAllPosts posts)

  -- A page for a specific post
  , Twain.get "/post/:id" $ do
    pid <- Twain.param "id"
    post <- liftIO $ db.getPost pid
    Twain.send (displayPost post)

  -- A page for creating a new post
  , Twain.get "/new" $
    Twain.send handleGetNewPost

  -- A request to submit a new page
  , Twain.post "/new" $ do
    title <- Twain.param "title"
    author <- Twain.param "author"
    content <- Twain.param "content"

    response <-
      liftIO $ handlePostNewPost
        ( Post
          { pTitle = title
          , pAuthor = author
          , pContent = content
          , pTime = undefined
          }
        )
        db

    Twain.send response

  -- A request to delete a specific post
  , Twain.post "/post/:id/delete" $ do
    pid <- Twain.param "id"
    response <- liftIO $ handleDeletePost pid db
    Twain.send response

  -- css styling
  , Twain.get "/style.css" $
    Twain.send $ Twain.css ".main { width: 900px; margin: auto; }"
  ]

-- ** Business logic

-- | Respond with a list of all posts
displayAllPosts :: [(DB.Int64, Post)] -> Twain.Response
displayAllPosts =
  Twain.html . H.renderBS . template "Bulletin board - posts" . allPostsHtml

-- | Respond with a specific post or return 404
displayPost :: (DB.Int64, Post) -> Twain.Response
displayPost (pid, post) =
      Twain.html $
        H.renderBS $
          template "Bulletin board - posts" $
            postHtml pid post

-- | Delete a post and respond to the user.
handleDeletePost :: DB.Int64 -> DB -> IO Twain.Response
handleDeletePost pid db = do
  found <- True <$ db.deletePostById pid
  pure $
    if found
      then
        Twain.redirect302 "/"

      else
        Twain.raw
          Twain.status404
          [("Content-Type", "text/html; charset=utf-8")]
          "404 Not Found."

-- | Respond with the new post page.
handleGetNewPost :: Twain.Response
handleGetNewPost =
  Twain.html $
    H.renderBS $
      template "Bulletin board - posts" $
        newPostHtml

-- | Respond with the new post page.
handlePostNewPost :: Post -> DB -> IO Twain.Response
handlePostNewPost post db = do
  pid <- db.insertPost post
  pure $ Twain.redirect302 ("/post/" <> T.pack (show pid))

-- ** HTML

type Html = H.Html ()

-- | HTML boilerplate template
template :: T.Text -> Html -> Html
template title content =
  H.doctypehtml_ $ do
    H.head_ $ do
      H.meta_ [ H.charset_ "utf-8" ]
      H.title_ (H.toHtml title)
      H.link_ [ H.rel_ "stylesheet", H.type_ "text/css", H.href_ "/style.css"  ]
    H.body_ $ do
      H.div_ [ H.class_ "main" ] $ do
        H.h1_ [ H.class_ "logo" ] $
          H.a_ [H.href_ "/"] "Bulletin Board"
        content

-- | All posts page.
allPostsHtml :: [(DB.Int64, Post)] -> Html
allPostsHtml posts = do
  H.p_ [ H.class_ "new-button" ] $
    H.a_ [H.href_ "/new"] "New Post"
  mapM_ (uncurry postHtml) posts

-- | A single post as HTML.
postHtml :: DB.Int64 -> Post -> Html
postHtml pid post = do
  H.div_ [ H.class_ "post" ] $ do
    H.div_ [ H.class_ "post-header" ] $ do
      H.h2_ [ H.class_ "post-title" ] $
        H.a_
          [H.href_ ("/post/" <> T.pack (show pid))]
          (H.toHtml $ pTitle post)

      H.span_ $ do
        H.p_ [ H.class_ "post-time" ] $ H.toHtml (T.pack (show (pTime post)))
        H.p_ [ H.class_ "post-author" ] $ H.toHtml (pAuthor post)

    H.div_ [H.class_ "post-content"] $ do
      H.toHtml (pContent post)

    -- delete button
    H.form_
      [ H.method_ "post"
      , H.action_ ("/post/" <> T.pack (show pid) <> "/delete")
      , H.onsubmit_ "return confirm('Are you sure?')"
      , H.class_ "delete-post"
      ]
      ( do
        H.input_ [H.type_ "submit", H.value_ "Delete", H.class_ "deletebtn"]
      )

-- | A new post form.
newPostHtml :: Html
newPostHtml = do
  H.form_
    [ H.method_ "post"
    , H.action_ "/new"
    , H.class_ "new-post"
    ]
    ( do
      H.p_ $ H.input_ [H.type_ "text", H.name_ "title", H.placeholder_ "Title..."]
      H.p_ $ H.input_ [H.type_ "text", H.name_ "author", H.placeholder_ "Author..."]
      H.p_ $ H.textarea_ [H.name_ "content", H.placeholder_ "Content..."] ""
      H.p_ $ H.input_ [H.type_ "submit", H.value_ "Submit", H.class_ "submit-button"]
    )
