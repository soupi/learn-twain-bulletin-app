{-# language OverloadedStrings #-}

-- | A bulletin board app built with twain.
module Bulletin where

import qualified Web.Twain as Twain
import Network.Wai.Handler.Warp (run, Port)
import qualified Data.Text as T
import qualified Data.Time.Clock as C
import qualified Data.Map as M
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Lucid as H
import qualified Network.Wai.Middleware.RequestLogger as Logger

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
  dummyPosts <- makeDummyPosts
  appstateVar <- STM.newTVarIO AppState{asNextId = 1, asPosts = dummyPosts}
  pure $ foldr ($)
    (Twain.notFound $ Twain.send $ Twain.text "Error: not found.")
    (routes appstateVar)

-- | Bulletin board routing.
routes :: STM.TVar AppState -> [Twain.Middleware]
routes appstateVar =
  -- Our main page, which will display all of the bulletins
  [ Twain.get "/" $ do
    posts <- liftIO $ asPosts <$> STM.readTVarIO appstateVar
    Twain.send (displayAllPosts posts)

  -- A page for a specific post
  , Twain.get "/post/:id" $ do
    pid <- Twain.param "id"
    posts <- liftIO $ asPosts <$> STM.readTVarIO appstateVar
    Twain.send (displayPost pid posts)

  -- A page for creating a new post
  , Twain.get "/new" $
    Twain.send handleGetNewPost

  -- A request to submit a new page
  , Twain.post "/new" $ do
    title <- Twain.param "title"
    author <- Twain.param "author"
    content <- Twain.param "content"
    time <- liftIO C.getCurrentTime

    response <-
      liftIO $ handlePostNewPost
        ( Post
          { pTitle = title
          , pAuthor = author
          , pContent = content
          , pTime = time
          }
        )
        appstateVar

    Twain.send response

  -- A request to delete a specific post
  , Twain.post "/post/:id/delete" $ do
    pid <- Twain.param "id"
    response <- liftIO $ handleDeletePost pid appstateVar
    Twain.send response

  -- css styling
  , Twain.get "/style.css" $
    Twain.send $ Twain.css ".main { width: 900px; margin: auto; }"
  ]

-- ** Business logic

-- | Respond with a list of all posts
displayAllPosts :: Posts -> Twain.Response
displayAllPosts =
  Twain.html . H.renderBS . template "Bulletin board - posts" . allPostsHtml

-- | Respond with a specific post or return 404
displayPost :: Integer -> Posts -> Twain.Response
displayPost pid posts =
  case M.lookup pid posts of
    Just post ->
      Twain.html $
        H.renderBS $
          template "Bulletin board - posts" $
            postHtml pid post

    Nothing ->
      Twain.raw
        Twain.status404
        [("Content-Type", "text/plain; charset=utf-8")]
        "404 Not found."

-- | Delete a post and respond to the user.
handleDeletePost :: Integer -> STM.TVar AppState -> IO Twain.Response
handleDeletePost pid appstateVar = do
  found <- deletePost pid appstateVar
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
handlePostNewPost :: Post -> STM.TVar AppState -> IO Twain.Response
handlePostNewPost post appstateVar = do
  pid <- newPost post appstateVar
  pure $ Twain.redirect302 ("/post/" <> T.pack (show pid))

-- ** Application state

-- | Application state.
data AppState
  = AppState
    { asNextId :: Integer -- ^ The id for the next post
    , asPosts :: Posts -- ^ All posts
    }

-- ** Posts

-- | A mapping from a post id to a post.
type Posts = M.Map Integer Post

-- | A description of a bulletin board post.
data Post
  = Post
    { pTime :: C.UTCTime
    , pAuthor :: T.Text
    , pTitle :: T.Text
    , pContent :: T.Text
    }

-- | Create an initial posts Map with a dummy post.
makeDummyPosts :: IO Posts
makeDummyPosts = do
  time <- C.getCurrentTime
  pure $
    M.singleton
      0
      ( Post
        { pTime = time
        , pTitle = "Dummy title"
        , pAuthor = "Dummy author"
        , pContent = "bla bla bla..."
        }
      )

-- | Prettyprint a post to text.
ppPost :: Post -> T.Text
ppPost post =
  let
    header =
      T.unwords
        [ "[" <> T.pack (show (pTime post)) <> "]"
        , pTitle post
        , "by"
        , pAuthor post
        ]
    seperator =
      T.replicate (T.length header) "-"
  in
    T.unlines
      [ seperator
      , header
      , seperator
      , pContent post
      , seperator
      ]

-- | Add a new post to the store.
newPost :: Post -> STM.TVar AppState -> IO Integer
newPost post appstateVar = do
  STM.atomically $ do
    appstate <- STM.readTVar appstateVar
    STM.writeTVar
      appstateVar
      ( appstate
        { asNextId = asNextId appstate + 1
        , asPosts = M.insert (asNextId appstate) post (asPosts appstate)
        }
      )
    pure (asNextId appstate)

-- | Delete a post from the store.
deletePost :: Integer -> STM.TVar AppState -> IO Bool
deletePost pid appstateVar =
  STM.atomically $ do
    appstate <- STM.readTVar appstateVar
    case M.lookup pid (asPosts appstate) of
      Just{} -> do
        STM.writeTVar
          appstateVar
          ( appstate
            { asPosts = M.delete pid (asPosts appstate)
            }
          )
        pure True

      Nothing ->
        pure False

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
allPostsHtml :: Posts -> Html
allPostsHtml posts = do
  H.p_ [ H.class_ "new-button" ] $
    H.a_ [H.href_ "/new"] "New Post"
  mapM_ (uncurry postHtml) $ reverse $ M.toList posts

-- | A single post as HTML.
postHtml :: Integer -> Post -> Html
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
