{-# language OverloadedStrings #-}

-- | A bulletin board app built with twain.
module Bulletin where

import qualified Web.Twain as Twain
import Network.Wai.Handler.Warp (run, Port)
import qualified Data.Text as T
import qualified Data.Time.Clock as C
import qualified Data.Map as M

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
  run port app

-- ** Application and routing

-- | Bulletin board application description.
mkApp :: IO Twain.Application
mkApp = do
  dummyPosts <- makeDummyPosts
  pure $ foldr ($)
    (Twain.notFound $ Twain.send $ Twain.text "Error: not found.")
    (routes dummyPosts)

-- | Bulletin board routing.
routes :: Posts -> [Twain.Middleware]
routes posts =
  -- Our main page, which will display all of the bulletins
  [ Twain.get "/" $
    Twain.send (displayAllPosts posts)

  -- A page for a specific post
  , Twain.get "/post/:id" $ do
    pid <- Twain.param "id"
    Twain.send (displayPost pid posts)

  -- A page for creating a new post
  , Twain.get "/new" $
    Twain.send $ Twain.text "not yet implemented"

  -- A request to submit a new page
  , Twain.post "/new" $
    Twain.send $ Twain.text "not yet implemented"

  -- A request to delete a specific post
  , Twain.post "/post/:id/delete" $
    Twain.send $ Twain.text "not yet implemented"
  ]

-- ** Business logic

-- | Respond with a list of all posts
displayAllPosts :: Posts -> Twain.Response
displayAllPosts =
  Twain.text . T.unlines . map ppPost . M.elems

-- | Respond with a specific post or return 404
displayPost :: Integer -> Posts -> Twain.Response
displayPost pid posts =
  case M.lookup pid posts of
    Just post ->
      Twain.text (ppPost post)

    Nothing ->
      Twain.raw
        Twain.status404
        [("Content-Type", "text/plain; charset=utf-8")]
        "404 Not found."

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
