{-# language OverloadedStrings #-}

-- | A bulletin board app built with twain.
module Bulletin where

import qualified Web.Twain as Twain
import Network.Wai.Handler.Warp (run, Port)

-- | Entry point. Starts a bulletin-board server at port 3000.
main :: IO ()
main = runServer 3000

-- | Run a bulletin-board server at at specific port.
runServer :: Port -> IO ()
runServer port = do
  putStrLn $ unwords
    [ "Running bulletin board app at"
    , "http://localhost:" <> show port
    , "(ctrl-c to quit)"
    ]
  run port mkApp

-- | Bulletin board application description.
mkApp :: Twain.Application
mkApp =
  foldr ($)
    (Twain.notFound $ Twain.send $ Twain.text "Error: not found.")
    routes

-- | Bulletin board routing.
routes :: [Twain.Middleware]
routes =
  -- Our main page, which will display all of the bulletins
  [ Twain.get "/" $
    Twain.send $ Twain.text "not yet implemented"

  -- A page for a specific post
  , Twain.get "/post/:id" $
    Twain.send $ Twain.text "not yet implemented"

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
