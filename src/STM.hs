{-# language OverloadedStrings #-}

module STM where

import qualified Data.Time.Clock as C
import qualified Data.Map as M
import qualified Control.Concurrent.STM as STM
import Post

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
