module Post where

import qualified Data.Text as T
import qualified Data.Time.Clock as C

-- | A description of a bulletin board post.
data Post
  = Post
    { pTime :: C.UTCTime
    , pAuthor :: T.Text
    , pTitle :: T.Text
    , pContent :: T.Text
    }
