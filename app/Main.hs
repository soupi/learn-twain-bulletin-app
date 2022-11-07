import qualified Bulletin
import System.Environment

main :: IO ()
main = do
  port <- maybe 3000 read <$> lookupEnv "PORT"
  Bulletin.runServer port
