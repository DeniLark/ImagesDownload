module Run where

import           Network.HTTP
import           System.Environment

run :: IO ()
run = do
  args <- getArgs
  case args of
    []   -> putStrLn "No URLs"
    urls -> mapM_ processPage urls
