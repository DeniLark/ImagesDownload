module Run where

import           Network.HTTP                   ( processPage )
import           System.Environment             ( getArgs )

run :: IO ()
run = do
  args <- getArgs
  case args of
    []   -> putStrLn "No URLs"
    urls -> mapM_ processPage urls

