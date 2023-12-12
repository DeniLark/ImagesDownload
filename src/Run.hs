module Run where

import           Constants
import           Network.HTTP

run :: IO ()
run = processPage urlPage1
