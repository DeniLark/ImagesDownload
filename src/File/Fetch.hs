module File.Fetch where

import           Network.HTTP.Simple

import           Constants
import           File.Write

fetchFile :: String -> IO ()
fetchFile url = do
  file <- getResponseBody <$> httpBS (parseRequest_ url)
  saveFile (dirResult <> ('/' : getFileName url)) file
