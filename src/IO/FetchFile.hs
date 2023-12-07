module IO.FetchFile where

import           Network.HTTP.Simple

import           Constants
import           IO.WriteFile

fetchFile :: String -> IO ()
fetchFile url = do
  file <- getResponseBody <$> httpBS (parseRequest_ url)
  saveFile (dirResult <> ('/' : getFileName url)) file
