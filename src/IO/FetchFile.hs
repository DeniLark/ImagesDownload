module IO.FetchFile where

import           Network.HTTP.Simple

import           Constants
import           IO.WriteFile

fetchFile :: Request -> IO ()
fetchFile request = do
  file <- getResponseBody <$> httpBS request
  saveFile (dirResult <> "/file.jpg") file
