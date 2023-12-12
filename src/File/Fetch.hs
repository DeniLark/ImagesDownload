module File.Fetch where

import           Control.Exception
import           Network.HTTP.Simple

import           File.Write
import           Network.Exception


fetchFile :: String -> IO ()
fetchFile url = action `catch` handlerHttpException
 where
  action :: IO ()
  action = do
    request <- parseRequestThrow url
    file    <- getResponseBody <$> httpBS request
    let fileName = getFileName url
    saveFile fileName fileName file
