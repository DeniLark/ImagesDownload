module File.Fetch where

import           Control.Exception
import           Network.HTTP.Simple     hiding ( parseRequest )

import           Constants
import           File.Write

fetchFile :: String -> IO ()
fetchFile url = action `catch` handler
 where
  action :: IO ()
  action = do
    request <- parseRequestThrow url
    file    <- getResponseBody <$> httpBS request
    let fileName = getFileName url
    saveFile fileName (dirResult <> ('/' : fileName)) file

parseRequest :: String -> Maybe Request
parseRequest = parseRequestThrow

handler :: HttpException -> IO ()
handler = const $ putStrLn "Wrong url"
