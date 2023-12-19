module File.Fetch where

import           Control.Exception              ( catch )
import           Network.HTTP.Simple            ( getResponseBody
                                                , httpBS
                                                , parseRequestThrow
                                                )

import           File.Write                     ( getFileName
                                                , saveFile
                                                )
import           Network.Exception              ( handlerHttpException )


fetchFile :: String -> IO ()
fetchFile url = action `catch` handlerHttpException
 where
  action :: IO ()
  action = do
    request <- parseRequestThrow url
    file    <- getResponseBody <$> httpBS request
    let fileName = getFileName url
    saveFile (takeWhile (/= '?') fileName) file
