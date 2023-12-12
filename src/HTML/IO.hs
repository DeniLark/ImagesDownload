{-# LANGUAGE OverloadedStrings #-}

module HTML.IO where

import           Control.Exception
import           Data.Text                      ( unpack )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Network.HTTP.Simple
import           Zenacy.HTML

import           Constants
import           Data.FilePath
import           Data.URL
import           File.Fetch
import           HTML.UtilsZenacy
import           Network.Exception

processPage :: String -> IO ()
processPage url = action `catch` handlerHttpException
 where
  action :: IO ()
  action = do
    request <- parseRequestThrow url
    html    <- htmlParseEasy . decodeUtf8 . getResponseBody <$> httpBS request
    let imgs    = findElemsByTagName "img" [html]
        imgSrcs = imgsToSrcs imgs
        baseUrl = getBaseUrl url
        dirName = urlToDirName baseUrl
    withCurrentDirectory (dirResult <> ('/' : dirName))
      $ mapM_ (fetchFile . (`addBaseUrl` baseUrl) . unpack) imgSrcs


