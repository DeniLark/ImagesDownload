{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP where

import           Control.Exception
import           Data.Text                      ( unpack )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Network.HTTP.Simple
import           Zenacy.HTML

import           Constants
import           File.Fetch
import           File.FilePath
import           HTML.UtilsZenacy
import           Network.Exception
import           Network.URL

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


