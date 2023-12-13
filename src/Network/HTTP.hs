{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP where

import           Control.Exception
import           Data.Text                      ( unpack )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Network.HTTP.Simple
import           Zenacy.HTML

import           Constants
import           Control.Concurrent             ( threadDelay )
import           File.Fetch
import           File.FilePath
import           HTML.UtilsZenacy
import           Network.Exception
import           Network.URL

processPage :: String -> IO ()
processPage url =
  putStrLn ("Processing url: " <> url) >> action `catch` handlerHttpException
 where
  action :: IO ()
  action = do
    request <- parseRequestThrow url
    html    <- htmlParseEasy . decodeUtf8 . getResponseBody <$> httpBS request
    let baseUrl = getBaseUrl url
        dirName = urlToDirName baseUrl
    case baseUrl of
      "https://wallpaperscraft.ru" -> do
        let imgUrls =
              imglinkToLink $ findElemsByClass "wallpapers__link" [html]
        withCurrentDirectory (dirResult <> ('/' : dirName)) $ mapM_
          (fetchImageWallpaperscraft baseUrl . (`addBaseUrl` baseUrl) . unpack)
          imgUrls
      _ -> do
        let imgs    = findElemsByTagName "img" [html]
            imgSrcs = imgsToSrcs imgs
        withCurrentDirectory (dirResult <> ('/' : dirName))
          $ mapM_ (fetchFile . (`addBaseUrl` baseUrl) . unpack) imgSrcs

fetchImageWallpaperscraft :: String -> String -> IO ()
fetchImageWallpaperscraft baseUrl url = action `catch` handlerHttpException
 where
  action :: IO ()
  action = do
    request <- parseRequestThrow url
    html    <- htmlParseEasy . decodeUtf8 . getResponseBody <$> httpBS request
    let urlImages = imgsToSrcs $ findElemsByClass "wallpaper__image" [html]
    mapM_ (fetchFile . (`addBaseUrl` baseUrl) . unpack) urlImages
    threadDelay 2000000

