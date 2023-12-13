{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Zenacy.HTML                    ( HTMLNode )

import           Constants                      ( dirResult )
import           Control.Concurrent             ( threadDelay )
import           File.Fetch                     ( fetchFile )
import           File.FilePath                  ( withCurrentDirectory )
import           HTML.UtilsZenacy               ( findElemsByClass
                                                , findElemsByTagName
                                                , imglinkToLink
                                                , imgsToSrcs
                                                )
import           Network.URL                    ( addBaseUrl
                                                , getBaseUrl
                                                , htmlFromUrl
                                                , urlToDirName
                                                )

processPage :: String -> IO ()
processPage url = do
  putStrLn ("Processing url: " <> url)
  html <- htmlFromUrl url
  let baseUrl = getBaseUrl url
      dirName = urlToDirName baseUrl
  withCurrentDirectory (dirResult <> ('/' : dirName)) $ case baseUrl of
    "https://wallpaperscraft.ru" -> processWallpaperscraft baseUrl html
    _                            -> do
      let imgs    = findElemsByTagName "img" html
          imgSrcs = imgsToSrcs imgs
      mapM_ (fetchFile . (`addBaseUrl` baseUrl) . unpack) imgSrcs

processWallpaperscraft :: String -> [HTMLNode] -> IO ()
processWallpaperscraft baseUrl html = do
  let imgUrls = imglinkToLink $ findElemsByClass "wallpapers__link" html
  mapM_ (fetchImageWallpaperscraft baseUrl) imgUrls

fetchImageWallpaperscraft :: String -> Text -> IO ()
fetchImageWallpaperscraft baseUrl url = do
  threadDelay 2000000
  html <- htmlFromUrl $ (`addBaseUrl` baseUrl) $ unpack url
  let urlImages = imgsToSrcs $ findElemsByClass "wallpaper__image" html
  mapM_ (fetchFile . (`addBaseUrl` baseUrl) . unpack) urlImages


