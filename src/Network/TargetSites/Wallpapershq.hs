{-# LANGUAGE OverloadedStrings #-}

module Network.TargetSites.Wallpapershq
  ( process
  ) where

import           Control.Concurrent             ( threadDelay )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Zenacy.HTML                    ( HTMLNode )

import           File.Fetch                     ( fetchFile )
import           HTML.UtilsZenacy               ( findElemById
                                                , findElemsByClass
                                                , findElemsByTagName
                                                , imglinkToLink
                                                )
import           Network.URL                    ( addBaseUrl
                                                , htmlFromUrl
                                                )

process :: String -> [HTMLNode] -> IO ()
process baseUrl html = do
  let imgUrls = imglinkToLink $ findElemsByTagName "a" $ findElemsByClass
        "list-wallpapers__item"
        html
  mapM_ (fetchImage baseUrl) imgUrls

fetchImage :: String -> Text -> IO ()
fetchImage baseUrl url = do
  threadDelay 2000000
  html <- htmlFromUrl $ (`addBaseUrl` baseUrl) $ unpack url

  let urlImages = imglinkToLink $ findElemsByTagName "a" $ findElemById
        "photoswipe--gallery"
        html
  mapM_ (fetchFile . (`addBaseUrl` baseUrl) . unpack) urlImages

