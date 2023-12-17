{-# LANGUAGE OverloadedStrings #-}

module Network.TargetSites.Wallpapershq
  ( process
  ) where

import           Control.Concurrent             ( threadDelay )
import           Data.Text                      ( unpack )
import           Zenacy.HTML                    ( HTMLNode )

import           File.Fetch                     ( fetchFile )
import           HTML.UtilsZenacy               ( findElemById
                                                , findElemsByClass
                                                , findElemsByTagName
                                                , imglinkToLink
                                                )
import           Network.GeneralProcess         ( processManyOrError )
import           Network.URL                    ( addBaseUrl
                                                , htmlFromUrl
                                                )

process :: String -> [HTMLNode] -> IO ()
process baseUrl html = do
  let imgUrls = imglinkToLink $ findElemsByTagName "a" $ findElemsByClass
        "list-wallpapers__item"
        html
  processManyOrError baseUrl
                     (fetchImage baseUrl . (`addBaseUrl` baseUrl) . unpack)
                     imgUrls

fetchImage :: String -> String -> IO ()
fetchImage baseUrl url = do
  threadDelay 2000000
  html <- htmlFromUrl url

  let urlImages = imglinkToLink $ findElemsByTagName "a" $ findElemById
        "photoswipe--gallery"
        html
  processManyOrError url (fetchFile . (`addBaseUrl` baseUrl) . unpack) urlImages

