{-# LANGUAGE OverloadedStrings #-}

module Network.TargetSites.Wallpapershq
  ( process
  ) where

import           Control.Concurrent             ( threadDelay )
import           Data.Text                      ( unpack )
import           Zenacy.HTML                    ( HTMLNode )

import           HTML.UtilsZenacy               ( findElemById
                                                , findElemsByClass
                                                , findElemsByTagName
                                                , imglinkToLink
                                                )
import           Network.GeneralProcess         ( processManyOrError
                                                , processOneImage
                                                )
import           Network.URL                    ( addBaseUrl
                                                , htmlFromUrl
                                                )

process :: String -> String -> [HTMLNode] -> IO ()
process baseUrl url html = do
  let imgUrls = imglinkToLink $ findElemsByTagName "a" $ findElemsByClass
        "list-wallpapers__item"
        html
  putStrLn $ "Found images: " <> show (length imgUrls)
  processManyOrError url
                     (fetchImage baseUrl . (`addBaseUrl` baseUrl) . unpack)
                     imgUrls

fetchImage :: String -> String -> IO ()
fetchImage baseUrl url = do
  threadDelay 2000000
  html <- htmlFromUrl url

  let urlImages = imglinkToLink $ findElemsByTagName "a" $ findElemById
        "photoswipe--gallery"
        html
  processManyOrError url (processOneImage baseUrl . unpack) urlImages

