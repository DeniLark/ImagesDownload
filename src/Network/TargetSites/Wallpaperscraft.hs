module Network.TargetSites.Wallpaperscraft
  ( process
  ) where

import           Control.Concurrent             ( threadDelay )
import           Data.Text                      ( unpack )
import           Zenacy.HTML                    ( HTMLNode )

import           HTML.UtilsZenacy               ( findElemsByClass
                                                , imglinkToLink
                                                , imgsToSrcs
                                                )
import           Network.GeneralProcess         ( processManyOrError
                                                , processOneImage
                                                )
import           Network.URL                    ( addBaseUrl
                                                , htmlFromUrl
                                                )

process :: String -> String -> [HTMLNode] -> IO ()
process baseUrl url html = do
  let imgUrls = imglinkToLink $ findElemsByClass "wallpapers__link" html
  putStrLn $ "Found images: " <> show (length imgUrls)
  processManyOrError url
                     (fetchImage baseUrl . (`addBaseUrl` baseUrl) . unpack)
                     imgUrls

fetchImage :: String -> String -> IO ()
fetchImage baseUrl url = do
  threadDelay 2000000
  html <- htmlFromUrl url
  let urlImages = imgsToSrcs $ findElemsByClass "wallpaper__image" html
  processManyOrError url (processOneImage baseUrl . unpack) urlImages
