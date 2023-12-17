module Network.TargetSites.Wallpaperscraft
  ( process
  ) where

import           Control.Concurrent             ( threadDelay )
import           Data.Text                      ( unpack )
import           Zenacy.HTML                    ( HTMLNode )

import           File.Fetch                     ( fetchFile )
import           HTML.UtilsZenacy               ( findElemsByClass
                                                , imglinkToLink
                                                , imgsToSrcs
                                                )
import           Network.GeneralProcess         ( processManyOrError )
import           Network.URL                    ( addBaseUrl
                                                , htmlFromUrl
                                                )

process :: String -> [HTMLNode] -> IO ()
process baseUrl html = do
  let imgUrls = imglinkToLink $ findElemsByClass "wallpapers__link" html
  processManyOrError baseUrl
                     (fetchImage baseUrl . (`addBaseUrl` baseUrl) . unpack)
                     imgUrls

fetchImage :: String -> String -> IO ()
fetchImage baseUrl url = do
  threadDelay 2000000
  html <- htmlFromUrl url
  let urlImages = imgsToSrcs $ findElemsByClass "wallpaper__image" html
  processManyOrError url (fetchFile . (`addBaseUrl` baseUrl) . unpack) urlImages
