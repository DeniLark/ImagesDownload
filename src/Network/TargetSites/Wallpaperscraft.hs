module Network.TargetSites.Wallpaperscraft
  ( process
  ) where

import           Control.Concurrent             ( threadDelay )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Zenacy.HTML                    ( HTMLNode )

import           File.Fetch                     ( fetchFile )
import           HTML.UtilsZenacy               ( findElemsByClass
                                                , imglinkToLink
                                                , imgsToSrcs
                                                )
import           Network.URL                    ( addBaseUrl
                                                , htmlFromUrl
                                                )

process :: String -> [HTMLNode] -> IO ()
process baseUrl html = do
  let imgUrls = imglinkToLink $ findElemsByClass "wallpapers__link" html
  mapM_ (fetchImage baseUrl) imgUrls

fetchImage :: String -> Text -> IO ()
fetchImage baseUrl url = do
  threadDelay 2000000
  html <- htmlFromUrl $ (`addBaseUrl` baseUrl) $ unpack url
  let urlImages = imgsToSrcs $ findElemsByClass "wallpaper__image" html
  mapM_ (fetchFile . (`addBaseUrl` baseUrl) . unpack) urlImages
