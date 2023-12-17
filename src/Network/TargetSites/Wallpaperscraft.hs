module Network.TargetSites.Wallpaperscraft where

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
