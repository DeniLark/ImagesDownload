module Network.TargetSites where

import           Data.Text                      ( unpack )
import           Zenacy.HTML                    ( HTMLNode )

import           File.Fetch                     ( fetchFile )
import           HTML.UtilsZenacy               ( findElemsByTagName
                                                , imgsToSrcs
                                                )
import           Network.URL                    ( addBaseUrl )

import qualified Network.TargetSites.Wallpaperscraft
                                               as Wallpaperscraft
import qualified Network.TargetSites.Wallpapershq
                                               as Wallpapershq

processorsTargetSites :: [(String, String -> [HTMLNode] -> IO ())]
processorsTargetSites =
  [ ("https://wallpaperscraft.ru", Wallpaperscraft.process)
  , ("https://wallpapershq.ru"   , Wallpapershq.process)
  ]

processorUniversalSite :: String -> [HTMLNode] -> IO ()
processorUniversalSite baseUrl html = do
  let imgs    = findElemsByTagName "img" html
      imgSrcs = imgsToSrcs imgs
  mapM_ (fetchFile . (`addBaseUrl` baseUrl) . unpack) imgSrcs
