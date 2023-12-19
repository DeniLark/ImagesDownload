module Network.TargetSites where

import           Data.Text                      ( unpack )
import           Zenacy.HTML                    ( HTMLNode )

import           File.Fetch                     ( fetchFile )
import           HTML.UtilsZenacy               ( findElemsByTagName
                                                , imgsToSrcs
                                                )
import           Network.URL                    ( addBaseUrl )

import           Network.GeneralProcess
import qualified Network.TargetSites.SevenThemes
                                               as SevenThemes
import qualified Network.TargetSites.Wallpaperscraft
                                               as Wallpaperscraft
import qualified Network.TargetSites.Wallpapershq
                                               as Wallpapershq


processorsTargetSites :: [(String, String -> [HTMLNode] -> IO ())]
processorsTargetSites =
  [ ("https://wallpaperscraft.ru", Wallpaperscraft.process)
  , ("https://wallpapershq.ru"   , Wallpapershq.process)
  , ("https://7themes.su"        , SevenThemes.process)
  ]

processorUniversalSite :: String -> [HTMLNode] -> IO ()
processorUniversalSite baseUrl html = do
  let imgs    = findElemsByTagName "img" html
      imgUrls = imgsToSrcs imgs
  putStrLn $ "Found images: " <> show (length imgUrls)
  processManyOrError baseUrl
                     (fetchFile . (`addBaseUrl` baseUrl) . unpack)
                     imgUrls
