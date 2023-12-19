module Network.TargetSites where

import           Data.Text                      ( unpack )
import           Zenacy.HTML                    ( HTMLNode )

import           File.Fetch                     ( fetchFile )
import           HTML.UtilsZenacy               ( findElemsByTagName
                                                , imgsToSrcs
                                                )
import           Network.URL                    ( addBaseUrl )

import           Network.GeneralProcess         ( processManyOrError )
import qualified Network.TargetSites.SevenThemes
                                               as SevenThemes
import qualified Network.TargetSites.Wallpaperscraft
                                               as Wallpaperscraft
import qualified Network.TargetSites.Wallpapershq
                                               as Wallpapershq


processorsTargetSites :: [(String, String -> String -> [HTMLNode] -> IO ())]
processorsTargetSites =
  [ ("https://wallpaperscraft.ru", Wallpaperscraft.process)
  , ("https://wallpapershq.ru"   , Wallpapershq.process)
  , ("https://7themes.su"        , SevenThemes.process)
  -- https://zastavok.net/cosmos/
  -- https://www.google.com/search?q=%D0%BE%D0%B1%D0%BE%D0%B8+%D0%BD%D0%B0+%D1%80%D0%B0%D0%B1%D0%BE%D1%87%D0%B8%D0%B9+%D1%81%D1%82%D0%BE%D0%BB&oq=%D0%BE%D0%B1%D0%BE%D0%B8&gs_lcrp=EgZjaHJvbWUqBwgBEAAYgAQyDAgAEEUYORixAxiABDIHCAEQABiABDINCAIQABiDARixAxiABDIHCAMQABiABDIHCAQQABiABDIKCAUQABixAxiABDIHCAYQABiABDIGCAcQRRhB0gEIMzIzOWowajeoAgCwAgA&sourceid=chrome&ie=UTF-8#ip=1
  ]

processorUniversalSite :: String -> String -> [HTMLNode] -> IO ()
processorUniversalSite baseUrl url html = do
  let imgs    = findElemsByTagName "img" html
      imgUrls = imgsToSrcs imgs
  processManyOrError url (fetchFile . (`addBaseUrl` baseUrl) . unpack) imgUrls
