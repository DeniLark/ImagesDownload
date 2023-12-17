{-# LANGUAGE OverloadedStrings #-}

module Network.TargetSites where

import           Data.Text                      ( unpack )
import           Zenacy.HTML                    ( HTMLNode )

import           File.Fetch                     ( fetchFile )
import           HTML.UtilsZenacy               ( findElemsByTagName
                                                , imgsToSrcs
                                                )
import           Network.TargetSites.Wallpaperscraft
                                                ( processWallpaperscraft )
import           Network.URL                    ( addBaseUrl )

processorsTargetSites :: [(String, String -> [HTMLNode] -> IO ())]
processorsTargetSites =
  [("https://wallpaperscraft.ru", processWallpaperscraft)]

processorUniversalSite :: String -> [HTMLNode] -> IO ()
processorUniversalSite baseUrl html = do
  let imgs    = findElemsByTagName "img" html
      imgSrcs = imgsToSrcs imgs
  mapM_ (fetchFile . (`addBaseUrl` baseUrl) . unpack) imgSrcs
