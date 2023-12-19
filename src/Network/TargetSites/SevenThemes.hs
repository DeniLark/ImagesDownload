module Network.TargetSites.SevenThemes
  ( process
  ) where

import           Data.Text                      ( unpack )
import           Zenacy.HTML                    ( HTMLNode )

import           HTML.UtilsZenacy               ( findElemsByClass
                                                , findElemsByTagName
                                                , imglinkToLink
                                                )
import           Network.GeneralProcess         ( processManyOrError
                                                , processOneImage
                                                )

process :: String -> [HTMLNode] -> IO ()
process baseUrl html = do
  let imgUrls = imglinkToLink $ thumbsToLink $ findElemsByClass "thumb" html
  putStrLn $ "Found images: " <> show (length imgUrls)
  processManyOrError baseUrl (processOneImage baseUrl . unpack) imgUrls

thumbsToLink :: [HTMLNode] -> [HTMLNode]
thumbsToLink = foldr foldFunc []
 where
  foldFunc :: HTMLNode -> [HTMLNode] -> [HTMLNode]
  foldFunc node = case findElemsByTagName "a" [node] of
    []      -> id
    (a : _) -> (a :)
