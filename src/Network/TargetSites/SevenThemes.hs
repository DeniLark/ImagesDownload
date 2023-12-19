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

process :: String -> String -> [HTMLNode] -> IO ()
process baseUrl url html = do
  let imgUrls = imglinkToLink $ thumbsToLink $ findElemsByClass "thumb" html
  putStrLn $ "Found images: " <> show (length imgUrls)
  processManyOrError url (processOneImage baseUrl . unpack) imgUrls

thumbsToLink :: [HTMLNode] -> [HTMLNode]
thumbsToLink = foldr foldFunc []
 where
  foldFunc :: HTMLNode -> [HTMLNode] -> [HTMLNode]
  foldFunc node = case findElemsByTagName "a" [node] of
    []      -> id
    (a : _) -> (a :)
