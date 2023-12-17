{-# LANGUAGE LambdaCase #-}

module HTML.UtilsZenacy where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Zenacy.HTML                    ( HTMLAttr(htmlAttrVal)
                                                , HTMLNode(HTMLElement)
                                                , htmlElemAttrFindName
                                                , htmlElemGetAttr
                                                , htmlElemHasID
                                                , htmlNodeContent
                                                )


imglinkToLink :: [HTMLNode] -> [Text]
imglinkToLink = foldr foldFunc []
 where
  foldFunc :: HTMLNode -> [Text] -> [Text]
  foldFunc node = case getElemAttrValue "href" node of
    ""  -> id
    src -> (src :)

imgsToSrcs :: [HTMLNode] -> [Text]
imgsToSrcs = foldr foldFunc []
 where
  foldFunc :: HTMLNode -> [Text] -> [Text]
  foldFunc node = case getElemAttrValue "src" node of
    ""  -> id
    src -> (src :)

------------------------
findElemById :: Text -> [HTMLNode] -> [HTMLNode]
findElemById idName = filterNodes p
 where
  p :: HTMLNode -> Bool
  p = htmlElemHasID idName

getElemAttrValue :: Text -> HTMLNode -> Text
getElemAttrValue attrName =
  maybe "" htmlAttrVal . htmlElemAttrFindName attrName

findElemsByClass :: Text -> [HTMLNode] -> [HTMLNode]
findElemsByClass className = filterNodes p
 where
  p :: HTMLNode -> Bool
  p el = case htmlElemGetAttr "class" el of
    Nothing      -> False
    Just classes -> elem className $ T.words classes

findElemsByTagName :: Text -> [HTMLNode] -> [HTMLNode]
findElemsByTagName target = filterNodes p
 where
  p (HTMLElement tag _ _ _) | target == tag = True
                            | otherwise     = False
  p _ = False

filterNodes :: (HTMLNode -> Bool) -> [HTMLNode] -> [HTMLNode]
filterNodes p (el : rest)
  | p el      = el : (filterNodes p (htmlNodeContent el) <> filterNodes p rest)
  | otherwise = filterNodes p (htmlNodeContent el) <> filterNodes p rest
filterNodes _ [] = []
