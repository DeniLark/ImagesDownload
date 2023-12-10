{-# LANGUAGE OverloadedStrings #-}

module HTML.IO where

import           Control.Exception
import           Data.Text                      ( unpack )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Network.HTTP.Simple
import           Zenacy.HTML

import           File.Fetch
import           HTML.UtilsZenacy
import           Network.Exception

processPage :: String -> IO ()
processPage url = action `catch` handlerHttpException
 where
  action :: IO ()
  action = do
    request <- parseRequestThrow url
    html    <- htmlParseEasy . decodeUtf8 . getResponseBody <$> httpBS request
    let imgs    = findElemsByTagName "img" [html]
        imgSrcs = imgsToSrcs imgs
        baseUrl = getBaseUrl url
    mapM_ (fetchFile . (`addBaseUrl` baseUrl) . unpack) imgSrcs

addBaseUrl :: String -> String -> String
addBaseUrl ""          = (<> "")
addBaseUrl s@('/' : _) = (<> s)
addBaseUrl s           = const s

getBaseUrl :: String -> String
getBaseUrl url
  | take 7 url == "http://" =  "http://"
  <> takeWhile (`notElem` seps) (drop 7 url)
  | take 8 url == "https://" =  "https://"
  <> takeWhile (`notElem` seps) (drop 8 url)
  | otherwise = url
 where
  seps :: String
  seps = "/?"

