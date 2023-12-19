module Network.URL where

import Control.Exception (catch)
import Data.Text.Encoding (decodeUtf8)
import Network.Exception (handlerHttpException)
import Network.HTTP.Simple
  ( getResponseBody,
    httpBS,
    parseRequestThrow,
  )
import Zenacy.HTML
  ( HTMLNode,
    htmlParseEasy,
  )

htmlFromUrl :: String -> IO [HTMLNode]
htmlFromUrl url = action `catch` ((>> pure []) . handlerHttpException)
  where
    action :: IO [HTMLNode]
    action = do
      request <- parseRequestThrow url
      pure . htmlParseEasy . decodeUtf8 . getResponseBody <$> httpBS request

urlToDirName :: String -> String
urlToDirName url
  | take 7 url == "http://" = replacer $ drop 7 url
  | take 8 url == "https://" = replacer $ drop 8 url
  | otherwise = replacer url
  where
    replacer :: String -> String
    replacer = foldr (\ch -> if ch == '.' then ('_' :) else (ch :)) ""

addBaseUrl :: String -> String -> String
addBaseUrl "" baseUrl = baseUrl <> ""
addBaseUrl url@('/' : '/' : _) baseUrl = getProtocolFromUrl baseUrl <> url
addBaseUrl url@('/' : _) baseUrl = baseUrl <> url
addBaseUrl url _ = url

getProtocolFromUrl :: String -> String
getProtocolFromUrl url
  | take 7 url == "http://" = "http:"
  | take 8 url == "https://" = "https:"
  | otherwise = url

getBaseUrl :: String -> String
getBaseUrl url
  | take 7 url == "http://" =
      "http://"
        <> takeWhile (`notElem` seps) (drop 7 url)
  | take 8 url == "https://" =
      "https://"
        <> takeWhile (`notElem` seps) (drop 8 url)
  | otherwise = url
  where
    seps :: String
    seps = "/?"
