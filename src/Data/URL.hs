module Data.URL where

urlToDirName :: String -> String
urlToDirName url | take 7 url == "http://"  = replacer $ drop 7 url
                 | take 8 url == "https://" = replacer $ drop 8 url
                 | otherwise                = replacer url
 where
  replacer :: String -> String
  replacer = foldr (\ch -> if ch == '.' then ('_' :) else (ch :)) ""


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
