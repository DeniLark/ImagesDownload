module Network.GeneralProcess where

import           Constants                      ( msgErrorGettingPage )
import           File.Fetch                     ( fetchFile )
import           Network.URL                    ( addBaseUrl )

processManyOrError :: String -> (a -> IO b) -> [a] -> IO ()
processManyOrError url _ [] = putStrLn $ url <> ": " <> msgErrorGettingPage
processManyOrError _   k as = mapM_ k as

processOneImage :: String -> String -> IO ()
processOneImage baseUrl urlImage = do
  putStrLn ("Processing url: " <> urlImage)
  fetchFile $ (`addBaseUrl` baseUrl) urlImage
