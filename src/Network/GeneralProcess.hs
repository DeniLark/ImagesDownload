module Network.GeneralProcess where

import           Constants                      ( msgNoImageFound )
import           File.Fetch                     ( fetchFile )
import           Network.URL                    ( addBaseUrl )

processManyOrError :: String -> (a -> IO b) -> [a] -> IO ()
processManyOrError url _ []  = putStrLn $ url <> ": " <> msgNoImageFound
processManyOrError _   k [a] = mapM_ k [a] -- one image
processManyOrError _ k as =
  putStrLn ("Found images: " <> show (length as)) >> mapM_ k as

processOneImage :: String -> String -> IO ()
processOneImage baseUrl urlImage = do
  let url = addBaseUrl urlImage baseUrl
  putStrLn ("Processing url: " <> url)
  fetchFile url
