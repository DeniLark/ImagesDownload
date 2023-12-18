module Network.GeneralProcess where

import           Constants                      ( msgErrorGettingPage )

processManyOrError :: String -> (a -> IO b) -> [a] -> IO ()
processManyOrError url _ [] = putStrLn $ url <> ": " <> msgErrorGettingPage
processManyOrError _ k as =
  putStrLn ("Found images: " <> show (length as)) >> mapM_ k as

