module Network.GeneralProcess where

import           Constants                      ( msgErrorGettingPage )

processManyOrError :: String -> (a -> IO b) -> [a] -> IO ()
processManyOrError url _ [] = putStrLn $ url <> ": " <> msgErrorGettingPage
processManyOrError _   k as = mapM_ k as
