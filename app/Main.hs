module Main
  ( main
  ) where

import           Constants
import           IO.FetchFile

main :: IO ()
main = do
  fetchFile urlImage1
  putStrLn "Done!"
