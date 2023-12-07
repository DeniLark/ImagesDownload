module Main
  ( main
  ) where

import           Constants
import           File.Fetch

main :: IO ()
main = do
  fetchFile urlImage1
  putStrLn "Done!"
