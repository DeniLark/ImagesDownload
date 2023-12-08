module Main
  ( main
  ) where

import           Constants
import           File.Fetch

main :: IO ()
main = do
  fetchFile urlImage1
  fetchFile urlImageWrong1
  fetchFile urlImage2
  fetchFile urlImageWrong2
