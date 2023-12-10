module Main
  ( main
  ) where

import           Constants
import           File.Fetch
import           HTML.IO

main :: IO ()
main = do
  processPage urlPage1
  -- fetchFile urlImage1
  -- fetchFile urlImageWrong1
  -- fetchFile urlImage2
  -- fetchFile urlImageWrong2
