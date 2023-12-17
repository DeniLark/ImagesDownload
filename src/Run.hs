module Run where

import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( getArgs )

import           Constants                      ( dirResult )
import           File.FilePath                  ( withCurrentDirectory )
import           Network.TargetSites            ( processorUniversalSite
                                                , processorsTargetSites
                                                )
import           Network.URL                    ( getBaseUrl
                                                , htmlFromUrl
                                                , urlToDirName
                                                )

run :: IO ()
run = do
  args <- getArgs
  case args of
    []   -> putStrLn "No URLs"
    urls -> mapM_ processPage urls

processPage :: String -> IO ()
processPage url = do
  putStrLn ("Processing url: " <> url)
  html <- htmlFromUrl url
  let baseUrl = getBaseUrl url
      dirName = urlToDirName baseUrl
  withCurrentDirectory (dirResult <> ('/' : dirName)) $ do
    let processor = fromMaybe processorUniversalSite
          $ lookup baseUrl processorsTargetSites
    processor baseUrl html
