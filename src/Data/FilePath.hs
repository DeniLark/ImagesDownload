{-# LANGUAGE LambdaCase #-}

module Data.FilePath where

import qualified System.Directory              as SD

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory filePath action = do
  SD.doesDirectoryExist filePath >>= \case
    False -> SD.createDirectory filePath
    True  -> pure ()
  SD.withCurrentDirectory filePath action


