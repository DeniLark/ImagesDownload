{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module File.Write where

import           Data.Bifunctor                 ( Bifunctor(first) )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as BC
import           Data.Char
import           System.Directory               ( doesFileExist )

saveFile :: String -> FilePath -> ByteString -> IO ()
saveFile fileName filePath bytes = doesFileExist filePath >>= \case
  True  -> saveFile fileName (changeFileName filePath) bytes
  False -> do
    BC.writeFile filePath bytes
    putStrLn $ "File: " <> fileName <> " is saved"

changeFileName :: FilePath -> FilePath
changeFileName = uncurry (<>) . first increaseFileName . span (/= '.')

data Postfix =
    Start
  | Process String
  | End

increaseFileName :: FilePath -> FilePath
increaseFileName = fst . foldr foldFunc ("", Start)
 where
  foldFunc :: Char -> (FilePath, Postfix) -> (FilePath, Postfix)
  foldFunc ')' (_  , Start) = (")", Process "")
  foldFunc ch  (_  , Start) = (ch : "(1)", End)
  foldFunc ch  (acc, End  ) = (ch : acc, End)
  foldFunc ch (acc, Process temp)
    | isDigit ch && acc == ")" = (")", Process $ ch : temp)
    | ch == '('                = (ch : (incTemp temp <> ")"), End)
    | otherwise                = (temp <> (')' : "(1)"), End)

  incTemp :: String -> String
  incTemp = show . (+ 1) . (read :: String -> Int)

getFileName :: FilePath -> FilePath
getFileName = safeTail . foldr foldFunc ""
 where
  foldFunc :: Char -> FilePath -> FilePath
  foldFunc _   acc@('/' : _) = acc
  foldFunc '/' acc           = '/' : acc
  foldFunc ch  ""            = [ch]
  foldFunc ch  acc           = ch : acc

safeTail :: String -> String
safeTail "" = ""
safeTail s  = tail s
