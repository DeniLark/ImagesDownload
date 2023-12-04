{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IO.WriteFile where

import           Data.Bifunctor                 ( Bifunctor(first) )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as BC
import           Data.Char
import           System.Directory               ( doesFileExist )

saveFile :: FilePath -> ByteString -> IO ()
saveFile filePath bytes = doesFileExist filePath >>= \case
  True  -> saveFile (changeFileName filePath) bytes
  False -> BC.writeFile filePath bytes

changeFileName :: FilePath -> FilePath
changeFileName = uncurry (<>) . first increaseFileName . span (/= '.')

-- >>> increaseFileName "file(10)"
-- "file(11)"
--

-- >>> increaseFileName ""
-- ""
--

-- >>> increaseFileName "file"
-- "file(1)"
--

-- >>> increaseFileName "fil)e(12)"
-- "fil)e(13)"
--

-- >>> increaseFileName "fi(l)e(12)"
-- "fi(l)e(13)"
--

-- >>> increaseFileName "fi(le(12)"
-- "fi(le(13)"
--

-- >>> increaseFileName "file(12)"
-- "file(13)"
--

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

-- increaseFileName :: FilePath -> FilePath
-- increaseFileName "" = ""
-- increaseFileName fp = let x = span (/= '(') fp in undefined
-- increaseFileName = reverse . helper . reverse
--  where
--   helper "" = ""
--   helper (')' : rest) =
--     let (n :: Int, rest') = first read $ span isDigit rest
--     in  ')' : show (n + 1) <> rest'
--   helper fp = ")1(" <> fp
