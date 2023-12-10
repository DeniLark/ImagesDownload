module Network.Exception where

import           Network.HTTP.Conduit

handlerHttpException :: HttpException -> IO ()
handlerHttpException = const $ putStrLn "Something wrong"
