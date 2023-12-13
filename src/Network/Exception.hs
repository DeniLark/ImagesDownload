module Network.Exception where

import           Network.HTTP.Conduit

handlerHttpException :: HttpException -> IO ()
handlerHttpException (InvalidUrlException _ _) =
  putStrLn "Something wrong: InvalidUrlException"
handlerHttpException (HttpExceptionRequest _ exceptionContent) =
  putStrLn $ "Something wrong: HttpExceptionRequest" <> show exceptionContent
