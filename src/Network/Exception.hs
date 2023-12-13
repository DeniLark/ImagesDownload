module Network.Exception where

import           Network.HTTP.Conduit           ( HttpException(..) )

handlerHttpException :: HttpException -> IO ()
handlerHttpException (InvalidUrlException _ _) =
  putStrLn "Something wrong: InvalidUrlException"
handlerHttpException (HttpExceptionRequest _ _) =
  putStrLn "Something wrong: HttpExceptionRequest" -- <> show exceptionContent
