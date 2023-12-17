module Network.Exception where

import           Network.HTTP.Conduit              -- ( HttpException(..) )
import           Network.HTTP.Types             ( Status(statusCode) )


handlerHttpException :: HttpException -> IO ()
handlerHttpException (InvalidUrlException url _) =
  putStrLn $ "Invalid url: " <> url
handlerHttpException (HttpExceptionRequest request exceptionContent) = do
  let msg = exceptionContentToString exceptionContent
      url = host request <> path request
  putStrLn $ show url <> ": " <> msg

responseToStatusCode :: Response () -> String
responseToStatusCode = show . statusCode . responseStatus

exceptionContentToString :: HttpExceptionContent -> String
exceptionContentToString c = case c of
  StatusCodeException response _ ->
    "StatusCodeException: " <> responseToStatusCode response
  -- https://wallpaperscraft.ru/asdasda

  TooManyRedirects _                  -> "TooManyRedirects"
  OverlongHeaders                     -> "OverlongHeaders"
  ResponseTimeout                     -> "ResponseTimeout"
  ConnectionTimeout                   -> "ConnectionTimeout"

  ConnectionFailure    _              -> "ConnectionFailure"
  -- https://wwallpaperscraft.ru/ instead of https://wallpaperscraft.ru/

  InvalidStatusLine    _              -> "InvalidStatusLine"
  InvalidHeader        _              -> "InvalidHeader"
  InvalidRequestHeader _              -> "InvalidRequestHeader"
  InternalException    _              -> "InternalException"
  ProxyConnectException{}             -> "ProxyConnectException"
  NoResponseDataReceived              -> "NoResponseDataReceived"
  TlsNotSupported                     -> "TlsNotSupported"
  WrongRequestBodyStreamSize _ _      -> "WrongRequestBodyStreamSize"
  ResponseBodyTooShort       _ _      -> "ResponseBodyTooShort"
  InvalidChunkHeaders                 -> "InvalidChunkHeaders"
  IncompleteHeaders                   -> "IncompleteHeaders"
  InvalidDestinationHost _            -> "InvalidDestinationHost"
  HttpZlibException      _            -> "HttpZlibException"
  InvalidProxyEnvironmentVariable _ _ -> "InvalidProxyEnvironmentVariable"
  ConnectionClosed                    -> "ConnectionClosed"
  InvalidProxySettings _              -> "InvalidProxySettings"



