import Network.HTTP.Client
    ( httpLbs,
      newManager,
      parseRequest,
      Manager,
      Response(responseBody, responseStatus) )
import Network.HTTP.Types.Status (
      statusCode, 
      Status (statusMessage))
import Network.HTTP.Client.TLS (
      tlsManagerSettings )
import Data.ByteString.Lazy (
  ByteString, writeFile )
import Data.Functor.Classes (Read1(liftReadPrec))

openUrl :: String -> Manager -> IO (Maybe ByteString)
openUrl str manager = do
  request <- parseRequest str
  response <- httpLbs request manager
  let 
    status = responseStatus response
    responseCode = statusCode status
    ret 
      | responseCode >= 200 && responseCode < 300 = Just $ responseBody response
      | otherwise = Nothing
  return ret

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  putStrLn "Which Website would you like to parse?"
  url <- getLine
  body <- openUrl url manager
  case body of
    Just response -> Data.ByteString.Lazy.writeFile "temp1.htm" response
    Nothing -> putStrLn "Request wasn't successful"
  