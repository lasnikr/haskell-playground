{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types.Status (
      statusCode,
      Status (statusMessage))
import Text.HTML.TagSoup (parseTags, Tag, sections, (~==), fromTagText, (~/=), isTagText, innerText, renderTags)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Text(pack, unpack, replace, Text)
import Network.HTTP.Client
    ( httpLbs,
      newManager,
      parseRequest,
      Request(requestBody, method),
      RequestBody(RequestBodyLBS),
      Response(responseBody, responseStatus), Manager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Data.Aeson
    ( encode,
      decode,
      (.:?),
      withObject,
      object,
      Key,
      FromJSON(parseJSON),
      Value(String, Bool),
      KeyValue((.=)),
      ToJSON(toJSON) )
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as Map

data RequestData = RequestData { action :: String, params :: Map.Map Key Value, version :: Int } deriving Show
data ResponseData = ResponseData { errorField :: Maybe String, resultField :: Maybe Int } deriving Show

instance ToJSON RequestData where
    toJSON (RequestData action params version) = object ["action" .= action, "params" .= params, "version" .= version]

instance FromJSON ResponseData where
    parseJSON = withObject "ResponseData" $ \v -> ResponseData <$> v .:? "error" <*> v .:? "result"

request :: String -> Map.Map Key Value -> RequestData
request action params = RequestData action params 6

invoke :: String -> Map.Map Key Value -> IO (Maybe Int)
invoke action params = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest "http://127.0.0.1:8765"

    let requestJson = encode (request action params)
        httpRequest = initialRequest { method = "POST", requestBody = RequestBodyLBS requestJson }

    httpResponse <- httpLbs httpRequest manager

    let maybeResponseData = decode (responseBody httpResponse) :: Maybe ResponseData
    case maybeResponseData of
        Nothing -> error "response has an unexpected number of fields"
        Just responseData -> case errorField responseData of
            Just err -> error err
            Nothing -> return (resultField responseData)

addNote :: Value -> IO ()
addNote json = do
    res <- invoke "addNote" params
    case res of
      Nothing -> putStrLn "response is missing required result field"
      Just result -> putStrLn $ "ID of card: " ++ show result
    where note = object [
            "deckName" .= String "test",
            "modelName" .= String "Basic",
            "fields" .= object ["Front" .= String "loslsss", "Back" .= String "bacsks"],
            "options" .= object [
                "allowDuplicate" .= Bool True,
                "duplicateScope" .= String "deck",
                "duplicateScopeOptions" .= object [
                    "deckName" .= String "Default",
                    "checkChildren" .= Bool False,
                    "checkAllModels" .= Bool False
                ]
            ],
            "tags" .= [String "test tag"]
            ]
          params = Map.fromList ["note" .= note]

-- _ <- invoke "createDeck" (Map.fromList [("deck", String "test1")])
-- maybeResult <- invoke "deckNames" Map.empty
-- case maybeResult of
--     Nothing -> putStrLn "response is missing required result field"
--     Just result -> putStrLn $ "got list of decks: " ++ show result

openUrl :: String -> Manager -> IO (Either Int String)
openUrl str manager = do
  request <- parseRequest str
  response <- httpLbs request manager
  let
    status = responseStatus response
    responseCode = statusCode status
    ret
      | responseCode >= 200 && responseCode < 300 = Right . toString $ responseBody response
      | otherwise = Left responseCode
  return ret
  
main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  putStrLn "Which Website would you like to parse?"
  url <- getLine
  response <- openUrl url manager
  case response of
    Left status -> putStrLn $ "Request wasn't successful. Status code was: " ++ show status
    Right body -> putStrLn . concat . getResults $ parseTags body
  -- addNotes, call with json as parameter, json from new function with takes results of important tags

getResults :: [Tag String] -> [String]
getResults tags = map f $ sections (~== ("<div class='theorem'>" :: String)) tags

f :: [Tag String] -> String
f tags = fixLatex $ renderTags (takeWhile (~/= ("</div>" :: String)) tags) ++ "</div>"

fixLatex :: String -> String
fixLatex = unpack . mathSyntax . replaceCmds . pack

mathSyntax :: Text -> Text
mathSyntax = replace "$" "<anki-mathjax>" . replace "$$" "<anki-mathjax block=\"true\">"

replaceCmds :: Text -> Text
replaceCmds = replace "realnum" "mathbb{R}" . replace "natnum" "mathbb{N}" 