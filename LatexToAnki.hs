{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types.Status (
      statusCode,
      Status (statusMessage))
import Text.HTML.TagSoup (parseTags, Tag, sections, (~==), fromTagText, (~/=), isTagText, innerText, renderTags)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Text as T
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
import Data.Map
    (
      Map,
      fromList
    )

data RequestData = RequestData { action :: String, params :: Value, version :: Int } deriving Show
data ResponseData = ResponseData { errorField :: Maybe String, resultField :: Maybe [Maybe Int] } deriving Show

instance ToJSON RequestData where
    toJSON (RequestData action params version) = object ["action" .= action, "params" .= params, "version" .= version]

instance FromJSON ResponseData where
    parseJSON = withObject "ResponseData" $ \v -> ResponseData <$> v .:? "error" <*> v .:? "result"

request :: String -> Value -> RequestData
request action params = RequestData action params 6

invoke :: String -> Value -> IO (Maybe [Maybe Int])
invoke action params = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest "http://127.0.0.1:8765"

    let requestJson = encode (request action params)
        httpRequest = initialRequest { method = "POST", requestBody = RequestBodyLBS requestJson }

    print requestJson
    httpResponse <- httpLbs httpRequest manager

    let maybeResponseData = decode (responseBody httpResponse)
    case maybeResponseData of
        Nothing -> error "response has an unexpected number of fields"
        Just responseData -> case errorField responseData of
            Just err -> error err
            Nothing -> return (resultField responseData)

addNotes :: [Value] -> IO ()
addNotes json = do
    res <- invoke "addNotes" params
    case res of
      Nothing -> putStrLn "response is missing required result field"
      Just result -> putStrLn $ "ID of cards: " ++ show result
    where params = object ["notes" .= json]

createNote :: String -> String -> Value
createNote tags text  = object [
            "deckName" .= String "mathe1",
            "modelName" .= String "HPI",
            "fields" .= object ["Text" .= text],
            "options" .= object [
                "allowDuplicate" .= Bool False,
                "duplicateScope" .= String "deck",
                "duplicateScopeOptions" .= object [
                    "deckName" .= String "Default",
                    "checkChildren" .= Bool False,
                    "checkAllModels" .= Bool False
                ]
            ],
            "tags" .= [tags]
            ]

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
    Right body -> do 
      putStrLn "Which tags would you like to give (seperated by spaces)?"
      tags <- getLine
      addNotes $ map (createNote tags) $ getResults $ parseTags body

getResults :: [Tag String] -> [String]
getResults tags = map f $ sections (\tag -> tag ~== ("<div class='theorem'>" :: String) 
  || tag ~== ("<div class='definition'>" :: String)) tags

f :: [Tag String] -> String
f tags = fixLatex $ renderTags (takeWhile (~/= ("</div>" :: String)) tags) ++ "</div>"

fixLatex :: String -> String
fixLatex = T.unpack . mathSyntax . replaceCmds . T.pack

mathSyntax :: T.Text -> T.Text
mathSyntax text 
  | T.length st > 0 = mathSyntax $ replaceOne "$$" "</anki-mathjax>" . replaceOne "$$" "<anki-mathjax block=\"true\">" $ text
  | '$' `T.elem` text = mathSyntax $ replaceOne "$" "</anki-mathjax>" . replaceOne "$" "<anki-mathjax>" $ text
  | otherwise = text
    where 
      (_, st) = T.breakOn "$$" text

replaceCmds :: T.Text -> T.Text
replaceCmds = T.replace "\\realnum" "\\mathbb{R}"
            . T.replace "\\natnum" "\\mathbb{N}"
            . T.replace "\\integers" "\\mathbb{Z}"
            . T.replace "\\rationals" "\\mathbb{Q}"
            . T.replace "\\less" "\\lt"
            . T.replace "\\modulo" "\\; \\operatorname{mod} \\;"
            . T.replace "\\isom" "\\cong"
            . T.replace "\\qed" "\\Box"
            . T.replace "\\CalF" "\\mathcal{F}"
            . T.replace "\\CalX" "\\mathcal{X}"
            . T.replace "\\bigO" "\\mathcal{O}\\mathclose{\\left(#1\\right)}"
            . T.replace "\\bigOSymbol" "\\mathcal{O}"
            . T.replace "\\Bij" "\\mathcal{B}"

-- https://stackoverflow.com/a/14922122
replaceOne :: T.Text -> T.Text -> T.Text -> T.Text
replaceOne pattern substitution text
  | T.null back = text 
  | otherwise = T.concat [front, substitution, T.drop (T.length pattern) back] 
    where
      (front, back) = T.breakOn pattern text
