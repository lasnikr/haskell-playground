{-# LANGUAGE OverloadedStrings #-}
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
import Text.HTML.TagSoup (parseTags, Tag, sections, (~==), fromTagText, (~/=), isTagText, innerText, renderTags)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Text(pack, unpack, replace, Text)

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
  -- url <- getLine
  -- response <- openUrl url manager
  body <- readFile "ht.html"
  -- case response of
  --   Left status -> putStrLn $ "Request wasn't successful. Status code was: " ++ show status
  --   Right body -> putStrLn . concat . getResults $ parseTags body
  putStrLn . concat . getResults $ parseTags body

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