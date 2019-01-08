module Kinopoisk.Search where

import Codec.Text.IConv (convert)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Kinopoisk.SearchUrl
import Network.HTTP.Simple
import Text.HTML.TagSoup
import Text.Parsec hiding (satisfy)
import Text.Parsec.Pos

getFirstMovie :: ContentType -> Integer -> Integer -> IO (Maybe Text)
getFirstMovie type' fromYear toYear = do
  let url = buildUrl [ParamFromYear fromYear, ParamToYear toYear, ParamContentType type', ParamPerPage 10, ParamPage 3]
  putStrLn $ "url = " ++ unpack url
  request <- parseRequest $ unpack url
  response <- httpLBS request
  let statusCode = getResponseStatusCode response
  putStrLn $ "status code: " ++ show statusCode
  let headers = getResponseHeaders response
  putStrLn $ "headers: " ++ show headers
  let body = getResponseBody response
  let allTags = parseTags body
  titleTags <-
    case getTitleTags allTags :: Either ParseError [Tag'] of
      Left err -> do
        putStrLn $ "parse error: " ++ show err
        undefined
      Right tags -> return tags
  putStrLn $ "titleTags= " ++ show titleTags
  let movieTitles = innerText titleTags
  return $ decodeUtf8 . convert "cp1251" "utf8" <$> Just movieTitles

type Tag' = Tag LBS.ByteString

getTitleTags :: [Tag'] -> Either ParseError [Tag']
getTitleTags tags = (unToken <$>) <$> tokens
  where
    tokens = parse parser "html tokens" $ Token <$> tags
    parser :: Parsec [Token] () [Token]
    parser = do
      afterAnyTokens $ token'' "<div class=info>"
      afterAnyTokens $ token'' "<p class=name>"
      afterAnyTokens $ token'' "<a class=js-serp-metrika>"
      afterAnyTokens $ token'' "</a>"
      where
        afterAnyTokens p = manyTill anyToken (try p)

token'' :: String -> Parsec [Token] () Token
token'' = token' . Token . toTagRep

token' :: Token -> Parsec [Token] () Token
token' t = satisfy (== t) <?> show t

satisfy :: (Token -> Bool) -> Parsec [Token] () Token
satisfy pred =
  tokenPrim
    show
    nextPos
    (\t ->
       if pred t
         then Just t
         else Nothing)
  where
    nextPos pos token _tokens = setSourceColumn pos $ sourceColumn pos + 1

data Token = Token
  { unToken :: Tag LBS.ByteString
  } deriving (Show)

instance Eq Token where
  a == b = unToken a ~== unToken b -- this operation is not communicative
