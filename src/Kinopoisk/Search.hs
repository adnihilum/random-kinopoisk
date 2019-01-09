module Kinopoisk.Search where

import Codec.Text.IConv (convert)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Debug.Trace (trace)
import Kinopoisk.SearchUrl
import Network.HTTP.Simple
import Text.HTML.TagSoup
import Text.Parsec hiding (satisfy)
import Text.Parsec.Pos

getFirstMovie :: ContentType -> Integer -> Integer -> IO (Integer, [Text])
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
  (resultNum, foundElements) <-
    case parseBody allTags :: Either ParseError (Integer, [Text]) of
      Left err -> do
        putStrLn $ "parse error: " ++ show err
        undefined
      Right x -> return x
  putStrLn $ "resultNum = " ++ show resultNum
  putStrLn $ "foundElements = " ++ show foundElements
  return (resultNum, foundElements)

type Tag' = Tag LBS.ByteString

parseBody :: [Tag'] -> Either ParseError (Integer, [Text])
parseBody tags = parse parser "html tokens" $ Token <$> tags
  where
    parser :: Parsec [Token] () (Integer, [Text])
    parser = (,) <$> foundNum <*> many element
      where
        foundNum = do
          pText <- decodeUtf8 . convert "cp1251" "utf8" . innerText . (unToken <$>) <$> foundNumText
          trace ("pText = " ++ unpack pText) $ return ()
          trace ("words pText = " ++ show (words . unpack $ pText)) $ return ()
          let text = (!! 3) . words . unpack $ pText
          case readsPrec 10 text of
            [] -> unexpected "bad total number"
            [(total, _)] -> return total
        foundNumText = do
          afterAnyTokens $ token'' "<span class=search_results_topText>"
          afterAnyTokens $ token'' "</span>"
        element = do
          result <- decodeUtf8 . convert "cp1251" "utf8" . innerText . (unToken <$>) <$> elementTags
          trace ("found element=" ++ show result) $ return ()
          return result
        elementTags = do
          afterAnyTokens $ token'' "<div class=element>"
          afterAnyTokens $ token'' "<div class=info>"
          afterAnyTokens $ token'' "<p class=name>"
          afterAnyTokens $ token'' "<a class=js-serp-metrika>"
          afterAnyTokens $ token'' "</a>"
        afterAnyTokens p = manyTill anyToken (try p)

token'' :: String -> Parsec [Token] () Token
token'' = token' . Token . toTagRep

token' :: Token -> Parsec [Token] () Token
token' t = satisfy (== t) <?> show t --TODO:  replace Token with Tag' and (== t) with (~== t)

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

data Token = Token --TODO:  delete this
  { unToken :: Tag LBS.ByteString
  } deriving (Show)

instance Eq Token where
  a == b = unToken a ~== unToken b -- this operation is not communicative
