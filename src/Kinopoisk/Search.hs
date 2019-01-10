module Kinopoisk.Search where

import Codec.Text.IConv (convert)
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.Text.Lazy (Text, append, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Debug.Trace (trace)
import Kinopoisk.SearchUrl
import Network.HTTP.Simple
import System.Random
import Text.HTML.TagSoup
import Text.Parsec hiding (satisfy)
import Text.Parsec.Pos

data SearchParams = SearchParams
  { spContentType :: ContentType
  , spFromYear :: Integer
  , spToYear :: Integer
  , spPage :: Integer
  }

perPage = 10

getRandomElement :: SearchParams -> IO (Integer, Maybe (Text, Text))
getRandomElement params = do
  (total, _) <- getElementsOnPage params
  if total == 0
    then return (total, Nothing)
    else do
      putStrLn $ "total = " ++ show total
      let totalPages = (total `div` perPage) + 1
      putStrLn $ "totalPages = " ++ show totalPages
      randomPage <- randomRIO (1, totalPages)
      putStrLn $ "randomPage = " ++ show randomPage
      (_, elements) <- getElementsOnPage $ params {spPage = randomPage}
      randomElement <- (elements !!) <$> randomRIO (0, length elements - 1)
      return (total, Just randomElement)

getElementsOnPage :: SearchParams -> IO (Integer, [(Text, Text)])
getElementsOnPage params = do
  let url =
        buildUrl
          [ ParamFromYear $ params & spFromYear
          , ParamToYear $ params & spToYear
          , ParamContentType $ params & spContentType
          , ParamPerPage perPage
          , ParamPage $ params & spPage
          ]
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
    case parseBody allTags :: Either ParseError (Integer, [(Text, Text)]) of
      Left err -> do
        putStrLn $ "parse error: " ++ show err
        undefined
      Right x -> return x
  putStrLn $ "resultNum = " ++ show resultNum
  putStrLn $ "foundElements = " ++ show foundElements
  return (resultNum, (\(url, title) -> (baseUrl `append` url, title)) <$> foundElements)

type Token = Tag LBS.ByteString

parseBody :: [Token] -> Either ParseError (Integer, [(Text, Text)])
parseBody = parse parser "html tokens"
  where
    parser :: Parsec [Token] () (Integer, [(Text, Text)])
    parser = (,) <$> foundNum <*> many element
      where
        foundNum = do
          pText <- decodeBS . innerText <$> foundNumText
          trace ("pText = " ++ unpack pText) $ return ()
          trace ("words pText = " ++ show (words . unpack $ pText)) $ return ()
          let text = (!! 3) . words . unpack $ pText
          case readsPrec 10 text of
            [] -> unexpected "bad total number"
            [(total, _)] -> return total
        foundNumText = do
          afterAnyTokens $ token'' "<span class=search_results_topText>"
          afterAnyTokens $ token'' "</span>"
        element =
          try $ do
            afterAnyTokens $ token'' "<div class=info>"
            afterAnyTokens $ token'' "<p class=name>"
            let elementLink = token'' "<a class=js-serp-metrika>"
            many $ notFollowedBy elementLink
            TagOpen "a" attributes <- elementLink
            trace ("attributes = " ++ show attributes) $ return 0
            case decodeBS <$> lookup "href" attributes of
              Nothing -> unexpected "elements without url"
              Just url -> do
                title <- decodeBS . innerText <$> (afterAnyTokens $ token'' "</a>")
                return (url, title)
        afterAnyTokens p = manyTill anyToken (try p)
        decodeBS = decodeUtf8 . convert "cp1251" "utf8"

token'' :: String -> Parsec [Token] () Token
token'' = token' . toTagRep

token' :: Token -> Parsec [Token] () Token
token' t = satisfy (~== t) <?> show t

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
