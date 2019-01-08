module Kinopoisk.Search where

import Codec.Text.IConv (convert)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Simple
import qualified Network.URI.Encode as UE
import Text.HTML.TagSoup

getFirstMovie :: IO (Maybe Text)
getFirstMovie = do
  let url =
        "https://www.kinopoisk.ru/s/type/film/list/1/order/rating/" ++ UE.encode "m_act[genre][0]" ++ "/10/perpage/10/" :: String
  request <- parseRequest url
  response <- httpLBS request
  let body = getResponseBody response
  let allTags = parseTags body
  let titleTags =
        takeWhile (~/= ("</a>" :: String)) .
        dropWhile (~/= ("<a class=js-serp-metrika>" :: String)) .
        dropWhile (~/= ("<p class=name>" :: String)) . dropWhile (~/= ("<div class=info>" :: String)) $
        allTags
  putStrLn $ "titleTags= " ++ show titleTags
  let movieTitles = innerText titleTags
  return $ decodeUtf8 . LBS.toStrict . convert "cp1251" "utf8" <$> Just movieTitles
