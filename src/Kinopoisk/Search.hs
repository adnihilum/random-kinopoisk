module Kinopoisk.Search where

import Codec.Text.IConv (convert)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Kinopoisk.SearchUrl
import Network.HTTP.Simple
import Text.HTML.TagSoup

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
  let titleTags =
        takeWhile (~/= ("</a>" :: String)) .
        dropWhile (~/= ("<a class=js-serp-metrika>" :: String)) .
        dropWhile (~/= ("<p class=name>" :: String)) . dropWhile (~/= ("<div class=info>" :: String)) $
        allTags
  putStrLn $ "titleTags= " ++ show titleTags
  let movieTitles = innerText titleTags
  return $ decodeUtf8 . convert "cp1251" "utf8" <$> Just movieTitles
