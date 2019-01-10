module Kinopoisk.SearchUrl where

import Data.List
import Data.Map (Map, (!), fromList, member)
import Data.Text.Lazy (Text, append, pack, unpack)
import qualified Network.URI.Encode as UE

data Parameter
  = ParamFromYear Integer
  | ParamToYear Integer
  | ParamContentType ContentType
  | ParamPage Integer
  | ParamPerPage Integer

data ContentType
  = ContentTypeFilm
  | ContentTypeSerial
  | ContentTypeWallpaper
  | ContentTypeSnapshot
  | ContentTypePoster
  | ContentTypeTrailer
  | ContentTypeProduct
  deriving (Enum, Bounded, Eq, Ord)

instance Show ContentType where
  show ContentTypeFilm = "film"
  show ContentTypeSerial = "serial"
  show ContentTypeWallpaper = "wallpaper"
  show ContentTypeSnapshot = "kadr"
  show ContentTypePoster = "poster"
  show ContentTypeTrailer = "trailer"
  show ContentTypeProduct = "product"

instance Read ContentType where
  readsPrec _ t =
    if t `member` types
      then [(types ! t, "")]
      else []
    where
      types = fromList [(show type', type') | type' <- [minBound :: ContentType ..]]

-- "https://www.kinopoisk.ru/s/type/film/list/1/m_act[from_year]/2004/m_act[to_year]/2018/m_act[genre][0]/3/m_act[genre][1]/13/m_act[genre][2]/19/m_act[type]/serial/"
getContentTypeTitle type' =
  case type' of
    ContentTypeFilm -> "фильм"
    ContentTypeSerial -> "сериал"
    ContentTypeWallpaper -> "обои"
    ContentTypeSnapshot -> "фотографии"
    ContentTypePoster -> "постеры"
    ContentTypeTrailer -> "трейлеры"
    ContentTypeProduct -> "DVD"

contentTypeList = [minBound :: ContentType ..]

getParameterName :: Parameter -> Text
getParameterName param = pack $ UE.encode $ unpack name
  where
    wrapMAct :: Text -> Text
    wrapMAct n = "m_act[" `append` n `append` "]"
    name :: Text
    name =
      case param of
        ParamFromYear {} -> wrapMAct "from_year"
        ParamToYear {} -> wrapMAct "to_year"
        ParamContentType {} -> wrapMAct "type"
        ParamPerPage {} -> "perpage"
        ParamPage {} -> "page"

parameterToText :: Parameter -> Text
parameterToText parameter = name `append` "/" `append` value
  where
    name = getParameterName parameter
    value =
      case parameter of
        ParamFromYear year -> pack $ show year
        ParamToYear year -> pack $ show year
        ParamContentType cType -> pack $ show cType
        ParamPerPage perPage -> pack $ show perPage
        ParamPage page -> pack $ show page

buildUrl :: [Parameter] -> Text
buildUrl params = prefixUrl `append` mconcat paramsText `append` "/"
  where
    prefixUrl = baseUrl `append` "/s/type/film/list/1/"
    paramsText = intersperse "/" $ map parameterToText params

baseUrl :: Text
baseUrl = "https://www.kinopoisk.ru"
