module Kinopoisk.SearchUrl where

import Data.List
import Data.Text (Text, append, pack)

data Parameter
  = ParamFromYear Integer
  | ParamToYear Integer
  | ParamContentType ContentType
  | ParamPage Integer

data ContentType = ContentType
  { contentTypeCode :: Text
  , contentTypeTitle :: Text
  }

-- "https://www.kinopoisk.ru/s/type/film/list/1/m_act[from_year]/2004/m_act[to_year]/2018/m_act[genre][0]/3/m_act[genre][1]/13/m_act[genre][2]/19/m_act[type]/serial/"
contentTypeList =
  [ ContentType "film" "фильм"
  , ContentType "serial" "сериал"
  , ContentType "wallpaper" "обои"
  , ContentType "kadr" "фотографии"
  , ContentType "poster" "постеры"
  , ContentType "trailer" "трейлеры"
  , ContentType "product" "DVD"
  ]

getParameterName :: Parameter -> Text
getParameterName param = name
  where
    wrapMAct :: Text -> Text
    wrapMAct n = "m_act[" `append` n `append` "]"
    name :: Text
    name =
      case param of
        ParamFromYear {} -> wrapMAct "from_year"
        ParamToYear {} -> wrapMAct "to_year"
        ParamContentType {} -> wrapMAct "type"
        ParamPage {} -> "page"

parameterToText :: Parameter -> Text
parameterToText parameter = name `append` "/" `append` value
  where
    name = getParameterName parameter
    value =
      case parameter of
        ParamFromYear year -> pack $ show year
        ParamToYear year -> pack $ show year
        ParamContentType ContentType {contentTypeCode = code} -> code

buildUrl :: [Parameter] -> Text
buildUrl params = prefixUrl `append` mconcat paramsText
  where
    prefixUrl = "https://www.kinopoisk.ru/s/type/film/list/1/"
    paramsText = intersperse "/" $ map parameterToText params
