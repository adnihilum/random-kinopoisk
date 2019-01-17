module Views.Search.Results where

import Prelude hiding (div, head, id)

import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text, append)
import Kinopoisk.SearchUrl

-- blaze-html - DSL для создание шаблонов html страниц
import Text.Blaze.Html5
  ( Html
  , (!)
  , a
  , button
  , div
  , form
  , h1
  , input
  , label
  , option
  , p
  , select
  , string
  , stringValue
  , text
  , textValue
  )
import Text.Blaze.Html5.Attributes (action, class_, for, href, id, method, multiple, name, selected, type_, value)
import qualified Views.Layout
import Views.Utils (blaze)
import Web.Scotty (ActionM)

type FormValues = (ContentType, Integer, Integer)

-- страница с результатами поиск (с формой)
searchResultsView :: FormValues -> Integer -> Maybe (Text, Text) -> ActionM ()
searchResultsView formValues foundTotalNum foundElement =
  blaze $
  Views.Layout.layout "Search" $ -- <-- вложеные do хорошо читаемы в отличии от скалы
   do
    div ! class_ "container" $ do
      div ! class_ "jumbotron" $ do
        h1 "Случайная выборка"
        p $ string $ "total found " ++ show foundTotalNum ++ " elements"
        case foundElement of
          Just (elementUrl, elementTitle) -> a ! href (textValue elementUrl) $ text elementTitle
          Nothing -> p "not found"
        searchForm formValues

-- форма поиск 
searchForm :: FormValues -> Html
searchForm (contentType, fromYear, toYear) = do
  form ! action "/" ! method "get" $ do
    div ! class_ "form-group" $ do
      simpleInput "from_year" "from_year" "From year:" $ show fromYear
      simpleInput "to_year" "to_year" "To year:" $ show toYear
      multiSelectList "type" "type" "Content type:" (show contentType) $
        -- удобно генерировать списки html элементов
        map (\type' -> (show type', getContentTypeTitle type')) contentTypeList
      submitButton "Search"
  where
    simpleInput inputName inputId inputLabel inputValue = do
      label ! for inputId $ inputLabel
      input ! name inputName ! type_ "text" ! class_ "form-control" ! id inputId ! value (stringValue inputValue)
    multiSelectList inputName inputId inputLabel selectedValue values = do
      label ! for inputId $ inputLabel
      select ! name inputName ! class_ "form-control" ! id inputId $
        mapM_
          (\(optId, optLabel) ->
             let option' = option ! value (stringValue optId)
                 option'' =
                   if optId == selectedValue
                     then option' ! selected ""
                     else option'
              in option'' optLabel)
          values
    submitButton buttonLabel = input ! type_ "submit" ! class_ "btn btn-default" ! value buttonLabel
