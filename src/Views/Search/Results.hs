module Views.Search.Results where

import Prelude hiding (div, head, id)

import Data.Maybe (fromMaybe)
import Data.Text (Text, append)
import Kinopoisk.SearchUrl
import Text.Blaze.Html5 (Html, (!), a, button, div, form, h1, input, label, option, p, select, stringValue, text, textValue)
import Text.Blaze.Html5.Attributes (action, class_, for, href, id, method, multiple, name, type_, value)
import qualified Views.Layout
import Views.Utils (blaze)
import Web.Scotty (ActionM)

view :: Maybe Text -> ActionM ()
view movieTitle =
  blaze $
  Views.Layout.layout "Search" $ do
    div ! class_ "container" $ do
      div ! class_ "jumbotron" $ do
        h1 "Случайная выборка"
        p $ text $ "First found movie: " `append` fromMaybe "Not Found" movieTitle
        searchForm

-- from_year to_year type
searchForm :: Html
searchForm = do
  form ! action "/" ! method "get" $ do
    div ! class_ "form-group" $ do
      simpleInput "from_year" "from_year" "From year:"
      simpleInput "to_year" "to_year" "To year:"
      multiSelectList "type" "type" "Content type:" $
        map (\(type') -> (stringValue $ show type', getContentTypeTitle type')) contentTypeList
      submitButton "Search"
  where
    simpleInput inputName inputId inputLabel = do
      label ! for inputId $ inputLabel
      input ! name inputName ! type_ "text" ! class_ "form-control" ! id inputId
    multiSelectList inputName inputId inputLabel values = do
      label ! for inputId $ inputLabel
      select ! name inputName ! class_ "form-control" ! id inputId $
        mapM_ (\(optId, optLabel) -> option ! value optId $ optLabel) values
    submitButton buttonLabel = input ! type_ "submit" ! class_ "btn btn-default" ! value buttonLabel
