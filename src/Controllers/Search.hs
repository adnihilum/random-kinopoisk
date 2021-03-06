module Controllers.Search where

import Control.Monad (when)
import Data.List
import Data.Maybe (fromMaybe, listToMaybe, maybe)
import qualified Data.Text as T
import Data.Text.Lazy (Text, toStrict)
import Kinopoisk.Search (SearchParams(..), getElementsOnPage, getRandomElement)
import Kinopoisk.SearchUrl (ContentType(..))
import ParamParsers
import Views.Search.Results (searchResultsView)
import Web.Scotty

searchAction :: ActionM ()
searchAction = do
  -- получаем значения параметров запроса
  contentType <- getParamOrElse "type" ContentTypeFilm :: ActionM ContentType
  fromYear <- getParamOrElse "from_year" 2018 :: ActionM Integer
  toYear <- getParamOrElse "to_year" 2019 :: ActionM Integer
  -- валидируем параметры
  when (fromYear > toYear) $ error "fromYear should be less or equal than toYear"
  when (toYear - fromYear > 10) $ error "time span should be less than 10 years"
  -- получаем рандомное произведение с кинопоиска
  (foundTotalNum, foundElement) <- liftAndCatchIO $ getRandomElement (SearchParams contentType fromYear toYear 1)
  -- рендерим html
  searchResultsView (contentType, fromYear, toYear) foundTotalNum $
    (\(url, title) -> (toStrict url, toStrict title)) <$> foundElement
