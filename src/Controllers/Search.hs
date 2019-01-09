module Controllers.Search where

import Control.Monad (when)
import Data.List
import Data.Maybe (fromMaybe, listToMaybe, maybe)
import qualified Data.Text as T
import Data.Text.Lazy (Text, toStrict)
import Kinopoisk.Search (getFirstMovie)
import Kinopoisk.SearchUrl (ContentType(..))
import ParamParsers
import qualified Views.Search.Results
import Web.Scotty

searchAction :: ActionM ()
searchAction = do
  contentType <- getParamOrElse "type" ContentTypeFilm :: ActionM ContentType
  fromYear <- getParamOrElse "from_year" 2018 :: ActionM Integer
  toYear <- getParamOrElse "to_year" 2019 :: ActionM Integer
  when (fromYear > toYear) $ error "fromYear should be less or equal than toYear"
  when (toYear - fromYear > 10) $ error "time span should be less than 10 years"
  (foundTotalNum, foundElements) <- liftAndCatchIO $ getFirstMovie contentType fromYear toYear
  Views.Search.Results.view foundTotalNum $ toStrict <$> foundElements
