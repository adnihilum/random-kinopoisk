module Controllers.Search where

import Data.List
import Data.Maybe (fromMaybe, listToMaybe, maybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Kinopoisk.Search (getFirstMovie)
import qualified Views.Search.Results
import Web.Scotty

searchAction :: ActionM ()
searchAction = do
  query <- (toStrict <$>) . lookup "q" <$> params
  title <- liftAndCatchIO getFirstMovie
  Views.Search.Results.view query title
