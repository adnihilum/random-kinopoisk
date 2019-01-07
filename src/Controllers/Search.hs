module Controllers.Search where

import Data.List
import Data.Maybe (fromMaybe, maybe)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Views.Search.Results
import Web.Scotty

searchAction :: ActionM ()
searchAction = do
  query <- (toStrict <$>) . lookup "q" <$> params
  Views.Search.Results.view query
