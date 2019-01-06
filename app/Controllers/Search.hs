module Controllers.Search where

import Data.List
import Data.Maybe (fromMaybe, maybe)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Views.Search.Results
import Web.Scotty

searchAction :: ActionM ()
searchAction = do
  params <- params
  let query = toStrict $ fromMaybe "" $ lookup "q" params
  Views.Search.Results.view query
