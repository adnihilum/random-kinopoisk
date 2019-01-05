module Controllers.Search where

import qualified Views.Search.Results
import Web.Scotty

searchAction :: ActionM ()
searchAction = Views.Search.Results.view
