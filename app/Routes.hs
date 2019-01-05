module Routes where

import Controllers.Search
import Web.Scotty

routes :: ScottyM ()
routes = do
  get "/" searchAction
