module Views.Search.Results where

import Prelude hiding (div, head, id)

import Text.Blaze.Html5 ((!), a, div, h1, p)
import Text.Blaze.Html5.Attributes (class_, href, id)
import qualified Views.Layout
import Views.Utils (blaze)
import Web.Scotty (ActionM)

view :: ActionM ()
view =
  blaze $
  Views.Layout.layout "Search" $ do
    div ! class_ "container" $ do
      div ! class_ "jumbotron" $ do
        h1 "Scotty Starter"
        p "Welcome to the Scotty Starter template, equipped with Twitter Bootstrap 3.0 and HTML5 boilerplate"
        p $ do
          a ! class_ "btn btn-lg btn-primary" ! id "fb" ! href "#navbar" $ "Facebook"
          a ! class_ "btn btn-lg btn-danger" ! id "gmail" ! href "#navbar" $ "Gmail"
