module Views.Search.Results where

import Prelude hiding (div, head, id)

import Data.Text (Text, append)
import Text.Blaze.Html5 ((!), a, div, h1, p, text)
import Text.Blaze.Html5.Attributes (class_, href, id)
import qualified Views.Layout
import Views.Utils (blaze)
import Web.Scotty (ActionM)

view :: Text -> ActionM ()
view request =
  blaze $
  Views.Layout.layout "Search" $ do
    div ! class_ "container" $ do
      div ! class_ "jumbotron" $ do
        h1 "Scotty Starter"
        p $
          text $
          "Welcome to the Scotty Starter template, equipped with Twitter Bootstrap 3.0 and HTML5 boilerplate " `append`
          request
        p $ do
          a ! class_ "btn btn-lg btn-primary" ! id "fb" ! href "#navbar" $ "Facebook"
          a ! class_ "btn btn-lg btn-danger" ! id "gmail" ! href "#navbar" $ "Gmail"
