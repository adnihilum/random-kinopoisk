module Lib where

import Data.Maybe (fromMaybe)
import Routes (routes)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty (get, html, scotty)

main :: IO ()
main = do
  envPort <- (readMaybe =<<) <$> lookupEnv "Port"
  let port = fromMaybe 3000 envPort
  scotty port routes
