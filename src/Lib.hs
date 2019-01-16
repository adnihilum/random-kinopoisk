module Lib where

import Data.Maybe (fromMaybe)
import Routes (routes)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty (get, html, scotty)

main :: IO ()
main = do
  -- получаем порт из системного окружения
  envPort <- (readMaybe =<<) <$> lookupEnv "Port"
  -- если переменная не задана, то делаем 3000 по умолчанию
  let port = fromMaybe 3000 envPort
  -- запускаем веб приложение
  scotty port routes
