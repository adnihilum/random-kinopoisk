{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Environment (lookupEnv)
import Web.Scotty (scotty,get, html)


main :: IO ()
main = do
    envPort <- (readMaybe =<<) <$> lookupEnv "Port"
    let port = fromMaybe 3000 envPort

    scotty port $ do 
        get "/" $ html "<h1>Hello, world!</h1>"