module ParamParsers where

import Data.Maybe
import Data.Text.Lazy
import Web.Scotty

data ParamParseError =
  ParamParseError String

getParam :: (Read a) => Text -> ActionM a
getParam name = getParamOrElse name undefined

getParamOrElse :: (Read a) => Text -> a -> ActionM a
getParamOrElse name defaultValue = do
  strMaybeValue <- lookup name <$> params
  let maybeValue = read . unpack <$> strMaybeValue
  return $ fromMaybe defaultValue maybeValue
