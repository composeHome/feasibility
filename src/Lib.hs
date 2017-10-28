module Lib
    ( app 
    ) where

import System.Environment   
import Data.Yaml
import Cost
import House (House)

app :: IO ()
app = do
  (config:_) <- getArgs
  ehouse <- decodeFileEither config :: IO (Either ParseException House)
  case ehouse of
    Left err -> print err
    Right house -> print $ aggregateCosts house

