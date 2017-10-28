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
  cost <- case ehouse of
    Left err -> print err
    Right house -> aggregateCosts house
  print cost
  -- case development of
  --   (Left error) -> print "error"
  --   Just v -> print v
  -- print development
  -- cost <- calculateCost development
  -- putStrLn $ (++) "Total Estimated Cost: " $ show cost
  -- putStrLn $ (++) "Total Estimated Revenue: " $ show revenue

