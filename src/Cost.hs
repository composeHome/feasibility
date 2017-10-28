{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Cost 
  ( aggregateCosts
  ) where

import Data.Aeson
import Data.Foldable
import Network.HTTP.Conduit (simpleHttp)
import Control.Monad
import House



-- homeAdvisorCostIds :: [(String, String)]
-- homeAdvisorCostIds =
--   [("83", "Foundation")
--   ,("167", "Electrical")
--   ]


homeAdvisorUrl :: String -> HomeAdvisorId -> String
homeAdvisorUrl zipcode costId = "https://www.homeadvisor.com/sm/cost/widget/updateGeo?r_username=cammlagoogle&r_accesskey=43xNWjRy&zipCode=" ++ zipcode ++ "&costGuideId=" ++ show costId


data LineItemCost = LineItemCost Int String deriving (Show)

instance FromJSON LineItemCost where
  parseJSON = withObject "line item" $ \o -> do
    widget <- o .: "costGuideWidgetDataHolder"
    (costs, region) <- asum [
      do
        localCosts <- widget .:? "localCostData"
        case localCosts of
          Nothing -> fail "no local cost"
          Just lc -> return (lc, "local"),
      do 
        regionCosts <- widget .:? "regionCostData"
        case regionCosts of
          Nothing -> fail "no region cost"
          Just rc -> return (rc, "region"),
      do
        nationalCosts <- widget .:? "nationalCostData"
        case nationalCosts of
          Nothing -> fail "no national cost"
          Just nc -> return (nc, "national") ]

    cost <- costs .: "avgCost"

    return (LineItemCost cost region)

getPrice :: LineItemCost -> Int
getPrice (LineItemCost cost _) = cost

getLineItemCost :: String -> HomeAdvisorId -> Int
getLineItemCost zipcode costId = do
  mlc <- fmap (fmap getPrice) $ fmap decode $ simpleHttp $ homeAdvisorUrl zipcode costId
  case mlc of
    -- In case of error parsing the JSON data, we report it.
    Nothing -> return 0
    -- Otherwise, we show the results in a human-readable way.
    Just v -> return v

calculateCost :: House -> Cost -> Int
calculateCost _ (FixedCost _ (Price price)) = price
calculateCost house (VariableCost _ (Price price) unit) = price * getHouseProperty unit house
calculateCost House {zipcode = z} (EstimateCost name homeAdId) = getLineItemCost z homeAdId
calculateCost house (Category _ (Subcosts subcosts)) = calculateCosts house subcosts

calculateCosts :: House -> [Cost] -> Int
calculateCosts house = sum . map (calculateCost house)

aggregateCosts :: House -> Int 
aggregateCosts house = calculateCosts house (costs house)


    -- lineItemCosts <- forM homeAdvisorCostIds (\(costId, name) -> do
    --   mc <- getLineItemCost zipcode costId
    --   case mc of
    --     -- In case of error parsing the JSON data, we report it.
    --     Nothing -> putStrLn $ "There was an error getting the " ++ show name ++ " line item cost."
    --     -- Otherwise, we show the results in a human-readable way.
    --     Just (LineItemCost cost level) -> print $ name ++ ": " ++ show cost ++ " (" ++ level ++ ")"
    --   return mc)
    -- print lineItemCosts

