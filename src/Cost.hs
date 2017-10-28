{-# LANGUAGE DataKinds #-}

module Cost 
  ( aggregateCosts
  ) where

import House
-- import Debug.Trace

calculateCost :: House -> Cost -> Double 
-- calculateCost house cost | trace (show cost) False = undefined
calculateCost _ (FixedCost _ (Price price)) = price
calculateCost house (VariableCostRef _ (Price price) unit) = price * getHouseProperty unit house
calculateCost _ (VariableCost _ (Price price) (Units units)) = price * units
calculateCost house (Category _ (Subcosts subcosts)) = calculateCosts house subcosts

calculateCosts :: House -> [Cost] -> Double 
-- calculateCosts house costs | trace (show costs) False = undefined
calculateCosts house = sum . map (calculateCost house)

aggregateCosts :: House -> Double 
aggregateCosts house = calculateCosts house (costs house)
