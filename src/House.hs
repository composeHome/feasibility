{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}


module House 
    ( House(..) 
    , Cost(..)
    , Price(..)
    , Subcosts(..)
    , Units(..)
    , getHouseProperty
    ) where

import Data.Yaml
import Data.Text (unpack)
import Data.Traversable
import Control.Applicative
import qualified Data.HashMap.Strict as HM
-- import Debug.Trace

newtype Name           = Name String          deriving Show
newtype Price          = Price Double         deriving Show
newtype Unit           = Unit String          deriving (Show, Eq)
newtype Units          = Units Double         deriving (Show, Eq)
newtype Subcosts       = Subcosts [Cost]      deriving Show

data Cost = FixedCost         Name Price
          | VariableCostRef   Name Price Unit
          | VariableCost      Name Price Units
          | Category          Name Subcosts
            deriving Show


getHouseProperty :: Unit -> (House -> Double)
getHouseProperty string
      | string == Unit "bedrooms"  = bedrooms
      | string == Unit "bathrooms" = bathrooms 
      | string == Unit "land_area" = land_area
      | string == Unit "rooms"     = rooms
      | otherwise                  = area


parseCost :: String -> Value -> Parser Cost 
-- parseCost o | trace ("parse cost" ++ show o) False = undefined
parseCost name =
  withObject "cost" $ \o -> do
    mprice <- o .:? "price"
    munitPrice <- o .:? "price_per_unit"
    munit <- o  .:? "unit"
    munits <- o  .:? "units"
    mcosts <- optional (parseCosts o)
    case (mprice, munitPrice, munit, munits, mcosts) of
      (_, Just unitPrice, Just unit, _, _)  -> return $ VariableCostRef (Name name) (Price unitPrice) (Unit unit)
      (_, Just unitPrice, _, Just units, _) -> return $ VariableCost (Name name) (Price unitPrice) (Units units)
      (Just price, _, _, _, _)              -> return $ FixedCost (Name name) (Price price)
      (_, _, _, _, Just costs')             -> return $ Category (Name name) (Subcosts costs')
      v                                     -> fail $ "could not parse cost for " ++ name ++ show v


parseCosts :: Object -> Parser [Cost]
-- parseCosts o | trace ("parse costs" ++ show o) False = undefined
parseCosts o = 
  for (HM.toList o) $ \(name, v) ->
    parseCost (unpack name) v



parseCostsField :: Object -> Parser [Cost]
parseCostsField o = case HM.lookup "costs" o of
                      Just (Object v) -> parseCosts v
                      Just _ -> fail "costs expected object"
                      Nothing -> fail "can't parse costs"


data House = House {
  client               :: String, 
  address              :: String,
  zipcode              :: Double,
  area                 :: Double,
  land_area            :: Double,
  bedrooms             :: Double,
  bathrooms            :: Double,
  rooms                :: Double,
  comparable_zillow_id :: String,
  costs                :: [Cost]
} deriving Show

instance FromJSON House where
  parseJSON (Object v) = 
    House <$>
    v .: "client" <*> 
    v .: "address" <*> 
    v .: "zipcode" <*> 
    v .: "area" <*> 
    v .: "land_area" <*> 
    v .: "bedrooms" <*> 
    v .: "bathrooms" <*> 
    v .: "rooms" <*>
    v .: "comparable_zillow_id" <*>
    parseCostsField v
  parseJSON _ = fail "Expected Object for House value"
