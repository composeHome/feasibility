{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}


module House 
    ( House(..) 
    , getHouseProperty
    , HomeAdvisorId
    , Price(..)
    , Cost(..)
    , Subcosts(..)
    ) where

import Data.Yaml
import Data.Text (unpack)
import Data.Traversable
import Control.Applicative
import qualified Data.HashMap.Strict as HM

newtype Name           = Name String          deriving Show
newtype Price          = Price Int            deriving Show
newtype Unit           = Unit String          deriving (Show, Eq)
newtype Subcosts       = Subcosts [Cost]      deriving Show
newtype HomeAdvisorId  = HomeAdvisorId String deriving Show

data Cost = FixedCost       Name Price
          | VariableCost    Name Price Unit
          | EstimateCost    Name HomeAdvisorId
          | Category        Name Subcosts
            deriving Show


getHouseProperty :: Unit -> (House -> Int)
getHouseProperty string
      | string == Unit "bedrooms"  = bedrooms
      | string == Unit "bathrooms" = bathrooms 
      | string == Unit "land_area" = land_area
      | otherwise             = area


parseCost :: String -> Value -> Parser Cost 
parseCost name =
  withObject "cost" $ \o -> do
    mprice <- o .:? "price"
    munitPrice <- o .:? "price_per_unit"
    munit <- o  .:? "unit"
    mhomeAdId <- o .:? "home_advisor_id"
    mcosts <- optional (parseCosts o)
    case (mprice, munitPrice, munit, mcosts, mhomeAdId) of
      (_, Just unitPrice, Just unit, _, _) -> return $ VariableCost (Name name) (Price unitPrice) (Unit unit)
      (Just price, _, _, _, _)             -> return $ FixedCost (Name name) (Price price)
      (_, _, _, _, Just homeAdId)          -> return $ EstimateCost (Name name) (HomeAdvisorId homeAdId)
      (_, _, _, Just costs', _)            -> return $ Category (Name name) (Subcosts costs')
      _                                    -> fail $ "could not parse cost for " ++ name


parseCosts :: Object -> Parser [Cost]
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
  zipcode              :: String,
  area                 :: Int,
  land_area            :: Int,
  bedrooms             :: Int,
  bathrooms            :: Int,
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
    v .: "comparable_zillow_id" <*>
    parseCostsField v
  parseJSON _ = fail "Expected Object for House value"
