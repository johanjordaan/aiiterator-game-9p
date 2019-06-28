{-# LANGUAGE DeriveGeneric #-}
module Person where

import Data.Aeson
import GHC.Generics

data Person = Person {
      name :: String
    , age  :: Int
    } deriving (Generic, Show)

instance ToJSON Person where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Person
    -- No need to provide a parseJSON implementation.



data Bound = Bound { lower::Int, upper::Int} deriving (Generic, Show)
type Bounds = [Bound]

instance ToJSON Bound
