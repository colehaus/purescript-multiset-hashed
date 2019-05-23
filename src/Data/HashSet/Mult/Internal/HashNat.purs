module Data.HashSet.Multi.Internal.HashNat where

import Prelude

import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Natural (Natural, natToInt)
import Data.Newtype (class Newtype, un)

newtype HashNat = MkHashNat Natural
derive instance newtypeHashNat :: Newtype HashNat _
derive newtype instance eqHashNat :: Eq HashNat
derive newtype instance ordHashNat :: Ord HashNat
derive newtype instance showHashNat :: Show HashNat
derive newtype instance enumHashNat :: Enum HashNat
derive newtype instance semiringHashNat :: Semiring HashNat
derive instance genericHashNat :: Generic HashNat _
instance hashableNat :: Hashable HashNat where
  hash = natToInt <<< un MkHashNat
hashNatToInt :: HashNat -> Int
hashNatToInt = natToInt <<< un MkHashNat
