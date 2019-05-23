module Data.HashSet.Multi where

import Data.HashSet.Multi.Internal.HashNat
import Prelude

import Data.Array as Array
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Functor (map) as Functor
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (HashMap)
import Data.HashMap as Map
import Data.HashSet (HashSet)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Natural (Natural, intToNat, natToInt)
import Data.Natural as Nat
import Data.Newtype (class Newtype, un)
import Data.Newtype as Newtype
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

newtype MultiSet a = MkMultiSet (HashMap a HashNat)
instance foldableMultiSet :: Foldable MultiSet where
  foldMap f = foldMap f <<< toRepeatingArray
  foldr f a = foldr f a <<< toRepeatingArray
  foldl f a = foldl f a <<< toRepeatingArray
derive newtype instance eqMultiSet :: Eq a => Eq (MultiSet a)
derive instance eq1MultiSet :: Eq1 MultiSet
derive newtype instance hashableMultiSet :: Hashable a => Hashable (MultiSet a)
derive newtype instance monoidMultiSet :: Hashable a => Monoid (MultiSet a)
derive instance newtypeMultiSet :: Newtype (MultiSet a) _
instance semigroupMultiSet :: Hashable a => Semigroup (MultiSet a) where
  append = Newtype.over2 MkMultiSet (Map.unionWith (+))
derive instance genericMultiSet :: Generic (MultiSet a) _
instance showMultiSet :: Show a => Show (MultiSet a) where
  show = genericShow

empty :: forall a. MultiSet a
empty = MkMultiSet Map.empty

singleton :: forall a. Hashable a => a -> MultiSet a
singleton a = MkMultiSet (Map.singleton a one)

insert :: forall a. Hashable a => a -> MultiSet a -> MultiSet a
insert a = Newtype.over MkMultiSet (Map.insertWith (+) a one)

insertN :: forall a. Hashable a => Natural -> a -> MultiSet a -> MultiSet a
insertN n a = Newtype.over MkMultiSet (Map.insertWith (+) a $ Newtype.wrap n)

member :: forall a. Hashable a => a -> MultiSet a -> Boolean
member a = Map.member a <<< un MkMultiSet

count :: forall a. Hashable a => a -> MultiSet a -> Natural
count a = maybe zero (un MkHashNat) <<< Map.lookup a <<< un MkMultiSet

delete :: forall a. Hashable a => a -> MultiSet a -> MultiSet a
delete a (MkMultiSet m) =
  case natToInt <<< un MkHashNat <$> Map.lookup a m of
    Just 0 -> unsafeCrashWith "Key should always be deleted before reaching 0"
    Just 1 -> MkMultiSet $ Map.delete a m
    Just _ -> MkMultiSet $ Map.alter (Functor.map decHashNat) a m
    Nothing -> MkMultiSet m
  where
    decHashNat = Newtype.over MkHashNat (_ `Nat.minus` one)

fromFoldable :: forall f a. Foldable f => Hashable a => f a -> MultiSet a
fromFoldable = foldr insert empty

fromFoldableWithCounts ::
  forall f a.
  Foldable f => Hashable a =>
  f (Tuple a Natural) -> MultiSet a
fromFoldableWithCounts = MkMultiSet <<< Functor.map MkHashNat <<< Map.fromFoldable

toRepeatingArray :: forall a. MultiSet a -> Array a
toRepeatingArray =
  flip bind tupleToRepeats <<< Map.toArrayBy Tuple <<< un MkMultiSet
  where
    tupleToRepeats (Tuple a n) = Array.replicate (hashNatToInt n) a

toArray :: forall a. MultiSet a -> Array (Tuple a Natural)
toArray = Map.toArrayBy (\a n -> Tuple a (un MkHashNat n)) <<< un MkMultiSet

toSet :: forall a. MultiSet a -> HashSet a
toSet = unsafeCoerce <<< Functor.map (const unit) <<< un MkMultiSet

map :: forall a b. Hashable b => (a -> b) -> MultiSet a -> MultiSet b
map f = foldr (\x -> insert (f x)) empty

mapWithCount :: forall a b. Hashable b => (a -> Natural -> b) -> MultiSet a -> MultiSet b
mapWithCount f = foldr (\(Tuple a n) -> insertN n $ f a n) empty <<< toArray

filter :: forall a. (a -> Boolean) -> MultiSet a -> MultiSet a
filter f = Newtype.over MkMultiSet (Map.filterWithKey (\k v -> f k))

filterWithCount :: forall a. (a -> Natural -> Boolean) -> MultiSet a -> MultiSet a
filterWithCount f = Newtype.over MkMultiSet (Map.filterWithKey (\k v -> f k (un MkHashNat v)))

union :: forall a. Hashable a => MultiSet a -> MultiSet a -> MultiSet a
union = Newtype.over2 MkMultiSet (Map.unionWith (+))

intersection :: forall a. Hashable a => MultiSet a -> MultiSet a -> MultiSet a
intersection = Newtype.over2 MkMultiSet (Map.intersectionWith (+))

difference :: forall a. Hashable a => MultiSet a -> MultiSet a -> MultiSet a
difference (MkMultiSet m1) (MkMultiSet m2) =
  MkMultiSet $ intersect `Map.unionWith (+)` diff
    where
      diff = m1 `Map.difference` m2
      intersect =
        Functor.map intToHashNat <<< Map.filter (_ > 0) $
        Map.intersectionWith (-) (Functor.map hashNatToInt m1) (Functor.map hashNatToInt m2)
      intToHashNat = MkHashNat <<< intToNat

size :: forall a. MultiSet a -> Natural
size = un Additive <<< foldMapWithIndex (const $ Additive <<< un MkHashNat) <<< un MkMultiSet

isEmpty :: forall a. MultiSet a -> Boolean
isEmpty = Map.isEmpty <<< un MkMultiSet
