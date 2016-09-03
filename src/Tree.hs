{-# LANGUAGE UndecidableInstances #-} -- for Show only
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DataKinds, GADTs #-}

module Tree where

--import Control.Comonad
import Data.Foldable

-- * A stupid binary tree
-- what about considering the subtree at a certain position
-- being the element type of the tree?
-- Then we can always extract, no?

data Tree a where
  Leaf :: Tree a
  Fork :: Tree a -> Tree a -> Tree a
 deriving Functor

-- * A non-empty rose tree
-- We'll want to map the top-dimensional part of our
-- higher-dimensional trees to these.
--
data RTree a = RLeaf | a `RBranch` [RTree a] deriving (Eq, Show)

--instance Comonad RTree where
  --extract

-- * Higher tree
-- For motivation see: http://opetopic.net/docs/hdts

-- ** =Peano= naturals
-- We need them for keeping track of the dimensions.
-- Only to be used as a data kind.
--
data Peano = Z | S Peano

-- ** Our higher-dimensional tree type
-- =Points= (and only those) live in dimension zero
-- the other critters live in higher dimensions
--
data HTree n a where
  Point :: a -> HTree Z a
  HLeaf :: HTree (S n) a
  Branch :: a -> HTree n (HTree (S n) a) -> HTree (S n) a

-- *** =Show= ing
instance Show a => Show (HTree Z a) where
  show (Point a) = "Point (" ++ show a ++ ")"

instance (Show a, Show (HTree n (HTree (S n) a))) => Show (HTree (S n) a) where
  show HLeaf = "HLeaf"
  show (a `Branch` t) = '(' : show a ++ " `Branch` " ++ show t ++ ")"


-- *** Equating
instance Eq a => Eq (HTree n a) where
  Point a == Point b = a == b
  HLeaf == HLeaf = True
  a `Branch` t == b `Branch` u = (a, t) == (b, u)

-- *** Mapping
instance Functor (HTree Z) where
  fmap f (Point a) = Point (f a)

instance Functor (HTree n) => Functor (HTree (S n)) where
  fmap _ HLeaf = HLeaf
  fmap f (a `Branch` t) = f a `Branch` fmap (fmap f) t

-- *** A few specimen
t2, t21, t213 :: HTree (S Z) Int
t2 = 1 `Branch` Point HLeaf
t21 = 1 `Branch` Point (2 `Branch` Point HLeaf)
t213 = 1 `Branch` Point (2 `Branch` Point (3 `Branch` Point HLeaf))
-- ILLEGAL: t21x = 1 `Branch` (2 `Branch` Point HLeaf)

t3, t313 :: HTree (S (S Z)) Char
t3 = 'a' `Branch` (('b' `Branch` HLeaf) `Branch` Point HLeaf)
t313 = 'a' `Branch` (HLeaf `Branch` Point (('b' `Branch` HLeaf) `Branch` Point (HLeaf `Branch` Point HLeaf)))

-- ** TODO
-- - extrude
-- - sprout (pointer?)
-- - extrude drop

-- *** Top-dimensional extraction

top :: Roseable (HTree n) => HTree n a -> RTree a
top = roseMap id


class Roseable f where
  roseMap :: (a -> b) -> f a -> RTree b

instance Roseable (HTree Z) where
  roseMap f (Point a) = f a `RBranch` []
instance (Foldable (HTree n), Roseable (HTree n)) => Roseable (HTree (S n)) where
  roseMap _ HLeaf = RLeaf
  roseMap f (a `Branch` t) = f a `RBranch` roses
    where roses = (foldMap ((:[]) . roseMap f) t)


instance Foldable (HTree Z) where
  foldMap f (Point a) = f a `mappend` mempty

instance Foldable (HTree n) => Foldable (HTree (S n)) where
  foldMap _ HLeaf = mempty
  foldMap f (a `Branch` t) = f a `mappend` foldMap (foldMap f) t

-- *** Extrude

--extrude :: a -> HTree Z a -> HTree Z a
--extrude a t = a `Branch` 

-- *** Diagrams?

class HComonad hf where
  extract :: hf n a -> a
  duplicate :: hf (S n) a -> hf n (hf (S n) a)

instance HComonad HTree where
  --extract :: HTree n a -> a
  extract (Point a) = a
  extract (a `Branch` _) = a
  duplicate (_ `Branch` r) = r
  -- duplicate l@HLeaf = undefined `Branch` _


-- * Complexes
-- these are /categories without identity/

--data Complex :: (* -> * -> *) -> * -> * -> * where
--  Comp :: Complex (p f) (f m) (f n) -> p f (S n) -> Complex (p f) (f m) (f (S n))
