{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- for Show only
{-# LANGUAGE TypeInType, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DataKinds, GADTs, StandaloneDeriving #-}

module Tree where

import Data.Kind
--import Control.Comonad
import Data.Foldable
--import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine

-- * A rose tree
-- We'll want to map the top-dimensional part of our
-- higher-dimensional trees to these.
--
data RTree a = RLeaf | a `RBranch` [RTree a] deriving (Eq, Show)

-- corresponding singleton type
data RTree' :: RTree () -> * -> * where
  RLeaf' :: RTree' RLeaf a
  RNil' :: a -> RTree' (RBranch '() '[]) a -- no fan-ins yet
  RCons' :: RTree' b a -> RTree' (RBranch '() bs) a -> RTree' (RBranch '() (b ': bs)) a -- add a fan-in

deriving instance Show a => Show (RTree' i a)
-- ** We can hide the type index
data Hidden :: * -> * where
  Hide :: RTree' i a -> Hidden a

deriving instance Show a => Show (Hidden a)

-- now convert!

toHidden :: RTree a -> Hidden a
toHidden RLeaf = Hide RLeaf'
toHidden (a `RBranch` []) = Hide (RNil' a)
toHidden (a `RBranch` (t:ts)) = case toHidden t of
                                  Hide t' -> case toHidden (a `RBranch` ts) of
                                    Hide ts'@(RNil' _) -> Hide (t' `RCons'` ts')
                                    Hide ts'@(_ `RCons'` _) -> Hide (t' `RCons'` ts')
-- and back...
fromHidden :: Hidden a -> RTree a
fromHidden (Hide RLeaf') = RLeaf
fromHidden (Hide (RNil' a)) = a `RBranch` []
fromHidden (Hide (t `RCons'` ts)) = a `RBranch` (fromHidden (Hide t) : ts')
  where a `RBranch` ts' = fromHidden (Hide ts)


--instance Comonad RTree' (x `RBranch` tr) where
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
  --Drop :: HTree n a -> HTree (S (S n)) a
  Leaf :: HTree (S n) a
  Branch :: a -> HTree n (HTree (S n) a) -> HTree (S n) a


data HTree' n a :: HTree n a -> * where
  Point' :: a -> HTree' Z a (Point x)
  Leaf' :: HTree' (S n) a Leaf
  --Branch' :: a -> HTree' n (HTree (S n) a) stru -> HTree' (S n) a (Branch x stru)
  Branch' :: a -> HTree'' n (HTree' (S n) a) stru -> HTree' (S n) a (x `Branch` stru)


-- similarly one that takes the presheaf
data HTree'' n :: (HTree (S n) a -> *) -> HTree n (HTree (S n) a) -> * where
  --Point'' :: a -> HTree'' Z a (Point x)
  --Leaf'' :: HTree'' (S n) a Leaf
  ----Branch'' :: HTree' (S n) a i -> HTree'' n (HTree' (S n) a) stru -> HTree'' n (HTree' (S n) a) (i `Branch` stru)
  Branch'' :: {-(f ~ HTree' (S n) a) => -} f i -> HTree'' n f stru -> HTree'' n f stru

  Cons :: f i -> HTree'' (S Z) f rest -> HTree'' (S Z) f (i `Branch` Point rest)

  --Extrude :: f i3 -> HTree'' (S (S Z)) f (j3 `Branch` list1) -> HTree'' (S (S Z)) f (i3 `Branch` ((j3 `Branch` Leaf) `Branch` Point list1))
  Extrude :: f i3 -> HTree'' (S (S Z)) f struct -> HTree'' (S (S Z)) f (i3 `Branch` (struct `Branch` Point Leaf))

-- give me an n-dim tree that holds a single (n+1)-dim tree
-- this is needed for extruding
-- it also corresponds to the terminal object
type family Terminal n (t :: HTree (S n) a) :: HTree n (HTree (S n) a) where
  Z `Terminal` t = Point t
  S Z `Terminal` t = t `Branch` Point Leaf
  S (S n) `Terminal` t = t `Branch` Leaf


data Tidden :: Peano -> * -> * where
  Tide :: HTree' n a s -> Tidden n a

-- now convert!

toTidden :: HTree n a -> Tidden n a
toTidden (Point a) = Tide (Point' a)
toTidden Leaf = Tide Leaf'

-- and back...
fromTidden :: Tidden n a -> HTree n a
fromTidden (Tide (Point' a)) = Point a
fromTidden (Tide Leaf') = Leaf


{- MAYBE LATER
data Zoom (n :: Peano) :: RTree () -> RTree () -> * -> * where
  Empty :: RTree' (RBranch '() os) a -> Zoom n (RBranch '() os) RLeaf a
  
-}

{-

h1 = Nil' 'a'
h1'1 = Nil' 'c' `Cons'` h1
h1'2 = Nil' 'b' `Cons'` h1'1
h1'3 = Leaf' `Cons'` h1'2
-}


-- *** =Show= ing
instance Show a => Show (HTree Z a) where
  show (Point a) = "Point (" ++ show a ++ ")"

instance (Show a, Show (HTree n (HTree (S n) a))) => Show (HTree (S n) a) where
  show Leaf = "Leaf"
  show (a `Branch` t) = '(' : show a ++ " `Branch` " ++ show t ++ ")"


-- *** Equating
instance Eq a => Eq (HTree n a) where
  Point a == Point b = a == b
  Leaf == Leaf = True
  a `Branch` t == b `Branch` u = (a, t) == (b, u)

-- *** Mapping
instance Functor (HTree Z) where
  fmap f (Point a) = Point (f a)

instance Functor (HTree n) => Functor (HTree (S n)) where
  fmap _ Leaf = Leaf
  fmap f (a `Branch` t) = f a `Branch` fmap (fmap f) t

-- *** A few specimen
t2, t21, t213 :: HTree (S Z) Int
t2 = 1 `Branch` Point Leaf
t21 = 1 `Branch` Point (2 `Branch` Point Leaf)
t213 = 1 `Branch` Point (2 `Branch` Point (3 `Branch` Point Leaf))
-- ILLEGAL: t21x = 1 `Branch` (2 `Branch` Point Leaf)

t3, t313 :: HTree (S (S Z)) Char
t3 = 'a' `Branch` (('b' `Branch` Leaf) `Branch` Point Leaf)
t313 = 'a' `Branch` (Leaf `Branch` Point (('b' `Branch` Leaf) `Branch` Point (Leaf `Branch` Point Leaf)))

{-
ti2 :: HTree' (S Z) Int (w `Branch` Point Leaf)
ti2 = 1 `Branch'` Point' Leaf
ti21 :: HTree' ('S 'Z) Integer (w `Branch` Point x)
ti21 = 1 `Branch'` Point' (2 `Branch` Point Leaf)
ti213 :: HTree' ('S 'Z) Integer (w `Branch` Point x)
ti213 = 1 `Branch'` Point' (2 `Branch` Point (3 `Branch` Point Leaf))
-}


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
  roseMap _ Leaf = RLeaf
  roseMap f (a `Branch` t) = f a `RBranch` roses
    where roses = (foldMap ((:[]) . roseMap f) t)


instance Foldable (HTree Z) where
  foldMap f (Point a) = f a `mappend` mempty

instance Foldable (HTree n) => Foldable (HTree (S n)) where
  foldMap _ Leaf = mempty
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
  -- duplicate l@Leaf = undefined `Branch` _

-- * Decorations
-- Eric Finster calls these /bonds/ in http://opetopic.net/docs/diagrams/complexes

-- ** Add a card under the whole tree (all its nodes)

-- * Complexes
-- these are /categories without identity/

-- ** =data= definition
-- for now just chain by dimension,
-- later we'll want to /bond/ by tree shapes
data Complex :: (Peano -> * -> *) -> Peano -> Peano -> * where
  Comp :: Complex HTree m n -> HTree n a -> Complex HTree m (S n)

-- we'll also want to concatenate, etc. (regarding a complex as a cell?)


-- * Visualize
{-
-- left side of each zoom is a graphical tree
visualizeL :: (Diagram B ~ a) => HTree (S Z) a -> Diagram B
visualizeL = go
  where go :: (Diagram B ~ a) => HTree (S Z) a -> Diagram B
        go Leaf = circle 10
        go (img `Branch` ts) = foldMap go ts === strut unitY === (img <> regPoly 4 30)

-- right side of each zoom is a stack of cards
-- but they are actually come from the dimension above
visualizeR :: HTree (S Z) (Diagram B) -> Diagram B
visualizeR tr = toDiag (go tr)
  where go :: HTree (S Z) (Diagram B) -> Sum Int
        go Leaf = mempty
        --go (img `Branch` ts) = foldMap go ts # frame 5 <> regPoly 4 30
        go (img `Branch` ts) = foldMap go ts <> Sum 1
        toDiag :: Sum Int -> Diagram B
        toDiag (Sum 0) = circle 2 # fc yellow
        toDiag (Sum n) = regPoly 4 (fromIntegral n * 10) <> toDiag (Sum $ n - 1)

-- stacking l above r with operation (|=|)
--data Cards = Cards { (|=|) :: Diagram B -> Diagram B -> Diagram B, l, r :: Diagram B }
data Cards = Cards { l, r :: Diagram B }

--instance Semigroup Cards where
  --

instance Monoid Cards where
  mempty = Cards mempty mempty
  mappend = undefined -- Cards

main = mainWith ((visualizeR $ (const (circle 3 # fc black)) <$> t213) ||| strutX 4 ||| (visualizeL $ (const (circle 3 # fc black)) <$> t213))
-}
