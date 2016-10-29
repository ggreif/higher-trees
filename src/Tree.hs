{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- for Show only
{-# LANGUAGE RankNTypes, TypeInType, TypeOperators, KindSignatures, ViewPatterns #-}
{-# LANGUAGE TypeApplications, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DataKinds, GADTs, StandaloneDeriving, ScopedTypeVariables #-}

module Tree where

import Data.Kind
--import Control.Comonad
import Data.Foldable
--import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine
import Data.Type.Equality

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

-- can we simply always pass the presheaf?
-- this needs polymorphic recursion in the kinds!
-- we need to be totally explicit about the kinds,
-- so that no inference is attempted...
data STree n :: forall a . (a -> *) -> HTree n a -> * where
  SPoint :: f a -> STree Z f (Point a)
  SLeaf :: STree (S n) f Leaf
  SBranch :: f a -> STree n (STree (S n) f) stru -> STree (S n) f (a `Branch` stru)
  --SBranch' :: (Payload (S n) (Payload n stru) ~ a) => f a -> STree n (STree (S n) f) stru -> STree (S n) f (a `Branch` stru)
  SBranch' :: Structure (S n) (a `Branch` stru) => f a -> STree n (STree (S n) f) stru -> STree (S n) f (a `Branch` stru)

type family Structure (n :: Peano) (stru :: HTree n a) :: Constraint where
  Structure Z (Point a) = ()
  Structure (S n) Leaf = ()
  Structure (S n) (a `Branch` stru :: HTree (S n) *) = Payload (S n) (Payload n stru) ~ a

s2h :: STree n f stru -> HTree n (f (Payload n stru))
s2h (SPoint a) = Point a
s2h SLeaf = Leaf
--s2h (a `SBranch` stru) = a `Branch` hmap s2h (s2h stru)
s2h (a `SBranch'` stru) = a `Branch` hmap s2h (s2h stru)




{-
type family Cod f a c :: 

--data XTree p c n :: forall a . (a ~ Cod f p c) => (a -> *) -> HTree n a -> * where
data XTree p c n :: forall a . (a -> *) -> HTree n a -> * where
  XPoint :: f s -> XTree a c Z f (Point s)
  XLeaf :: XTree a c (S n) f Leaf
--  XBranch :: f a -> XTree n (XTree (S n) f a) a' stru -> XTree (S n) f a (a `Branch` stru)
-}

data HTree' n :: forall a . a -> HTree n a -> * where
  Point' :: a -> HTree' Z a (Point x)
  Leaf' :: HTree' (S n) a Leaf
  Branch' :: a -> HTree'' n (HTree' (S n) a) stru -> HTree' (S n) a (x `Branch` stru)


-- similarly one that takes the presheaf
data HTree'' n :: forall a . (HTree (S n) a -> *) -> HTree n (HTree (S n) a) -> * where
  Point'' :: f i -> HTree'' Z f (Point i)
  Leaf'' :: HTree'' (S n) f Leaf
  Branch'' :: f i -> HTree'' n (HTree' (S n) a) stru -> HTree'' (S n) f (i `Branch` stru)


  ----Branch'' :: HTree' (S n) a i -> HTree'' n (HTree' (S n) a) stru -> HTree'' n (HTree' (S n) a) (i `Branch` stru)
  -- BUG REPORT NEEDED?  Branch'' :: {-(f ~ HTree' (S n) a) => -} f i -> HTree'' n f stru -> HTree'' n f stru

  Extrude0 :: f (t2 `Branch` Point Leaf) -> HTree'' Z f (Point struct1) -> HTree'' Z f (Point (t2 `Branch` Point struct1))
  Extrude :: f i -> HTree'' (S n) f struct -> HTree'' (S n) f (i `Branch` (n `Terminal` struct))

-- * Extrude0 tests

e0 = Extrude0 (Leaf' `Branch'` Point'' Leaf') (Point'' Leaf')
e0' = Extrude0 (Leaf' `Branch'` Point'' Leaf') e0

f1 :: HTree' (S Z) Char (Branch f1 (Point Leaf))
f1 = '1' `Branch'` (Point'' Leaf')
f1a :: HTree' (S Z) Char (Branch z (Point (Branch z (Point Leaf))))
f1a = 'a' `Branch'` (Point'' f1)
f1ab = 'b' `Branch'` (Point'' f1a)
f1abE = 'e' `Branch'` (Extrude0 f1 (Point'' f1ab))


g2 :: HTree' (S (S Z)) Char (Branch x ('Branch 'Leaf ('Point Leaf)))
g2 = '2' `Branch'` (Extrude Leaf' Leaf'')

-- * Terminal, etc. type families
-- give me an n-dim tree that holds a single (n+1)-dim tree
-- this is needed for extruding
-- it also corresponds to the terminal object
type family Terminal n (t :: HTree (S n) a) :: HTree n (HTree (S n) a) where
  Z `Terminal` t = Point t
  S n `Terminal` t = t `Branch` Empty n

-- initial object does not exist in 0-dim:
type family Initial n :: HTree (S n) a where
  Initial n = Leaf

-- the empty tree of trees in the succeeding dimension
-- i.e. it has no place for an 'a'
type family Empty n :: HTree n (HTree (S n) a) where
  Empty Z = Point Leaf
  Empty (S n) = Leaf


data Tidden :: Peano -> (a -> *) -> * where
  Tide :: STree n f s -> Tidden n f

-- * now convert!

toTidden :: HTree n (f a) -> Tidden n f
toTidden (Point a) = Tide (SPoint a)
toTidden Leaf = Tide SLeaf
toTidden (a `Branch` (nest . hmap toTidden -> Tide stru)) = Tide $ a `SBranch` stru

nest :: HTree m (Tidden (S m) f) -> Tidden m (STree ('S m) f)
nest (Point (Tide st)) = Tide (SPoint st)
nest Leaf = Tide SLeaf
nest (Tide a `Branch` (nest . hmap nest -> Tide tr)) = Tide $ a `SBranch` tr

hmap :: (x -> y) -> HTree n x -> HTree n y
hmap f (Point a) = Point (f a)
hmap f Leaf = Leaf
hmap f (a `Branch` tr) = f a `Branch` hmap (hmap f) tr

-- ** Track codimension while converting
-- esp. only hide the singleton index, don't hide the leaf type
-- Correction: STree *is* parametrized in 'a', when it stores the 'f a'.
--             Just look at the singleton index.

type family Payload (n :: Peano) (s :: HTree n x) where
  Payload Z (Point a) = a
  Payload (S n) (a `Branch` stru) = a

{-
data TiddenC :: * -> Peano -> (a -> *) -> * where
  --TideC :: (f (Payload n s) ~ a) => STree n f s -> TiddenC a n f
  TideC :: STree n f s -> TiddenC (f (Payload n s)) n f


toTiddenC :: HTree n (f a) -> TiddenC (f a) n f
toTiddenC (Point a) = TideC (SPoint a)
toTiddenC Leaf = undefined -- TODO: TideC SLeaf
toTiddenC (a `Branch` (nest . hmap toTidden -> Tide stru)) = TideC $ a `SBranch` stru

{-
nest :: HTree m (Tidden (S m) f) -> Tidden m (STree ('S m) f)
nest (Point (Tide st)) = Tide (SPoint st)
nest Leaf = Tide SLeaf
nest (Tide a `Branch` (nest . hmap nest -> Tide tr)) = Tide $ a `SBranch` tr
-}

fromTiddenC :: forall f a n . TiddenC (f a) n f -> HTree n (f a)
fromTiddenC (TideC (SPoint a)) = Point a
fromTiddenC (TideC SLeaf) = Leaf
fromTiddenC (TideC (a `SBranch` stru)) = a `Branch` wumm stru
  where wumm :: (S m ~ n) => STree m (STree n f) stru
             -> HTree m (HTree n (f a))
        wumm (SPoint SLeaf) = Point Leaf -- Point(fromTiddenC (_ a))
        --wumm (SPoint (a `SBranch` stru)) = Point (a `Branch` wumm stru)
{-fromTiddenC (TideC (a `SBranch` stru)) = (a `Branch` hmap (fromTiddenC . wrap) (fromTiddenC . TideC $ stru))
  where wrapB :: (a ~ a', n ~ S m) => STree (S m) f (a' `Branch` stru) -> TiddenC (f a) n f
        wrapB t = TideC t
        wrap :: (n ~ S m) => STree (S m) f stru -> TiddenC (f a) n f
        wrap t@(a `SBranch` _) = _ t
        -}
-}



type family Castable co (a' :: i') (f :: i -> *) (a :: i) :: Constraint where
  Castable Z a' f a = f a ~ f a' -- TODO is (~) enough?
  Castable (S co) a' f a = () -- f a ~ f a'

-- and back...
data STreeA co n :: forall a . a' -> (a -> *) -> HTree n a -> * where
  SPointA :: Castable co a' f a => f a -> STreeA co Z a' f (Point a)
  SLeafA :: STreeA co (S n) a' f Leaf
  SBranchA :: Castable co a' f a => f a -> STreeA (S co) n a' (STreeA co (S n) a' f) stru -> STreeA co (S n) a' f (a `Branch` stru)

data TiddenA :: Peano -> a -> Peano -> (a -> *) -> * where
  TideA :: STreeA co n a f s -> TiddenA co a n f 
  TideA' :: STreeA (S co) n a (STreeA co (S n) a f)  s -> TiddenA (S co) a n f 


fromTidden :: TiddenA Z a n f -> HTree n (f a)
fromTidden (TideA (SPointA a)) = Point a -- in codimension 0 we should be able to cast!
fromTidden (TideA SLeafA) = Leaf
fromTidden (TideA (a `SBranchA` stru)) = (a `Branch` hmap fromTidden (fromTidden' (TideA' stru)))

fromTidden' :: TiddenA (S Z) a n f -> HTree n (TiddenA Z a (S n) f)
--fromTidden' (TideA' (SPointA a)) = Point a
fromTidden' (TideA' SLeafA) = Leaf
--fromTidden' (TideA' (a `SBranchA` stru)) = undefined --(a `Branch` hmap fromTidden' (fromTidden'' (TideA' stru)))

fromTidden'' :: TiddenA (S (S Z)) a n f -> HTree n (TiddenA (S Z) a (S n) f)
fromTidden'' = undefined


type family Raise n f a where
  Raise Z f a = f a
  Raise (S n) f a = Raise n f (f a)
{-

type family Castable n (a' :: i') (f :: i -> *) (a :: i) :: Constraint where
  Castable Z a' f a = f a ~ f a'
  Castable (S n) a' (STreeA n' a' (S n) f) a = ()



fromTidden :: TiddenA n f Z a -> HTree n (f a)
fromTidden (TideA (SPointA a)) = Point a -- in codimension 0 we should be able to cast!
fromTidden (TideA SLeafA) = Leaf
fromTidden (TideA (a `SBranchA` tr)) = (a `Branch` hmap (fromTidden . TideA)  (fromTidden (TideA tr)))
-}

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
