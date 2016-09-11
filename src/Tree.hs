{-# LANGUAGE TypeInType, TypeOperators, KindSignatures #-}
{-# LANGUAGE DataKinds, GADTs #-}

module Tree where

import Data.Kind

data Peano = Z | S Peano

data HTree n a where
  Point :: a -> HTree Z a
  Leaf :: HTree (S n) a
  Branch :: a -> HTree n (HTree (S n) a) -> HTree (S n) a


data HTree' n a :: HTree n a -> * where
  Point' :: a -> HTree' Z a (Point x)
  Branch' :: a -> HTree'' n (HTree' (S n) a) stru -> HTree' (S n) a (x `Branch` stru)


data HTree'' n :: (HTree (S n) a -> *) -> HTree n (HTree (S n) a) -> * where


data Tidden :: Peano -> * -> * where
  Tide :: HTree' n a s -> Tidden n a

toTidden :: HTree n a -> Tidden n a
toTidden (a `Branch` stru) = Tide $ case toTidden stru of
                                      Tide (Point' Leaf) -> a `Branch'` a
