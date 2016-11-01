{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeInType #-}
{-# LANGUAGE DeriveFunctor, DataKinds, GADTs, StandaloneDeriving, ScopedTypeVariables #-}

module STree where

import Data.Singletons
import Data.Singletons.TH

-- * Higher tree
-- For motivation see: http://opetopic.net/docs/hdts

-- ** =Peano= naturals
-- We need them for keeping track of the dimensions.
-- Only to be used as a data kind.
--
$(singletons [d|data Peano = Z | S Peano|])

-- ** Our higher-dimensional tree type
-- =Points= (and only those) live in dimension zero
-- the other critters live in higher dimensions
--
$(singletons [d|
  data HTree n a where
    Point :: a -> HTree Z a
    --Drop :: HTree n a -> HTree (S (S n)) a
    Leaf :: HTree (S n) a
    Branch :: a -> HTree n (HTree (S n) a) -> HTree (S n) a
 |])
