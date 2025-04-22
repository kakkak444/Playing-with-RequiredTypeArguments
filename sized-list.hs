{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Prelude hiding (head, tail)

import GHC.TypeLits hiding (natVal', natVal)
import Data.Proxy
import Data.Type.Equality
import Data.Type.Natural hiding (natVal')
import Data.Type.Natural.Lemma.Order
import Data.Type.Natural.Lemma.Arithmetic
import Proof.Propositional (IsTrue(..), withWitness, Not)

infixr 5 `Cons`

data Vector (n :: Natural) a where
    Nil :: Vector 0 a
    Cons :: a -> Vector n a -> Vector (n + 1) a

deriving instance (Show a) => Show (Vector n a)

head :: Vector (n + 1) a -> a
head (Cons x _) = x

tail :: Vector (n + 1) a -> Vector n a
tail (Cons _ xs) = xs

singleton :: a -> Vector 1 a
singleton a = Cons a Nil

generate :: forall (n :: Natural) -> (Natural -> a) -> (KnownNat n) => Vector n a
generate n f = generate' 0 n f

generate' :: forall (i :: Natural) -> forall (n :: Natural) -> (Natural -> a) -> (KnownNat i, KnownNat n, i <= n) => Vector (n - i) a
generate' i n f =
    case leqToCmp sNati sNatn Witness of
        Left  Refl -> Nil
        Right Refl -> withWitness (ltToSuccLeq sNati sNatn Refl) $
                            Cons (f $ natVal sNati) $ generate' (type (i + 1)) n f
  where
    sNati = SNat @i
    sNatn = SNat @n

