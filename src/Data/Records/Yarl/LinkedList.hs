{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module        : Data.Records.Yarl.LinkedList
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : not portable
--
-- Provide a simple record library working with 'HasField'
--
-- Example:
--
-- > import Data.Records.Yarl.LinkedList
-- >
-- > type Person = Record '[Field "name" String, Field "age" Int]
-- >
-- > marvin :: Person
-- > marvin = Field "marvin" :> Field 42 :> RNil
-- >
-- > desc :: Person -> String
-- > desc p = "My name is " <> p.name <> " and I'm " <> show p.age
--
module Data.Records.Yarl.LinkedList
  ( Record (..),
    Field (..),
    HasNotField (..),
    HasField (..),
  )
where

import Data.Kind
import GHC.Records
import GHC.TypeLits

-- | Watch for field name duplication
type family HasNotField (target :: Symbol) (names :: [Type]) :: Constraint where
  HasNotField x '[] = ()
  HasNotField x (Field x v ': ys) = TypeError ('Text "Field already declared: " ':<>: 'ShowType x)
  HasNotField x (Field y v ': ys) = HasNotField x ys

-- | Field container
newtype Field (name :: Symbol) a = Field {fieldValue :: a}

-- | Full extensible record
data Record :: [Type] -> Type where
  RNil :: Record '[]
  (:>) :: HasNotField fieldName fields => Field fieldName a -> Record fields -> Record (Field fieldName a : fields)

infixr 5 :>

instance {-# OVERLAPS #-} HasField fieldName (Record (Field fieldName a ': otherFields)) a where
  getField (Field x :> _) = x
  {-# INLINE getField #-}

instance
  HasField fieldName (Record otherFields) a =>
  HasField fieldName (Record (headField ': otherFields)) a
  where
  getField (_ :> otherFields) = getField @fieldName otherFields
  {-# INLINE getField #-}
