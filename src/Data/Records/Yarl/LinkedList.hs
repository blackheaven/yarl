{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Records.Yarl.LinkedList
  ( Record (..),
    Field (..),
    HasField (..),
  )
where

import Data.Kind
import GHC.Records
import GHC.TypeLits

type family HasNotField (target :: Symbol) (names :: [Type]) :: Constraint where
  HasNotField x '[] = ()
  HasNotField x (Field x v ': ys) = TypeError ('Text "Field already declared: " ':<>: 'ShowType x)
  HasNotField x (Field y v ': ys) = HasNotField x ys

newtype Field (name :: Symbol) a = Field {fieldValue :: a}

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
