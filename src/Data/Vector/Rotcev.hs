{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Vector.Rotcev
  ( Rotcev(..)
  , MRotcev(..)
  , reverse
  , mreverse
  ) where

import Prelude hiding (reverse)
import Data.Function
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV

data Rotcev v a
  = Forward  !(v a)
  | Backward !(v a)
  deriving (Show)

fromRotcev :: Rotcev v a -> v a
fromRotcev = \case
  Forward  v -> v
  Backward v -> v
{-# INLINE fromRotcev #-}

reverse :: Rotcev v a -> Rotcev v a
reverse = \case
  Forward  v -> Backward v
  Backward v -> Forward  v
{-# INLINE reverse #-}

data MRotcev v s a
  = MForward  !(V.Mutable v s a)
  | MBackward !(V.Mutable v s a)

fromMRotcev :: MRotcev v s a -> V.Mutable v s a
fromMRotcev = \case
  MForward  v -> v
  MBackward v -> v
{-# INLINE fromMRotcev #-}

mreverse :: MRotcev v s a -> MRotcev v s a
mreverse = \case
  MForward  v -> MBackward v
  MBackward v -> MForward  v
{-# INLINE mreverse #-}

type instance V.Mutable (Rotcev v) = MRotcev v

instance MV.MVector (V.Mutable v) a => MV.MVector (MRotcev v) a where
  basicLength = MV.basicLength . fromMRotcev
  basicUnsafeSlice off len = \case
    MForward  v -> MForward  $ MV.basicUnsafeSlice off len v
    MBackward v -> MBackward $ MV.basicUnsafeSlice (MV.basicLength v - off - len) len v
  basicOverlaps = MV.basicOverlaps `on` fromMRotcev
  basicUnsafeNew = fmap MForward . MV.basicUnsafeNew
  basicInitialize = MV.basicInitialize . fromMRotcev
  basicUnsafeRead = \case
    MForward  v -> MV.basicUnsafeRead v
    MBackward v -> MV.basicUnsafeRead v . ((MV.basicLength v - 1) -)
  basicUnsafeWrite = \case
    MForward  v -> MV.basicUnsafeWrite v
    MBackward v -> MV.basicUnsafeWrite v . ((MV.basicLength v - 1) -)
  {-# INLINE basicLength      #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps    #-}
  {-# INLINE basicUnsafeNew   #-}
  {-# INLINE basicInitialize  #-}
  {-# INLINE basicUnsafeRead  #-}
  {-# INLINE basicUnsafeWrite #-}

instance V.Vector v a => V.Vector (Rotcev v) a where
  basicUnsafeFreeze = \case
    MForward  v -> Forward  <$> V.basicUnsafeFreeze v
    MBackward v -> Backward <$> V.basicUnsafeFreeze v
  basicUnsafeThaw = \case
    Forward  v -> MForward  <$> V.basicUnsafeThaw v
    Backward v -> MBackward <$> V.basicUnsafeThaw v
  basicLength = V.basicLength . fromRotcev
  basicUnsafeSlice off len = \case
    Forward  v -> Forward  $ V.basicUnsafeSlice off len v
    Backward v -> Backward $ V.basicUnsafeSlice (V.basicLength v - off - len) len v
  basicUnsafeIndexM = \case
    Forward  v -> V.basicUnsafeIndexM v
    Backward v -> V.basicUnsafeIndexM v . ((V.basicLength v - 1) -)
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}