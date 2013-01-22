module Data.LinearProgram.Types where

-- import Control.DeepSeq

import Data.Monoid

data VarKind = ContVar | IntVar | BinVar deriving (Eq, Ord, Show, Read)

-- instance NFData VarKind

instance Monoid VarKind where
	mempty = ContVar
	mappend = max

data Direction = Min | Max deriving (Eq, Ord, Show, Read)

-- instance NFData Direction

data Bounds a =
	Free | LBound a | UBound a | Equ a | Bound a a deriving (Eq, Show, Read)

-- instance NFData (Bounds a)

-- Bounds form a monoid under intersection.
instance Ord a => Monoid (Bounds a) where
	mempty = Free
	Free `mappend` bd = bd
	bd `mappend` Free = bd
	Equ a `mappend` _ = Equ a
	_ `mappend` Equ a = Equ a
	LBound a `mappend` LBound b = LBound (max a b)
	LBound l `mappend` UBound u = Bound l u
	UBound u `mappend` LBound l = Bound l u
	LBound a `mappend` Bound l u = Bound (max a l) u
	Bound l u `mappend` LBound a = Bound (max a l) u
	UBound a `mappend` UBound b = UBound (min a b)
	UBound a `mappend` Bound l u = Bound l (min a u)
	Bound l u `mappend` UBound a = Bound l (min a u)
	Bound l u `mappend` Bound l' u' = Bound (max l l') (min u u')