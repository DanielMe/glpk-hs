{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Data.LinearProgram.LinFunc (LinFunc, Module(..), var, varSum, (*&), vsum, combination, linCombination) where

import Control.Monad

import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Ratio
import Data.Array.Base
import Data.Array.IArray
-- import Data.Array.Unboxed

-- import Data.LinFunc.TH
import Data.LinearProgram.LinFunc.Class

-- | @'LinFunc' v c@ is a linear combination of variables of type @v@ with coefficients
-- from @c@.  Formally, this is the free @c@-module on @v@.  
type LinFunc = M.Map

instance Module Int Int where
	(*^) = (*)
	zero = 0
	(^+^) = (+)
	(^-^) = (-)
	neg = negate

instance Module Double Double where
	(*^) = (*)
	zero = 0
	(^+^) = (+)
	(^-^) = (-)
	neg = negate

instance Module Integer Integer where
	(*^) = (*)
	zero = 0
	(^+^) = (+)
	(^-^) = (-)
	neg = negate


instance Integral a => Module (Ratio a) (Ratio a) where
	{-# SPECIALIZE instance Module Rational Rational #-}
	{-# SPECIALIZE instance Module (Ratio Int) (Ratio Int) #-}
	(*^) = (*)
	zero = 0
	(^+^) = (+)
	(^-^) = (-)
	neg = negate

instance Module r m => Module r (a -> m) where
	(*^) = fmap . (*^)
	zero = const zero
	(^+^) = liftM2 (^+^)
	(^-^) = liftM2 (^-^)
	neg = fmap neg

instance (Ord k, Module r m) => Module r (M.Map k m) where
	(*^) = fmap . (*^)
	zero = M.empty
	(^+^) = M.unionWith (^+^)
	neg = fmap neg

instance Module r m => Module r (IM.IntMap m) where
	(*^) = fmap . (*^)
	zero = IM.empty
	(^+^) = IM.unionWith (^+^)
	neg = fmap neg
	
instance (Module r m) => Module r (Array Int m) where
	(*^) = amap . (*^)
	zero = listArray (0,0) [zero]
	a ^+^ b	| numElements a >= numElements b
			= accum (^+^) a (assocs b)
		| otherwise
			= accum (^+^) b (assocs a)
	a ^-^ b | numElements a >= numElements b
			= accum (^-^) a (assocs b)
		| otherwise
			= accum (^-^) b (assocs a)
	neg = amap neg

instance (IArray UArray m, Module r m) => Module r (UArray Int m) where
	(*^) = amap . (*^)
	zero = listArray (0,0) [zero]
	a ^+^ b	| numElements a >= numElements b
			= accum (^+^) a (assocs b)
		| otherwise
			= accum (^+^) b (assocs a)
	a ^-^ b | numElements a >= numElements b
			= accum (^-^) a (assocs b)
		| otherwise
			= accum (^-^) b (assocs a)
	neg = amap neg

-- | Given a variable @v@, returns the function equivalent to @v@.
var :: (Ord v, Num c) => v -> LinFunc v c
var v = M.singleton v 1

-- | @c '*&' v@ is equivalent to @c '*^' 'var' v@.
(*&) :: (Ord v, Num c) => c -> v -> LinFunc v c
c *& v = M.singleton v c

-- | Equivalent to @'vsum' . 'map' 'var'@.
varSum :: (Ord v, Num c) => [v] -> LinFunc v c
varSum vs = M.fromList [(v, 1) | v <- vs]

-- | Returns a vector sum.
vsum :: Module r v => [v] -> v
vsum = foldr (^+^) zero

-- | Given a collection of vectors and scaling coefficients, returns this
-- linear combination.
combination :: Module r m => [(r, m)] -> m
combination xs = vsum [r *^ m | (r, m) <- xs]

{-# INLINE linCombination #-}
-- | Given a set of basic variables and coefficients, returns the linear combination obtained
-- by summing.
linCombination :: (Ord v, Num r) => [(r, v)] -> LinFunc v r
linCombination xs = M.fromListWith (+) [(v, r) | (r, v) <- xs]