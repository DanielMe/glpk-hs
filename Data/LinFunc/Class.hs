{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Data.LinFunc.Class where

infixr 4 ^+^
infixr 4 ^-^
infixr 6 *^

-- | In algebra, if @r@ is a ring, an @r@-module is an additive group with a scalar multiplication
-- operation.  When @r@ is a field, this is equivalent to a vector space.
class Module r m | m -> r where
	(*^) :: r -> m -> m
	zero :: m
	(^+^) :: m -> m -> m
	(^-^) :: m -> m -> m
	neg :: m -> m
	
	a ^-^ b = a ^+^ neg b
	neg a = zero ^-^ a