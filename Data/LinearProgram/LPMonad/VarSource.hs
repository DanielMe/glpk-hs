{-# LANGUAGE UndecidableInstances, FlexibleInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses #-}
module Data.LinearProgram.LPMonad.VarSource (
	-- * Variable generation monad
	VarSource, evalVarSource, VarSourceT, evalVarSourceT, Var(..),
	MonadSource(..)) where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS
import qualified Control.Monad.State.Lazy as SL
import Control.Monad.Cont
-- import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.RWS.Class

import Data.Monoid

-- | A type suitable for use as a variable in linear programs.
newtype Var = Var Int deriving (Eq, Ord, Enum)
-- | A monad capable of generating unique variables. 
type VarSource = VarSourceT Identity
-- | A monad transformer capable of generating unique variables.  To generate variables while constructing a linear program in the 'LPT' monad, work in the monad @'LPT' 'Var' c 'VarSource'@ or @'LPT' 'Var' c ('VarSourceT' m)@.
newtype VarSourceT m a = VarSourceT {runVarSourceT :: StateT Var m a} deriving (Monad, MonadReader r, MonadWriter w, MonadIO, MonadFix,
	MonadTrans, MonadCont)

evalVarSource :: VarSource a -> a
evalVarSource = runIdentity . evalVarSourceT

evalVarSourceT :: Monad m => VarSourceT m a -> m a
evalVarSourceT m = evalStateT (runVarSourceT m) (Var 0)

instance Show Var where
	show (Var x) = "x_" ++ show x

instance Read Var where
	readsPrec _ ('x':'_':xs) = [(Var x, s') | (x, s') <- readsPrec 0 xs]
	readsPrec _ _ = []

instance MonadState s m => MonadState s (VarSourceT m) where
	put x = VarSourceT (lift (put x))
	get = VarSourceT (lift get)

-- instance MonadTrans VarSource where
-- 	lift 

-- | A type class for monads capable of repeatedly generating unique elements of a specified type.
class Monad m => MonadSource x m | m -> x where
	makeNew :: m x

instance Monad m => MonadSource Var (VarSourceT m) where
	makeNew = VarSourceT $ do	v <- get
					put (succ v)
					return v

instance MonadSource x m => MonadSource x (StateT s m) where
	makeNew = lift makeNew

instance MonadSource x m => MonadSource x (ReaderT r m) where
	makeNew = lift makeNew

instance (MonadSource x m, Monoid w) => MonadSource x (WL.WriterT w m) where
	makeNew = lift makeNew

instance (MonadSource x m, Monoid w) => MonadSource x (WS.WriterT w m) where
	makeNew = lift makeNew

instance MonadSource x m => MonadSource x (ContT r m) where
	makeNew = lift makeNew

instance MonadSource x m => MonadSource x (SL.StateT s m) where
	makeNew = lift makeNew