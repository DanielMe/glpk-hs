{-# LANGUAGE RecordWildCards #-}

module Data.LinearProgram.LPMonad where

import Control.Monad.State.Strict

import Data.Map
import Data.Monoid
-- import Data.Bounds

import Data.LinFunc
import Data.LinearProgram.Types
import Data.LinearProgram.Spec

-- | A 'State' monad used for the construction of a linear program.
type LPM v c = State (LP v c)

-- | Constructs a linear programming problem, returning any
-- desired return value.
runLPM :: (Ord v, Module r c) => LPM v c a -> (a, LP v c)
runLPM m = runState m (LP Max zero [] mempty mempty)

-- | Constructs a linear programming problem.
execLPM :: (Ord v, Module r c) => LPM v c a -> LP v c
execLPM = snd . runLPM

-- | Sets the optimization direction of the linear program:
-- maximization or minimization.
setDirection :: Direction -> LPM v c ()
setDirection dir = modify (\ lp -> lp{direction = dir})

equal, leq, geq :: (Ord v, Module r c) => LinFunc v c -> LinFunc v c -> LPM v c ()
equal f g = equalTo (f ^-^ g) zero
leq f g = leqTo (f ^-^ g) zero
geq = flip leq

equal', leq', geq' :: (Ord v, Module r c) => String -> LinFunc v c -> LinFunc v c -> LPM v c ()
equal' lab f g = equalTo' lab (f ^-^ g) zero
leq' lab f g = leqTo' lab (f ^-^ g) zero
geq' = flip . leq'

equalTo, leqTo, geqTo :: LinFunc v c -> c -> LPM v c ()
equalTo f v = constrain f (Equ v)
leqTo f v = constrain f (UBound v)
geqTo f v = constrain f (LBound v)

equalTo', leqTo', geqTo' :: String -> LinFunc v c -> c -> LPM v c ()
equalTo' lab f v = constrain' lab f (Equ v)
leqTo' lab f v = constrain' lab f (UBound v)
geqTo' lab f v = constrain' lab f (LBound v)

varEq, varLeq, varGeq :: (Ord v, Ord c) => v -> c -> LPM v c ()
varEq v c = setVarBounds v (Equ c)
varLeq v c = setVarBounds v (UBound c)
varGeq v c = setVarBounds v (LBound c)

varBds :: (Ord v, Ord c) => v -> c -> c -> LPM v c ()
varBds v l u = setVarBounds v (Bound l u)

constrain :: LinFunc v c -> Bounds c -> LPM v c ()
constrain f bds = modify addConstr where
	addConstr lp@LP{..}
		= lp{constraints = Constr Nothing f bds:constraints}

constrain' :: String -> LinFunc v c -> Bounds c -> LPM v c ()
constrain' lab f bds = modify addConstr where
	addConstr lp@LP{..}
		= lp{constraints = Constr (Just lab) f bds:constraints}

setObjective :: LinFunc v c -> LPM v c ()
setObjective obj = modify setObj where
	setObj lp = lp{objective = obj}

addObjective :: (Ord v, Module r c) => LinFunc v c -> LPM v c ()
addObjective obj = modify addObj where
	addObj lp@LP{..}
		= lp {objective = obj ^+^ objective}
		
addWeightedObjective :: (Ord v, Module r c) => r -> LinFunc v c -> LPM v c ()
addWeightedObjective wt obj = addObjective (wt *^ obj)

setVarBounds :: (Ord v, Ord c) => v -> Bounds c -> LPM v c ()
setVarBounds var bds = modify addBds where
	addBds lp@LP{..} = lp{varBounds = insertWith mappend var bds varBounds}

setVarKind :: Ord v => v -> VarKind -> LPM v c ()
setVarKind v k = modify setK where
	setK lp@LP{..} = lp{varTypes = insertWith mappend v k varTypes}