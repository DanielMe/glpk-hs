{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

-- | A collection of operations that can be used to specify linear programming in a
-- simple, monadic way.  It is not too difficult to construct 'LP' values explicitly,
-- but this module may help simplify and modularize the construction of the linear program,
-- for example separating different families of constraints in the problem specification.
-- 
-- Many of these functions should be executed in either the @'LPM' v c@ or the @'LPT' v c 'IO'@ monad.
module Data.LinearProgram.LPMonad (
	module Data.LinearProgram.LPMonad.Internal,
	-- * Solvers
	quickSolveMIP,
	quickSolveLP,
	glpSolve,
	quickSolveMIP',
	quickSolveLP',
	glpSolve') where

import Control.Monad.State.Strict
import Control.Monad.Identity

import Data.Map
import Data.Monoid

import Data.LinearProgram.Common
import Data.LinearProgram.LPMonad.Internal

import Data.LinearProgram.GLPK.Solver
import Data.LinearProgram.GLPK.IO


{-# SPECIALIZE quickSolveLP :: (Ord v, Real c) => 
	LPT v c IO (ReturnCode, Maybe (Double, Map v Double)) #-}
{-# SPECIALIZE quickSolveMIP :: (Ord v, Real c) => 
	LPT v c IO (ReturnCode, Maybe (Double, Map v Double)) #-}
-- | Solves the linear program with the default settings in GLPK.  Returns the return code,
-- and if the solver was successful, the objective function value and the settings of each variable.
quickSolveLP, quickSolveMIP :: (Ord v, Real c, MonadState (LP v c) m, MonadIO m) => 
	m (ReturnCode, Maybe (Double, Map v Double))
quickSolveLP = glpSolve simplexDefaults
quickSolveMIP = glpSolve mipDefaults

{-# SPECIALIZE glpSolve :: (Ord v, Real c) => GLPOpts -> LPT v c IO (ReturnCode, Maybe (Double, Map v Double)) #-}
-- | Solves the linear program with the specified options in GLPK.  Returns the return code,
-- and if the solver was successful, the objective function value and the settings of each variable.
glpSolve :: (Ord v, Real c, MonadState (LP v c) m, MonadIO m) => GLPOpts -> m (ReturnCode, Maybe (Double, Map v Double))
glpSolve opts = get >>= liftIO . glpSolveVars opts

{-# SPECIALIZE quickSolveLP' :: (Ord v, Real c) => LPT v c IO (ReturnCode, Maybe (Double, Map v Double, [RowValue v c])) #-}
{-# SPECIALIZE quickSolveMIP' :: (Ord v, Real c) => LPT v c IO (ReturnCode, Maybe (Double, Map v Double, [RowValue v c])) #-}
-- | Solves the linear program with the default settings in GLPK.  Returns the return code,
-- and if the solver was successful, the objective function value, the settings of each variable, and the
-- value of each constraint/row.
quickSolveLP', quickSolveMIP' :: (Ord v, Real c, MonadState (LP v c) m, MonadIO m) => 
	m (ReturnCode, Maybe (Double, Map v Double, [RowValue v c]))
quickSolveLP' = glpSolve' simplexDefaults
quickSolveMIP' = glpSolve' mipDefaults

{-# SPECIALIZE glpSolve' :: (Ord v, Real c) => GLPOpts -> LPT v c IO (ReturnCode, Maybe (Double, Map v Double, [RowValue v c])) #-}
-- | Solves the linear program with the specified options in GLPK.  Returns the return code,
-- and if the solver was successful, the objective function value, the settings of each variable, and
-- the value of each constraint/row.
glpSolve' :: (Ord v, Real c, MonadState (LP v c) m, MonadIO m) => 
	GLPOpts -> m (ReturnCode, Maybe (Double, Map v Double, [RowValue v c]))
glpSolve' opts = get >>= liftIO . glpSolveAll opts

{-# SPECIALIZE writeLPToFile :: (Ord v, Show v, Real c) => FilePath -> LPT v c IO () #-}
writeLPToFile :: (Ord v, Show v, Real c, MonadState (LP v c) m, MonadIO m) =>
	FilePath -> m ()
writeLPToFile file = get >>= liftIO . writeLP file 

{-# SPECIALIZE readLPFromFile :: FilePath -> LPT String Double IO () #-}
readLPFromFile :: (MonadState (LP String Double) m, MonadIO m) =>
	FilePath -> m ()
readLPFromFile file = put =<< liftIO (readLP file)