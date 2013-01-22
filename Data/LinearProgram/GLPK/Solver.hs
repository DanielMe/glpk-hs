{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE TupleSections, RecordWildCards #-}

module Data.LinearProgram.GLPK.Solver (
	-- * Solver options
	GLPOpts(..),
	simplexDefaults, 
	mipDefaults, 
	-- * Running the solver
	glpSolveVars,
	RowValue(..),
	glpSolveAll,
	-- * GLPK enumerations
	ReturnCode(..),
	MsgLev(..), 
	BranchingTechnique(..),
	BacktrackTechnique(..), 
	Preprocessing(..), 
	Cuts(..)) where 

import Control.Monad
-- import Control.Monad.Trans

-- import Debug.Trace

import Data.Map
-- import Data.Maybe (catMaybes)
import Data.LinearProgram.Common
import Data.LinearProgram.GLPK.Internal
import Data.LinearProgram.GLPK.Types

-- import Data.Time.Clock
-- import System.Time

import GHC.Exts(build)

-- | Options available for customizing GLPK operations.  This also determines
-- which kind of solving is performed -- relaxed LP, or MIP.
data GLPOpts = SimplexOpts {msgLev :: MsgLev, tmLim :: !Int, presolve :: Bool} |
	MipOpts {msgLev :: MsgLev, tmLim :: !Int, presolve :: Bool,
		brTech :: BranchingTechnique, btTech :: BacktrackTechnique,
		ppTech :: Preprocessing,
		fpHeur :: Bool,
		cuts :: [Cuts],
		mipGap :: !Double}

data RowValue v c = RowVal {row :: !(Constraint v c), rowVal :: !Double}

simplexDefaults, mipDefaults :: GLPOpts
simplexDefaults = SimplexOpts MsgOn 10000 True
mipDefaults = MipOpts MsgOn 10000 True DrTom LocBound AllPre False [] 0.0

-- | Solves the linear or mixed integer programming problem.  Returns
-- the value of the objective function, and the values of the variables.
glpSolveVars :: (Ord v, Real c) => GLPOpts -> LP v c -> IO (ReturnCode, Maybe (Double, Map v Double))
glpSolveVars opts@SimplexOpts{} lp = runGLPK $ do
	(code, vars) <- doGLP opts lp
	liftM (code, ) $ maybe (return Nothing) ( \ vars -> do
		obj <- getObjVal
		vals <- sequence [do
			val <- getColPrim i
			return (v, val)
				| (v, i) <- assocs vars]
		return (Just (obj, fromDistinctAscList vals))) vars
glpSolveVars opts@MipOpts{} lp = runGLPK $ do
	(code, vars) <- doGLP opts lp
	liftM (code, ) $ maybe (return Nothing) (\ vars -> do
		obj <- mipObjVal
		vals <- sequence [do
			val <- mipColVal i
			return (v, val)
				| (v, i) <- assocs vars]
		return (Just (obj, fromDistinctAscList vals))) vars

-- | Solves the linear or mixed integer programming problem.  Returns
-- the value of the objective function, the values of the variables,
-- and the values of any labeled rows.
glpSolveAll :: (Ord v, Real c) => GLPOpts -> LP v c -> IO (ReturnCode, Maybe (Double, Map v Double, [RowValue v c]))
glpSolveAll opts@SimplexOpts{} lp@LP{..} = runGLPK $ do
	(code, vars) <- doGLP opts lp
	liftM (code, ) $ maybe (return Nothing) (\ vars -> do
		obj <- getObjVal
		vals <- sequence [do
			val <- getColPrim i
			return (v, val)
				| (v, i) <- assocs vars]
		rows <- sequence [liftM (RowVal c) (getRowPrim i)
					| (i, c) <- zip [1..] constraints]
		return (Just (obj, fromDistinctAscList vals, rows))) vars
glpSolveAll opts@MipOpts{} lp@LP{..} = runGLPK $ do
	(code, vars) <- doGLP opts lp
	liftM (code, ) $ maybe (return Nothing) (\ vars -> do
		obj <- mipObjVal
		vals <- sequence [do
			val <- mipColVal i
			return (v, val)
				| (v, i) <- assocs vars]
		rows <- sequence [liftM (RowVal c) (getRowPrim i)
					| (i, c) <- zip [1..] constraints]
		return (Just (obj, fromDistinctAscList vals, rows))) vars

doGLP :: (Ord v, Real c) => GLPOpts -> LP v c -> GLPK (ReturnCode, Maybe (Map v Int))
doGLP SimplexOpts{..} lp = do
	vars <- writeProblem lp
	success <- solveSimplex msgLev tmLim presolve
	return (success, guard (gaveAnswer success) >> Just vars)
doGLP MipOpts{..} lp = do
	vars <- writeProblem lp
-- 	time <- getTime
-- 	solveSimplex msgLev tmLim presolve
-- 	time' <- getTime
	let tmLim' = tmLim  --- round (toRational (time' `diffUTCTime` time))
	success <- mipSolve msgLev brTech btTech ppTech fpHeur cuts mipGap (fromIntegral tmLim') presolve
	return (success, guard (gaveAnswer success) >> Just vars) --(if success then Just vars else Nothing)
-- 	where	getTime = liftIO getCurrentTime
		
{-# RULES
	"assocs" assocs = \ m -> build (\ c n -> foldWithKey (curry c) n m);
	"elems" elems = \ m -> build (\ c n -> foldWithKey (const c) n m);
	"keys" keys = \ m -> build (\ c n -> foldWithKey (\ k _ -> c k) n m);
	#-}