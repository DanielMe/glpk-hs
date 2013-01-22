{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE RecordWildCards #-}

module Data.LinearProgram.GLPK (GLPOpts(..), MsgLev(..), BranchingTechnique(..),
	BacktrackTechnique(..), Preprocessing(..), Cuts(..), 
	simplexDefaults, mipDefaults, glpSolveVars, glpSolveAll) where

import Control.Monad.Trans

import Data.Map
import Data.Maybe (catMaybes)
import Data.LinearProgram.Spec
import Data.LinearProgram.Types
import Data.LinearProgram.GLPK.Internal

import System.CPUTime

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

simplexDefaults, mipDefaults :: GLPOpts
simplexDefaults = SimplexOpts MsgOn 10000 True
mipDefaults = MipOpts MsgOn 10000 True DrTom LocBound AllPre False [] 0.0

-- | Solves the linear or mixed integer programming problem.  Returns
-- the value of the objective function, and the values of the variables.
glpSolveVars :: (Ord v, Real c) => GLPOpts -> LP v c -> IO (Double, Map v Double)
glpSolveVars opts@SimplexOpts{} lp = runGLPK $ do
	Just vars <- doGLP opts lp
	obj <- getObjVal
	vals <- sequence [do
		val <- getColPrim i
		return (v, val)
			| (v, i) <- assocs vars]
	return (obj, fromDistinctAscList vals)
glpSolveVars opts@MipOpts{} lp = runGLPK $ do
	Just vars <- doGLP opts lp
	obj <- mipObjVal
	vals <- sequence [do
		val <- mipColVal i
		return (v, val)
			| (v, i) <- assocs vars]
	return (obj, fromDistinctAscList vals)

-- | Solves the linear or mixed integer programming problem.  Returns
-- the value of the objective function, the values of the variables,
-- and the values of any labeled rows.
glpSolveAll :: (Ord v, Real c) => GLPOpts -> LP v c -> IO (Double, Map v Double, Map String Double)
glpSolveAll opts@SimplexOpts{} lp@LP{..} = runGLPK $ do
	Just vars <- doGLP opts lp
	obj <- getObjVal
	vals <- sequence [do
		val <- getColPrim i
		return (v, val)
			| (v, i) <- assocs vars]
	rows <- sequence [maybe (return Nothing) (\ nam -> do
				val <- getRowPrim i
				return (Just (nam, val))) nam
				| (i, Constr nam _ _) <- zip [0..] constraints]
	return (obj, fromDistinctAscList vals, fromDistinctAscList (catMaybes rows))
glpSolveAll opts@MipOpts{} lp@LP{..} = runGLPK $ do
	Just vars <- doGLP opts lp
	obj <- mipObjVal
	vals <- sequence [do
		val <- mipColVal i
		return (v, val)
			| (v, i) <- assocs vars]
	rows <- sequence [maybe (return Nothing) (\ nam -> do
				val <- mipRowVal i
				return (Just (nam, val))) nam
				| (i, Constr nam _ _) <- zip [0..] constraints]
	return (obj, fromDistinctAscList vals, fromDistinctAscList (catMaybes rows))

doGLP :: (Ord v, Real c) => GLPOpts -> LP v c -> GLPK (Maybe (Map v Int))
doGLP SimplexOpts{..} lp = do
	vars <- writeProblem lp
	success <- solveSimplex msgLev tmLim presolve
	return (if success then Just vars else Nothing)
doGLP MipOpts{..} lp = do
	vars <- writeProblem lp
	time <- getTime
	solveSimplex msgLev tmLim presolve
	time' <- getTime
	let tmLim' = (fromIntegral tmLim - time' + time + 1000000000000 - 1) `quot` 1000000000000
	success <- mipSolve msgLev brTech btTech ppTech fpHeur cuts mipGap (fromIntegral tmLim') presolve
	return (if success then Just vars else Nothing)
	where	getTime = liftIO getCPUTime

writeProblem :: (Ord v, Real c) => LP v c -> GLPK (Map v Int)
writeProblem LP{..} = do
	setObjectiveDirection direction
	i0 <- addCols nVars
	sequence_ [setObjCoef (i + i0) v | (i, v) <- elems $ intersectionWith (,) allVars objective]
	j0 <- addRows (length constraints)
	sequence_ [do	case lab of
				Nothing	-> return ()
				Just n	-> setRowName (j0 + j) n
			setMatRow (j0 + j)
				(elems (intersectionWith (,) allVars f))
			setRowBounds (j0 + j) bnds
				| (j, Constr lab f bnds) <- zip [0..] constraints]
	createIndex
	sequence_ [setColBounds (i0 + i) bnds |
			(i, bnds) <- elems $ intersectionWith (,) allVars varBounds]
	sequence_ [setColKind (i0 + i) knd |
			(i, knd) <- elems $ intersectionWith (,) allVars varTypes]
	return allVars
	where	allVars0 = fmap (const ()) objective `union`
			unions [fmap (const ()) f | Constr _ f _ <- constraints] `union`
			fmap (const ()) varBounds `union` fmap (const ()) varTypes
		(nVars, allVars) = mapAccum (\ n _ -> (n+1, n)) (0 :: Int) allVars0
		
{-# RULES
	"assocs" assocs = \ m -> build (\ c n -> foldWithKey (curry c) n m);
	"elems" elems = \ m -> build (\ c n -> foldWithKey (const c) n m);
	"keys" keys = \ m -> build (\ c n -> foldWithKey (\ k _ -> c k) n m);
	#-}