{-# LANGUAGE RecordWildCards, ScopedTypeVariables, EmptyDataDecls, ForeignFunctionInterface #-}
module Data.LinearProgram.GLPK.Internal (writeProblem, addCols,
	addRows, createIndex, findCol, findRow, getColPrim, getRowPrim, getObjVal,
	mipColVal, mipRowVal, mipObjVal, mipSolve, setColBounds, setColKind, setColName, setMatRow,
	setObjCoef, setObjectiveDirection, setRowBounds, setRowName, solveSimplex) where

import Control.Monad

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array

import Data.Bits
import Data.Map hiding (map)
-- import Data.Bounds
import Data.LinearProgram.Common
import Data.LinearProgram.GLPK.Types

-- foreign import ccall "c_glp_set_obj_name" glpSetObjName :: Ptr GlpProb -> CString -> IO ()
foreign import ccall unsafe "c_glp_set_obj_dir" glpSetObjDir :: Ptr GlpProb -> CInt -> IO ()
foreign import ccall unsafe "c_glp_add_rows" glpAddRows :: Ptr GlpProb -> CInt -> IO CInt
foreign import ccall unsafe "c_glp_add_cols" glpAddCols :: Ptr GlpProb -> CInt -> IO CInt
foreign import ccall unsafe "c_glp_set_row_name" glpSetRowName :: Ptr GlpProb -> CInt -> CString -> IO ()
foreign import ccall unsafe "c_glp_set_col_name" glpSetColName :: Ptr GlpProb -> CInt -> CString -> IO ()
foreign import ccall unsafe "c_glp_set_row_bnds" glpSetRowBnds :: Ptr GlpProb -> CInt -> CInt -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "c_glp_set_col_bnds" glpSetColBnds :: Ptr GlpProb -> CInt -> CInt -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "c_glp_set_obj_coef" glpSetObjCoef :: Ptr GlpProb -> CInt -> CDouble -> IO ()
foreign import ccall unsafe "c_glp_set_mat_row" glpSetMatRow :: Ptr GlpProb -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> IO ()
foreign import ccall unsafe "c_glp_create_index" glpCreateIndex :: Ptr GlpProb -> IO ()
foreign import ccall unsafe "c_glp_find_row" glpFindRow :: Ptr GlpProb -> CString -> IO CInt
foreign import ccall unsafe "c_glp_find_col" glpFindCol :: Ptr GlpProb -> CString -> IO CInt
foreign import ccall unsafe "c_glp_solve_simplex" glpSolveSimplex :: Ptr GlpProb -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "c_glp_get_obj_val" glpGetObjVal :: Ptr GlpProb -> IO CDouble
foreign import ccall unsafe "c_glp_get_row_prim" glpGetRowPrim :: Ptr GlpProb -> CInt -> IO CDouble
foreign import ccall unsafe "c_glp_get_col_prim" glpGetColPrim :: Ptr GlpProb -> CInt -> IO CDouble
foreign import ccall unsafe "c_glp_set_col_kind" glpSetColKind :: Ptr GlpProb -> CInt -> CInt -> IO ()
foreign import ccall unsafe "c_glp_mip_solve" glpMipSolve :: 
	Ptr GlpProb -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> CInt -> IO CInt
foreign import ccall unsafe "c_glp_mip_obj_val" glpMIPObjVal :: Ptr GlpProb -> IO CDouble
foreign import ccall unsafe "c_glp_mip_row_val" glpMIPRowVal :: Ptr GlpProb -> CInt -> IO CDouble
foreign import ccall unsafe "c_glp_mip_col_val" glpMIPColVal :: Ptr GlpProb -> CInt -> IO CDouble

setObjectiveDirection :: Direction -> GLPK ()
setObjectiveDirection dir = GLP $ flip glpSetObjDir 
	(fromIntegral $ 1 + fromEnum dir)

addRows :: Int -> GLPK Int
addRows n = GLP $ liftM fromIntegral . flip glpAddRows (fromIntegral n)

addCols :: Int -> GLPK Int
addCols n = GLP $ liftM fromIntegral . flip glpAddCols (fromIntegral n)

setRowName :: Int -> String -> GLPK ()
setRowName i nam = GLP $ withCString nam . flip glpSetRowName (fromIntegral i)

setColName :: Int -> String -> GLPK ()
setColName i nam = GLP $ withCString nam . flip glpSetColName (fromIntegral i)

setRowBounds :: Real a => Int -> Bounds a -> GLPK ()
setRowBounds i bds = GLP $ \ lp -> onBounds (glpSetRowBnds lp (fromIntegral i)) bds

setColBounds :: Real a => Int -> Bounds a -> GLPK ()
setColBounds i bds = GLP $ \ lp -> onBounds (glpSetColBnds lp (fromIntegral i)) bds

onBounds :: Real a => (CInt -> CDouble -> CDouble -> x) -> Bounds a -> x
onBounds f bds = case bds of
	Free		-> f 1 0 0
	LBound a	-> f 2 (realToFrac a) 0
	UBound a	-> f 3 0 (realToFrac a)
	Bound a b	-> f 4 (realToFrac a) (realToFrac b)
	Equ a		-> f 5 (realToFrac a) 0

setObjCoef :: Real a => Int -> a -> GLPK ()
setObjCoef i v = GLP $ \ lp -> glpSetObjCoef lp (fromIntegral i) (realToFrac v)

setMatRow :: Real a => Int -> [(Int, a)] -> GLPK ()
setMatRow i row = GLP $ \ lp -> 
	allocaArray (len+1) $ \ (ixs :: Ptr CInt) -> allocaArray (len+1) $ \ (coeffs :: Ptr CDouble) -> do
		pokeArray ixs (0:map (fromIntegral . fst) row)
		pokeArray coeffs (0:map (realToFrac . snd) row)
		glpSetMatRow lp (fromIntegral i) (fromIntegral len) ixs coeffs
	where	len = length row

createIndex :: GLPK ()
createIndex = GLP glpCreateIndex

findRow :: String -> GLPK Int
findRow nam = GLP $ liftM fromIntegral . withCString nam . glpFindRow

findCol :: String -> GLPK Int
findCol nam = GLP $ liftM fromIntegral . withCString nam . glpFindCol

solveSimplex :: MsgLev -> Int -> Bool -> GLPK ReturnCode
solveSimplex msglev tmLim presolve = GLP $ \ lp -> liftM (toEnum . fromIntegral) $ glpSolveSimplex lp
	(getMsgLev msglev)
	tmLim'
	(if presolve then 1 else 0)
	where	tmLim' = fromIntegral (tmLim * 1000)

getMsgLev :: MsgLev -> CInt
getMsgLev = fromIntegral . fromEnum

getObjVal :: GLPK Double
getObjVal = liftM realToFrac $ GLP glpGetObjVal

getRowPrim :: Int -> GLPK Double
getRowPrim i = liftM realToFrac $ GLP (`glpGetRowPrim` fromIntegral i)

getColPrim :: Int -> GLPK Double
getColPrim i = liftM realToFrac $ GLP (`glpGetColPrim` fromIntegral i)

setColKind :: Int -> VarKind -> GLPK ()
setColKind i kind = GLP $ \ lp -> glpSetColKind lp (fromIntegral i) (fromIntegral $ 1 + fromEnum kind)

mipSolve :: MsgLev -> BranchingTechnique -> BacktrackTechnique -> Preprocessing -> Bool ->
	[Cuts] -> Double -> Int -> Bool -> GLPK ReturnCode
mipSolve msglev brt btt pp fp cuts mipgap tmlim presol =
		liftM (toEnum . fromIntegral) $ GLP $ \ lp -> glpMipSolve lp msglev'
						brt' btt' pp' fp' tmlim' cuts' mipgap' presol'
	where	!msglev' = getMsgLev msglev
		!brt' = case brt of
			FirstFrac	-> 1
			LastFrac	-> 2
			MostFrac	-> 3
			DrTom		-> 4
			HybridP		-> 5
		!btt' = case btt of
			DepthFirst	-> 1
			BreadthFirst	-> 2
			LocBound	-> 3
			ProjHeur	-> 4
		!pp' = case pp of
			NoPre	-> 0
			RootPre	-> 1
			AllPre	-> 2
		!fp' = if fp then 1 else 0
		!cuts' = (if GMI `elem` cuts then 1 else 0) .|.
			(if MIR `elem` cuts then 2 else 0) .|.
			(if Cov `elem` cuts then 4 else 0) .|.
			(if Clq `elem` cuts then 8 else 0)
		!mipgap' = realToFrac mipgap
		!tmlim' = fromIntegral (1000 * tmlim)
		!presol' = if presol then 1 else 0

mipObjVal :: GLPK Double
mipObjVal = liftM realToFrac $ GLP glpMIPObjVal

mipRowVal :: Int -> GLPK Double
mipRowVal i = liftM realToFrac $ GLP (`glpMIPRowVal` fromIntegral i)

mipColVal :: Int -> GLPK Double
mipColVal i = liftM realToFrac $ GLP (`glpMIPColVal` fromIntegral i)

writeProblem :: (Ord v, Real c) => LP v c -> GLPK (Map v Int)
writeProblem LP{..} = do
	setObjectiveDirection direction
	i0 <- addCols nVars
	let allVars' = fmap (i0 +) allVars
	sequence_ [setObjCoef i v | (i, v) <- elems $ intersectionWith (,) allVars' objective]
	j0 <- addRows (length constraints)
	sequence_ [do	case lab of
				Nothing	-> return ()
				Just n	-> setRowName j n
			setMatRow j
				[(i, v) | (i, v) <- elems (intersectionWith (,) allVars' f)]
			setRowBounds j bnds
				| (j, Constr lab f bnds) <- zip [j0..] constraints]
-- 	createIndex
	sequence_ [setColBounds (i) bnds |
			(i, bnds) <- elems $ intersectionWith (,) allVars' varBounds]
	sequence_ [setColBounds i Free | i <- elems $ difference allVars' varBounds]
	sequence_ [setColKind (i) knd |
			(i, knd) <- elems $ intersectionWith (,) allVars' varTypes]
-- 	writeLP
	return allVars'
	where	allVars0 = fmap (const ()) objective `union`
			unions [fmap (const ()) f | Constr _ f _ <- constraints] `union`
			fmap (const ()) varBounds `union` fmap (const ()) varTypes
		(nVars, allVars) = mapAccum (\ n _ -> (n+1, n)) (0 :: Int) allVars0