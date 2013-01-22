{-# LANGUAGE RecordWildCards #-}
module Data.LinearProgram.Spec where

-- import Control.DeepSeq
-- import Data.Bounds
import Data.LinFunc
import Data.LinearProgram.Types
import Data.Map

data Constraint v c = Constr (Maybe String)
			(LinFunc v c)
			(Bounds c) deriving (Read, Show)
type VarTypes v = Map v VarKind
type ObjectiveFunc = LinFunc
type VarBounds v c = Map v (Bounds c)

-- instance (NFData v, NFData c) => NFData (Constraint v c) where
-- 	rnf (Constr lab f b) = lab `deepseq` f `deepseq` rnf b

data LP v c = LP {direction :: Direction, objective :: ObjectiveFunc v c, constraints :: [Constraint v c],
			varBounds :: VarBounds v c, varTypes :: VarTypes v} deriving (Read, Show)

-- instance (NFData v, NFData c) => NFData (LP v c) where
-- 	rnf LP{..} = direction `deepseq` objective `deepseq` constraints `deepseq`
-- 		varBounds `deepseq` rnf varTypes