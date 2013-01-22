module Data.LinearProgram.Spec where

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

data LP v c = LP {direction :: Direction, objective :: ObjectiveFunc v c, constraints :: [Constraint v c],
			varBounds :: VarBounds v c, varTypes :: VarTypes v} deriving (Read, Show)