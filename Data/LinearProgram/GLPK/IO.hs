module Data.LinearProgram.GLPK.IO where

-- import Control.Monad.Trans

-- import Data.Map

import Data.LinearProgram.Common
import Data.LinearProgram.LPMonad.Internal

import Data.LinearProgram.GLPK.Common
import Data.LinearProgram.GLPK.IO.Internal

-- | Read a linear program from a file in CPLEX LP format.
readLP :: FilePath -> IO (LP String Double)
readLP = runGLPK . readGLP_LP

-- | Write a linear program to a file in CPLEX LP format.
writeLP :: (Ord v, Show v, Real c) => FilePath -> LP v c -> IO ()
writeLP file = runGLPK . writeGLP_LP file
