module Data.LinearProgram.Common (
	module Data.LinearProgram.Spec,
	module Data.LinearProgram.LinFunc,
	module Data.LinearProgram.Types) where

import Data.LinearProgram.Spec
import Data.LinearProgram.LinFunc
import Data.LinearProgram.Types

import Data.Map
import GHC.Exts (build)

{-# RULES
	"assocs" assocs = \ m -> build (\ c n -> foldWithKey (curry c) n m);
	"elems" elems = \ m -> build (\ c n -> foldWithKey (const c) n m);
	"keys" keys = \ m -> build (\ c n -> foldWithKey (\ k _ -> c k) n m);
	#-}