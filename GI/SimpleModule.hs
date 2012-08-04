module GI.SimpleModule where

import Data.List (nub)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Language.Haskell.Exts.Syntax as H
       
import GI.SyntaxBuilder

-- | Simple modules
data SimpleModule = SimpleModule (Set H.ImportDecl) [H.Decl]

addImport (SimpleModule imps dcls) str =
  SimpleModule (Set.insert (qimp str) imps) dcls

addDecl (SimpleModule imps dcls) d =
  SimpleModule imps (d:dcls)

noImportModule = SimpleModule Set.empty
                 
simpleToRealModule :: String -> SimpleModule -> H.Module
simpleToRealModule name (SimpleModule imps dcls) =
  modulImport name (Set.toList imps) (nub dcls)

mergeModules (SimpleModule imp1 dcls1) (SimpleModule imp2 dcls2) =
  SimpleModule (Set.union imp1 imp2) (dcls1 ++ dcls2)
emptyModule = SimpleModule Set.empty []