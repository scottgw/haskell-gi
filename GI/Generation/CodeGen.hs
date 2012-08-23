{-# LANGUAGE GADTs, BangPatterns, RankNTypes, TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module GI.Generation.CodeGen
    -- ( genConsts
    -- , genModules
    -- , upperFirst
    -- ) 
    where

import Control.Category

import Data.List (foldl')
import Data.Label
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set


import Prelude hiding ((.), id)

import GI.SimpleModule
import GI.SyntaxBuilder

import qualified Language.Haskell.Exts.Syntax as H

import GI.API
import GI.Value

import GI.TaggedType

import GI.Generation.Common
import GI.Generation.Object

data SelAndGen = forall a . SelAndGen (Namespace :-> NamedMap a) (Gen a)

-- top-level generation
genModule :: Namespace -> [Namespace] -> SimpleModule
genModule ns dependencies = module_
  where
    addToModule :: Gen a -> String -> a -> SimpleModule -> SimpleModule
    addToModule f name v mod = mergeModules mod (f ns dependencies name v)
    
    foldModule :: (Namespace :-> NamedMap a) -> Gen a -> SimpleModule
    foldModule sel f = Map.foldrWithKey (addToModule f) emptyModule (get sel ns)
    
    selsAndGens = [ SelAndGen nsObject genObject
                  , SelAndGen nsConst  genConsts
                  , SelAndGen nsEnum   genEnum
                  , SelAndGen nsUnion  genUnion
                  , SelAndGen nsStruct genStruct
                  , SelAndGen nsFlags  genFlags
                  , SelAndGen nsFunc   genFunction
                  , SelAndGen nsCB     genCallback
                  , SelAndGen nsIFace  genInterface
                  ]
    
    modules = map (\ (SelAndGen sel gen) -> foldModule sel gen) selsAndGens
    
    module_ = foldl' mergeModules emptyModule modules

-- Argument name that doesn't conflict with Haskell keywords
safeArgName = safeName . argName

genConsts _ns _deps str cnst = noImportModule (constDecl str cnst)

constDecl str cnst = 
  constDeclH (constCase str) (valueHType $ constValue cnst)
                             (valueExpr $ constValue cnst)

constDeclH n t rhs = funDefH n t [] rhs


-- Structure generation
genStruct _ns _deps n (Struct fields methods) =
     -- = let fieldDecl (Field name typOrFunc flags) 
     --           = ([H.Ident $ safeName name ++ "_" ++ n], typeDecl typOrFunc)
     --       typeDecl (Left t) = unBangTyH (fmapType cType (toTypedEx t))
     --       typeDecl (Right (Function symb flags namedCallable)) = 
     --         let Named _ns _n callable = namedCallable
     --             typedArgs = toTypedArgs (callArgs callable)
     --         in H.UnBangedTy $ ioHArgs typedArgs (toTypedEx $ returnType callable)
     --       constConstr = qConDeclH (recDeclH (typeCase n) 
     --                                         (map fieldDecl fields)) 
     --       d = dataDeclH (typeCase $ safeName n) [constConstr]
     --   in noImportModule (emptyPtr $ typeCase $ safeName n) -- [d] ++ concatMap (noTargetFunctionDecl n) methods)
          noImportModule (emptyPtr $ typeCase $ safeName n)

-- Generate unions, so far we'll just do the simple thing and make them
-- an empty data type

genUnion _ns _deps n (Union fields) = noImportModule (emptyPtr $ typeCase n)

-- Flag generation
genFlags _ns _deps n (Flags (Enumeration vals))
  = noImportModule [typeDeclH n (strType "Int")]

-- Callback generation
-- Callbacks are just types I believe, so this should in the end
-- become a system to pass Haskell function pointers to
-- the GLib/Gtk side.
genCallback _ns _deps n _call
  = noImportModule (emptyPtr n) -- constructorDecl "" n (lowerFirst n) call)
      

noTargetFunctionDecl deps (Function symbol _flags (Named ns n callable))
  = callableDecl deps Nothing symbol n callable

-- Interface

interfaceDecl deps className (Function symbolName _flags (Named ns name callable)) =
  FFIAndHaskell cPart hask
  where 
    prefixName = className ++ "_" ++ name
    
    typedArgs :: [TypedArg t]
    typedArgs  = toTypedArgs (callArgs callable)
            
    retType    :: TypeEx t
    retType    = toTypedEx (returnType callable)
    
    cPart     = cDecl symbolName typedArgs retType
    
    hask      = normalCall deps (cName symbolName) prefixName typedArgs retType 


genInterface _ns deps n (Interface meths consts _props)
  = let methDecls = concatMap (fromFFIAndHaskell . interfaceDecl deps n) meths
        dataDecls = emptyPtr n
        namedConst (Named _ cName cnst) = constDecl cName cnst
        constDecls = concatMap namedConst consts
    in SimpleModule Set.empty (dataDecls ++ methDecls ++ constDecls)


genEnum _ns _deps n (Enumeration _vals)
  = noImportModule [typeDeclH n (strType "Int")]
                    -- the old type: [enumDecl n vals] won't work in foreign
                    -- exports, so for now we just use ints.
    
genFunction _ns deps _n f
  = noImportModule (fromFFIAndHaskell $ noTargetFunctionDecl deps f) 


-- | Utility functions.
valueExpr VVoid         = H.Tuple []
valueExpr (VBoolean x)  = H.Con (H.UnQual (H.Ident (show x)))
valueExpr (VInt8 x)     = num x
valueExpr (VUInt8 x)    = num x
valueExpr (VInt16 x)    = num x
valueExpr (VUInt16 x)   = num x
valueExpr (VInt32 x)    = num x
valueExpr (VUInt32 x)   = num x
valueExpr (VInt64 x)    = num x
valueExpr (VUInt64 x)   = num x
valueExpr (VFloat x)    = float x
valueExpr (VDouble x)   = float x
valueExpr (VGType x)    = num x
valueExpr (VUTF8 x)     = H.Lit $ H.String [x]
valueExpr (VUTF8Ptr x)  = H.Lit $ H.String x
valueExpr (VFileName x) = H.Lit $ H.String x

valueHType :: Value -> H.Type
valueHType v = fmapType hType (toTypedEx (valueType v))