{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module GI.Generation.Object where

import Control.Category

import Data.Label
import Data.Maybe

import qualified Data.Set as Set

import Prelude hiding ((.), id)

import GI.SimpleModule
import GI.SyntaxBuilder

import qualified Language.Haskell.Exts.Syntax as H
import Language.Haskell.Exts.QQ

import GI.API
import GI.Value
import GI.Internal.FunctionInfo
import GI.TaggedType

import GI.Generation.Common


-- | Generates the bindings for an class (here they call it an object).
-- This includes the associated methods, data declarations, and typeclass
-- definition.
genObject :: Gen Object
genObject ns deps n obj@(Object name mbParent fields methods props)
    = let methodDefns = concatMap (fromFFIAndHaskell . functionDecl deps n) methods
          dataDecls   = emptyPtr n
          instances   = objectInstance namedObj namedObj : 
                        parentInstances namedObj mbParent
            where
              namedObj = Named (get nsName ns) n obj
            -- FIXME: should instantiate at its own typeclass withthe data decl.
          class_      = [ typeClass [] (typeClassName n) ["a"] 
                             [typeSigH (typeClassFunc n) [ty| a -> __n__ |] ]
                        ]
          allStuff    = dataDecls ++  methodDefns ++ instances ++ class_
      in SimpleModule Set.empty allStuff

-- | Generate instances for the named object give its parents.
parentInstances :: Named Object -> Maybe (Named Object) -> [H.Decl]
parentInstances namedObj = maybe [] genParent
  where
    genParent parent = 
      go parent : parentInstances namedObj (objParent $ nameNamed parent)
    go = objectInstance namedObj

-- | The name an object class uses for the associated typeclass
typeClassName n = typeCase n ++ "__Class"
-- | The name for the typeclass to cast to an object class
typeClassFunc n = "to" ++ typeClassName n

-- | Generate an instance for an object, these are all primarily the same:
-- using `castObj` as the implementation.
objectInstance (Named _instNs instName _) (Named classNs className _) =
  instance_ [] 
     (classNs ++ "." ++ typeClassName className)
     (strType $ instName)
     [funBind (typeClassFunc className) [] (evar "castPtr")]

-- | Generate a function declaration for an obect, where
-- the nature of the declaration changes depending on if it is
-- a constructor or not.
functionDecl deps className (Function symbol flags (Named ns n callable))
    | FunctionIsConstructor `elem` flags = 
      constructorDecl deps ns className symbol n callable
    | FunctionIsMethod `elem` flags = 
      callableDecl deps (Just (TypeEx (InterfaceType ns className))) 
                   symbol (className ++ "_" ++ n) callable
    | otherwise = 
      callableDecl deps (Just (TypeEx (InterfaceType ns className))) 
                   symbol (className ++ "_" ++ n) callable

fromFFIAndHaskell (FFIAndHaskell ffi hask) = ffi : fromHaskDecl hask

-- | A constructor declaration changes subtly in that it casts
-- the result to the concrete instance of the class. This is to
-- circumvent the convention in GObject based interfaces that
-- the constructor returns something higher (like a Widget for Gtk).
constructorDecl deps ns className symbolName name callable = go callDecl
  where 
    go = modify (haskFuncExp . haskDecl) funCast . 
         modify (haskTypeDecl . haskDecl) typeCast
    
    callDecl = 
      callableDecl deps Nothing symbolName (className ++ "_" ++ name) callable
    
    -- We cast the resulting pointer to the proper result type because
    -- Gtk makes `Widget` the return type of constructors: not that useful.
    funCast f = applies "fmap" [evar "castPtr", f]
    
    -- Likewise, change the result type (this actually is required so the
    -- type inference knows what the result of `castPtr` is).
    typeCast (H.TyFun le r) = H.TyFun le (typeCast r)
    typeCast (H.TyApp m x) = H.TyApp m (strType className)
    typeCast (H.TyForall mb classes ty) = H.TyForall mb classes (typeCast ty)
    typeCast ty = error $ "constructorDecl.typeCast: " ++ show ty