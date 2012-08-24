{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module GI.Generation.Common where

import Control.Category

import Data.Label
import Data.Maybe

import qualified Data.Map as Map

import Prelude hiding ((.), id)

import GI.SimpleModule
import GI.SyntaxBuilder

import qualified Language.Haskell.Exts.Syntax as H
import Language.Haskell.Exts.QQ

import GI.API
import GI.TaggedType

callableDecl :: [Namespace]
                -> (forall tag . Maybe (TypeEx tag)) -- ^ Possibly the target type
                -> String          -- ^ Name of the C imported function
                -> String          -- ^ Name of this function
                -> Callable        -- ^ The callable to convert
                -> FFIAndHaskell
callableDecl deps objTypeMb symbolName name callable = FFIAndHaskell cPart hask
  where
    typedArgs :: [TypedArg t]
    typedArgs  = toTypedArgs (callArgs callable)
    
    typedArgs' :: [TypedArg t]
    typedArgs' = case objTypeMb of
      Just objType -> thisArg objType : typedArgs
      Nothing      -> typedArgs
        
    retType    :: TypeEx t
    retType    = toTypedEx (returnType callable)
    
    cPart     = cDecl symbolName typedArgs' retType
    
    hask      = normalCall deps (cName symbolName) name typedArgs' retType 


cDecl symbolName typedArgs retType = ffiImport symbolName type_
  where
    type_ = ioCArgs typedArgs retType
  
ffiImport name t = 
  H.ForImp l H.CCall (H.PlaySafe False) name (H.Ident $ cName name) t

normalCall :: [Namespace]
              -> String 
              -> String 
              -> [TypedArg HaskTag] 
              -> TypeEx HaskTag 
              -> HaskDecl
normalCall deps cname name typedArgs retType = 
  HaskDecl funType caseName argNames (fmapType hReturn retType rhs)
  where
    !caseName = valueCase name
    funType = ioHArgs deps typedArgs retType
    
    -- Call to the function
    rhs       = hArg typedArgs withCasts 
    withCasts = foldl H.App cExpr args
    cExpr     = evar cname
    args      = map (castArg deps) typedArgs
    argNames  = map typedArgName typedArgs

-- Argument conversion
data TypedArg tag = TypedArg { typedArgName :: String 
                             , typedArgType :: TypeEx tag
                             }

castArg :: [Namespace] -> TypedArg tag -> H.Exp
castArg deps (TypedArg name (TypeEx tagType)) =
  case tagType of
    t@(InterfaceType ns n) -> 
      if isClass deps t
      then let toName = evar $ ns ++ ".to" ++ n ++ "__Class"
           in [hs| $toName __name__ |]
      else evar name
    _ -> evar name

thisArg :: TypeEx tag -> TypedArg tag
thisArg = TypedArg "this"

toTypedArg :: Arg -> TypedArg tag
toTypedArg arg = TypedArg (safeName $ argName arg) (toTypedEx $ argType arg)

-- | Converting a list of arguments to a type of the function.
-- This works in combination with the `castArg` function that will
-- call the appropriate conversion function to objects.
ioHArgs :: [Namespace] -> [TypedArg HaskTag] -> TypeEx HaskTag -> H.Type
ioHArgs deps args ret = H.TyForall Nothing context  withoutClasses
  where
    context = foldl go [] args 
      where
        go clses (TypedArg name type_) =
          case fmapType (className deps) type_ of
            Nothing  -> clses
            Just (ns, cls) -> newClass : clses
              where
                newClass :: H.Asst
                newClass = 
                      H.ClassA
                      (unqual $ ns ++ "." ++ cls ++ "__Class") 
                        [maybe
                         (error "ioHArgs.context")
                         (\i -> strType ("cls" ++ show i))
                         (Map.lookup name classTypes)]
              
    withoutClasses = foldr toHType resType args
    
    resType = H.TyApp (strType "IO") (typeExHType ret)
    toHType (TypedArg name type_) = H.TyFun newType
      where 
        newType = 
          case Map.lookup name classTypes of
            Just n -> strType ("cls" ++ show n)
            Nothing -> typeExHType type_
    
    classTypes :: Map.Map String Int
    classTypes = foldl go Map.empty args
      where
        go classMap (TypedArg name type_)
          | fmapType (isClass deps) type_ = 
            Map.insert name (Map.size classMap + 1) classMap
          | otherwise = classMap
        

isClass deps = isJust . className deps
    

className :: [Namespace] -> TaggedType tag type_ -> Maybe (String, String)
className deps (InterfaceType ns name) =
  case nss of
    [] -> Nothing
    (nameSp:_) -> case Map.lookup name (get nsObject nameSp) of
      Just _ -> Just (ns, name)
      _ -> Nothing
  where
    nss = filter ( (== ns) . get nsName) deps
className _ _ = Nothing


hArg :: [TypedArg HaskTag] -> H.Exp -> H.Exp
hArg [] = id
hArg (TypedArg name (TypeEx t) : rest) = f . hArg rest
  where
    f =
      case t of
        FileNameType -> withString name
        PtrType UTF8Type -> withString name
        t -> id
    withString n e = 
      H.App (H.App (evar "withCString") (evar n))
            (H.Lambda l [pvar n] e)


typeExHType :: TypeEx HaskTag -> H.Type
typeExHType = fmapType hType

typeExCType :: TypeEx CTag -> H.Type
typeExCType = fmapType cType


toTypedArgs :: [Arg] -> [TypedArg tag]
toTypedArgs = map toTypedArg


argCTypes :: Callable -> [TypedArg CTag]
argCTypes = map toTypedArg . callArgs

ioCArgs :: [TypedArg CTag] -> TypeEx CTag -> H.Type
ioCArgs args ret 
  = foldr (H.TyFun . typeExCType . typedArgType) 
          (H.TyApp (strType "IO") (typeExCType ret)) 
          args
      


type Gen a  = Namespace -> [Namespace] -> String -> a -> SimpleModule

data FFIAndHaskell = FFIAndHaskell 
                  { _ffiDecl  :: H.Decl
                  , _haskDecl :: HaskDecl
                  }

mkLabels [''FFIAndHaskell]
