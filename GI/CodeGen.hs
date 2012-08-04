{-# LANGUAGE GADTs #-}
module GI.CodeGen
    ( genConsts
    , genModules
    , upperFirst
    ) where

import Control.Applicative

import Data.Char (toUpper, toLower, isDigit)
import Data.List (foldl')

import qualified Data.Set as Set

import GI.SimpleModule
import GI.SyntaxBuilder

import qualified Language.Haskell.Exts.Syntax as H

import GI.API
import GI.Value
import GI.Internal.FunctionInfo
import GI.TaggedType

-- generate the modules, compressing the constants into one module
genModules :: [API] -> SimpleModule
genModules apis = foldl' mergeModules emptyModule modules
  where  
    modules = map genAPI apis

-- top-level generation
genAPI :: API -> SimpleModule
genAPI (APIConst nConst)         = genConsts nConst
genAPI (APICallback nCallback)   = genCallback nCallback
genAPI (APIEnum nEnum)           = genEnum nEnum
genAPI (APIFlags nFlags)         = genFlags nFlags
genAPI (APIInterface nInterface) = genInterface nInterface
genAPI (APIObject nObject)       = genObject nObject
genAPI (APIStruct nStruct)       = genStruct nStruct
genAPI (APIFunction func)        = genFunction func
genAPI (APIUnion u)              = genUnion u

-- Objects
genObject :: Named Object -> SimpleModule
genObject (Named ns n (Object name mbParent fields methods props))
    = let methodDefns = concatMap (functionDecl n) methods
          dataDecls   = [ emptyData (n ++ "_")
                        , typeDeclH n (ptrTypeH (strType $ n ++ "_"))
                        ]
          allStuff    = dataDecls ++  methodDefns
      in SimpleModule Set.empty allStuff

functionDecl className (Function symbol flags (Named ns n callable))
    | FunctionIsConstructor `elem` flags = 
      constructorDecl (Just className) symbol n callable
    | FunctionIsMethod `elem` flags = 
      callableDecl (Just ns) (Just className) symbol n callable
    | otherwise = constructorDecl (Just className) symbol n callable

data FFIAndHaskell = FFIAndHaskell 
                  { ffiDecl :: H.Decl
                  , haskDecl :: HaskDecl
                  }

fromFFIAndHaskell (FFIAndHaskell ffi hask) = ffi : fromHaskDecl hask

constructorDecl classNameMb symbolName name callable
    = callableDecl Nothing classNameMb symbolName name callable


callableDecl namespaceMb classNameMb symbolName name callable = 
  FFIAndHaskell cPart haskDecl
  where 
    objTypeMb = InterfaceType <$> namespaceMb <*> classNameMb
    name'     = maybe name (++ "_" ++ name) classNameMb
    cPart     = cDecl symbolName
                      (argCTypes callable)
                      (toTypedEx $ returnType callable)
    haskDecl = normalCall objTypeMb symbolName name' callable 


cDecl symbolName typedArgs retType = ffiImport symbolName type_
  where
    type_ = ioCArgs typedArgs retType
  
ffiImport name t = 
  H.ForImp l H.CCall (H.PlaySafe False) name (H.Ident $ cName name) t

normalCall objTypeM symbolName name callable = 
  normalCall' cName name typedArgs retType
  where
    -- Arguments and return type using the existential `TypeEx`
    typedArgs :: [TypedArg HaskTag]
    typedArgs  = toTypedArgs (args callable)
    
    typedArgs' :: [TypedArg HaskTag]
    typedArgs' = maybe id (\t -> (thisArg t :)) objTypeM typedArgs
    retType    = toTypedEx (returnType callable)
    
    cName = "c_" ++ symbolName
    
normalCall' :: String 
               -> String 
               -> [TypedArg HaskTag] 
               -> TypeEx HaskTag 
               -> HaskDecl
normalCall' cName name typedArgs retType = 
  funDefH (valueCase name) funType argPats (fmapType hReturn retType wrappedRhs)
  where
    funType    = ioHArgs typedArgs retType
    
    -- Call to the function
    wrappedRhs = hArg typedArgs rhs
    rhs        = foldl H.App cExpr 
                   (map (evar . safeName . typedArgName) typedArgs)
    
    cExpr      = evar cName

    argPats    = map (pvar . safeName . typedArgName) typedArgs

-- Argument conversion
data TypedArg tag = TypedArg { typedArgName :: String 
                             , typedArgType :: TypeEx tag
                             }
                    

thisArg t = TypedArg "this" (TypeEx t)

toTypedArg :: Arg -> TypedArg tag
toTypedArg arg = TypedArg (safeName $ argName arg) (toTypedEx $ argType arg)


ioHArgs :: [TypedArg HaskTag] -> TypeEx HaskTag -> H.Type
ioHArgs args ret 
  = foldr (H.TyFun . typeExHType . typedArgType) 
          (H.TyApp (strType "IO") (typeExHType ret)) 
          args


hArg :: [TypedArg HaskTag] -> H.Exp -> H.Exp
hArg [] = id
hArg (TypedArg name (TypeEx t) : rest) = f . hArg rest
  where
    f =
      case t of
        FileNameType -> withString name
        -- ArrayType v -> withString name
        -- ListType v  -> withString name
        -- SListType v -> withString name
        -- I'm not sure how GI will export "char *", likely to use strings
        -- conveniently, I'll have to intercept that subcase and translate to
        -- strings.
        PtrType UTF8Type -> withString name
        t -> id
    -- withArray n e =
    --   H.App (H.App (evar "withArray0") (H.Lit $ H.Int 0) (evar n))
    --         (H.Lambda l [pvar n] e)
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
argCTypes = map toTypedArg . args

ioCArgs :: [TypedArg CTag] -> TypeEx CTag -> H.Type
ioCArgs args ret 
  = foldr (H.TyFun . typeExCType . typedArgType) 
          (H.TyApp (strType "IO") (typeExCType ret)) 
          args
      

-- Argument name that doesn't conflict with Haskell keywords
safeArgName = safeName . argName

genConsts cnst = noImportModule (constDecl cnst)

constDecl (Named _ str cnst) = 
  constDeclH (constCase str) (valueHType $ constValue cnst)
                             (valueExpr $ constValue cnst)

constDeclH n t rhs = funDefH n t [] rhs


-- Structure generation
genStruct (Named ns n (Struct fields methods))
     = let fieldDecl (Field name typOrFunc flags) 
               = ([H.Ident $ safeName name ++ "_" ++ n], typeDecl typOrFunc)
           typeDecl (Left t) = unBangTyH (fmapType cType (toTypedEx t))
           typeDecl (Right (Function symb flags namedCallable)) = 
             let Named ns n callable = namedCallable
                 typedArgs = toTypedArgs (args callable)
             in H.UnBangedTy $ ioHArgs typedArgs (toTypedEx $ returnType callable)
           constConstr = qConDeclH (recDeclH (typeCase n) 
                                             (map fieldDecl fields)) 
           d = dataDeclH (typeCase $ safeName n) [constConstr]
       in noImportModule (emptyPtr $ typeCase $ safeName n) -- [d] ++ concatMap (noTargetFunctionDecl n) methods)

-- Generate unions, so far we'll just do the simple thing and make them
-- an empty data type

genUnion (Named ns n (Union fields)) = noImportModule (emptyPtr $ typeCase n)

-- Flag generation
genFlags (Named ns n (Flags (Enumeration vals)))
  = noImportModule [typeDeclH n (strType "Int")]

-- Callback generation
-- Callbacks are just types I believe, so this should in the end
-- become a system to pass Haskell function pointers to
-- the GLib/Gtk side.
genCallback (Callback (Named ns n call))
  = noImportModule (emptyPtr n) -- constructorDecl "" n (lowerFirst n) call)
      

noTargetFunctionDecl (Function symbol flags (Named ns n callable))
  = constructorDecl Nothing symbol n callable

-- Interface
genInterface (Named ns n (Interface meths consts props))
  = let methDecls = concatMap (fromFFIAndHaskell . functionDecl n) meths
        dataDecls = [ emptyData (n ++ "_")
                    , typeDeclH n (ptrTypeH (strType $ n ++ "_"))
                    ]
        constDecls = concatMap constDecl consts
    in SimpleModule Set.empty (dataDecls ++ methDecls ++ constDecls)


genEnum (Named ns n (Enumeration vals)) 
  = noImportModule [typeDeclH n (strType "Int")]
                    -- the old type: [enumDecl n vals] won't work in foreign
                    -- exports, so for now we just use ints.
    
genFunction f -- (Function symbol flags (Named ns n callable))
  = noImportModule (fromFFIAndHaskell $ noTargetFunctionDecl f) 


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