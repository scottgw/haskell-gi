
module GI.CodeGen
    ( genConsts
    , genModules
    ) where

import Data.Char (toUpper, toLower)
import Data.List (partition)

import qualified Language.Haskell.Exts.Syntax as H

import GI.API
import GI.Code
import GI.Type
import GI.Value
import GI.Internal.ArgInfo
import GI.Internal.FunctionInfo

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

num :: Integral a => a -> H.Exp
num = H.Lit . H.Int . fromIntegral

float :: (Read a, Show a, Num a) => a -> H.Exp
float = H.Lit . H.Frac . read . show

l = H.SrcLoc "<unknown>.hs" 0 0

strType :: String -> H.Type
strType = H.TyCon . H.UnQual . H.Ident

typOf TBoolean = strType "Bool"
typOf TInt8 = strType "Int8"
typOf TUInt8 = strType "Word8"
typOf TInt16 = strType "Int16"
typOf TUInt16 = strType "Word16"
typOf TInt32 = strType "Int32"
typOf TUInt32 = strType "Word32"
typOf TInt64 = strType "Int64"
typOf TUInt64 = strType "Word64"
typOf TFloat = strType "Float"
typOf TDouble = strType "Double"
typOf TUniChar = strType "Char"
typOf TGType = strType "GType"
typOf TUTF8 = strType "Char8"
typOf TFileName = strType "String"
typOf TVoid = strType "()"

hType (TBasicType t) = typOf t
hType (TPtr (TBasicType TUTF8)) = strType "String"
hType (TPtr t) = H.TyApp (strType "Ptr") (hType t)
hType (TArray t) = H.TyList (hType t)
hType (TInterface str1 str2) = strType (str1 ++ "." ++ str2)
hType (TGList t) = H.TyList (hType t)
hType (TGSList t) = H.TyList (hType t)
hType (TGHash k v) = H.TyFun (hType k) (hType v)
hType TError = strType "Error"

valueHType = hType . valueType

-- top-level generation
genAPI :: API -> [H.Decl]
genAPI (APIConst nConst) = error "Should not process constants"
genAPI (APICallback nCallback) = genCallback nCallback
genAPI (APIEnum nEnum) = genEnum nEnum
genAPI (APIFlags nFlags) = genFlags nFlags
genAPI (APIInterface nInterface) = genInterface nInterface
genAPI (APIObject nObject) = genObject nObject
genAPI (APIStruct nStruct) = genStruct nStruct
genAPI (APIFunction func) = genFunction func
genAPI (APIUnion u) = error $ "unimplemented union " ++ show u

genModules :: String -> [API] -> H.Module
genModules name apis 
    = let (consts, others) = partition isConst apis
          isConst (APIConst _) = True
          isConst _            = False
      in modul name "" (genConsts consts ++ concatMap genAPI others)

-- uppercase the letter after an underscore
upperAfterUnder :: String -> String
upperAfterUnder [] = []
upperAfterUnder ('_':c:cs) = toUpper c : upperAfterUnder cs
upperAfterUnder (c:cs) = c:upperAfterUnder cs

upperFirst [] = []
upperFirst (c:cs) = toUpper c : cs

lowerFirst [] = []
lowerFirst (c:cs) = toLower c : cs

constCase = upperAfterUnder . map toLower
valueCase = upperAfterUnder . lowerFirst
typeCase = upperAfterUnder . upperFirst

-- Short-hands for common code generation
undefH = H.Var $ H.UnQual $ H.Ident "undefined"
unBangTyH = H.UnBangedTy . hType
dataDeclH name cons = H.DataDecl l H.DataType [] (H.Ident name) [] cons []
qConDeclH d = H.QualConDecl l [] [] d
conDeclH n ts = H.ConDecl (H.Ident n) ts
recDeclH n fields = H.RecDecl (H.Ident n) fields
typeSigH n t = H.TypeSig l [H.Ident n] t
constDeclH n t rhs = 
    [ typeSigH n t
    , H.FunBind [
          H.Match l (H.Ident n) []
                 Nothing (H.UnGuardedRhs rhs) (H.BDecls [])]
    ]


-- Module generation
emptyModule = modul "Empty" "Empty" []

modul :: String -> String -> [H.Decl] -> H.Module
modul ns name = H.Module l 
                (H.ModuleName $ ns ++ "." ++ name) 
                [H.LanguagePragma l [H.Ident "ForeignFunctionInterface"]]
                Nothing
                Nothing
                []

-- Generate constants all in one module
genConsts :: [API] -> [H.Decl]
genConsts cs = concatMap apiConst cs

apiConst (APIConst nConst) = constDecl nConst

constDecl (Named _ str cnst) = 
    constDeclH (constCase str) (valueHType $ constValue cnst)
                               (valueExpr $ constValue cnst)


-- Objects
genObject (Named ns n (Object name mbParent fields methods props))
    = let objectDecls = concatMap (functionDecl (strType (typeCase n))) methods
          parentStuff = 
            case mbParent of
              Just (Named parentNs parentName _) -> 
                let noName = parentNs ++ "." ++ parentName -- objName (named no)
                in [dataDeclH noName []] -- qConDeclH $ conDecl noName]
              Nothing -> []
      in parentStuff ++ objectDecls

argTypeH = hType . argType
argTypesH = map argTypeH . args

ioArgs args ret 
    = foldr H.TyFun (H.TyApp (strType "IO") (hType ret)) args

callableDecl objType name call 
    = let t = ioArgs (objType : argTypesH call) (returnType call)
      in [ffiCall name t]

ffiCall name t = 
    H.ForImp l H.CCall (H.PlaySafe False) name (H.Ident $ valueCase name) t

constructorDecl name call
    = let t = ioArgs (argTypesH call) (returnType call)
      in [ffiCall name t] -- constDeclH (valueCase name) t undefH

functionDecl objType (Function symbol flags (Named ns n callable))
    | FunctionIsConstructor `elem` flags = constructorDecl symbol callable
    | FunctionIsMethod `elem` flags = callableDecl objType symbol callable
    | otherwise = callableDecl objType symbol callable


genStruct (Named ns n struct)
    = let nameStr = ns ++ n
          d = dataDeclH nameStr [structCon nameStr struct]
      in [d]

structCon typeName (Struct fields)
     = let fieldDecl (Field name typ flags) 
               = ([H.Ident $ name ++ show flags], unBangTyH typ)
       in qConDeclH (recDeclH typeName (map fieldDecl fields))

genFlags (Named ns n (Flags (Enumeration vals)))
    = [enumDecl n vals]

genCallback (Callback (Named ns n call))
    = let cb = constructorDecl n call
      in cb

enumDecl n vals
    = let mkConstr (conName, _) = qConDeclH $ conDeclH (typeCase conName) []
      in dataDeclH n (map mkConstr vals)


genInterface (Named ns n (Interface meths consts props))
    = let methDecls = concatMap (functionDecl (strType $ ns ++ "." ++ n)) meths
          constDecls = concatMap constDecl consts
      in methDecls ++ constDecls


genEnum (Named ns n (Enumeration vals)) 
    = [enumDecl n vals]

genFunction (Function symbol flags (Named ns n callable))
    = constructorDecl symbol callable
