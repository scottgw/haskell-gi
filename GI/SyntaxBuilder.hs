{-# LANGUAGE TemplateHaskell #-}
module GI.SyntaxBuilder where

import Control.Applicative

import Data.Char
import Data.List (foldl')
import Data.Label

import qualified Language.Haskell.Exts.Syntax as H


data HaskDecl = HaskDecl
                { _haskTypeDecl :: H.Type
                , _haskFunName  :: String
                , _haskArgNames :: [String]
                , _haskFuncExp  :: H.Exp
                }

mkLabels [''HaskDecl]


l = H.SrcLoc "<unknown>.hs" 0 0

-- Module generation
modul ns name = modulImport ns name []

modulImport :: String -> [H.ImportDecl] -> [H.Decl] -> H.Module
modulImport name imports = 
  H.Module l 
  (H.ModuleName name)
  [H.LanguagePragma l 
   [ H.Ident "ForeignFunctionInterface"
   , H.Ident "EmptyDataDecls"
   , H.Ident "TypeSynonymInstances"
   , H.Ident "FlexibleInstances"
   ]]
  Nothing
  Nothing
  ([imp "Foreign", imp "Foreign.C", imp "Foreign.Ptr"] ++ imports)

qimp impName = import_ True impName (Just impName)
imp impName = import_ False impName Nothing

import_ qual name asNameMb = 
  H.ImportDecl l (H.ModuleName name) qual False Nothing 
                 (H.ModuleName <$> asNameMb) Nothing

applies :: String -> [H.Exp] -> H.Exp
applies func = foldl' H.App (evar func)


-- | Type class creation
typeClass :: H.Context -> String -> [String] -> [H.Decl] -> H.Decl
typeClass ctx name vars functions =
  H.ClassDecl l ctx 
      (H.Ident name) 
      (map (H.UnkindedVar . H.Ident) vars)
      []
      (map H.ClsDecl functions)

-- | Type class instance creation
instance_ :: H.Context -> String -> H.Type -> [H.Decl] -> H.Decl
instance_ ctx className instanceType decls =
  H.InstDecl l ctx (unqual className) [instanceType]
    (map H.InsDecl decls)

-- Short-hands for common code generation
anyPtrH = ptrTypeH (H.TyVar $ H.Ident "a")

undefH = evar "undefined"
errorH s = H.App (evar "error") (H.Lit $ H.String s)
emptyPtr name = 
  let emptyName = name ++ "_" 
  in [emptyData emptyName, typeDeclH name (ptrTypeH $ strType emptyName)]
                           
emptyData name = dataDeclH name []

dataDeclH name cons = H.DataDecl l H.DataType [] (H.Ident name) [] cons []
typeDeclH name type_ = H.TypeDecl l (H.Ident name) [] type_
ptrTypeH = H.TyApp (strType "Ptr")
unqual = H.UnQual . H.Ident

qConDeclH d = H.QualConDecl l [] [] d
conDeclH n ts = H.ConDecl (H.Ident n) ts
recDeclH n fields = H.RecDecl (H.Ident n) fields
typeSigH n t = H.TypeSig l [H.Ident n] t
evar = H.Var . unqual
pvar = H.PVar . H.Ident

fromHaskDecl (HaskDecl type_ name args expr) = 
  funDefH name type_ (map pvar args) expr

funBind name pats rhs = 
  H.FunBind [H.Match l (H.Ident name) pats
               Nothing (H.UnGuardedRhs rhs) (H.BDecls [])]

funDefH :: String -> H.Type -> [H.Pat] -> H.Exp -> [H.Decl]
funDefH name t pats rhs = 
  [ typeSigH name t
  , funBind name pats rhs
  ]

genH p e = H.Generator l p e

strType :: String -> H.Type
strType = H.TyCon . H.UnQual . H.Ident

-- numeric conversions
num :: Integral a => a -> H.Exp
num = H.Lit . H.Int . fromIntegral

float :: Real a => a -> H.Exp
float = H.Lit . H.Frac . toRational


-- Name conversions

safeName str = 
  case str of
    "data" -> "data_"
    "type" -> "type_"
    "module" -> "module_"
    "where" -> "where_"
    "in" -> "in_"
    "instance" -> "instance_"
    "String" -> "GString"
    "Error" -> "GError"
    -- "IOError" -> "GLibIOError"
    s -> s
  

-- uppercase the letter after an underscore
cName = ("c_" ++)

upperAfterUnder :: String -> String
upperAfterUnder []         = []
upperAfterUnder ('_':c:cs) = toUpper c : upperAfterUnder cs
upperAfterUnder str@(c:cs)     
  | take 12 str == "ObjectObject" = error "upperAfterUnder: "
  | otherwise = c : upperAfterUnder cs

upperFirst [] = []
upperFirst (c:cs) 
  | isDigit c = 'N' : c : cs
  | otherwise = toUpper c : cs

lowerFirst [] = []
lowerFirst (c:cs) = toLower c : cs

constCase = lowerFirst -- . map toUpper
valueCase = upperAfterUnder . lowerFirst
typeCase = upperAfterUnder . upperFirst

unBangTyH = H.UnBangedTy
