module GI.SyntaxBuilder where

import Control.Applicative

import Data.Char

import qualified Language.Haskell.Exts.Syntax as H


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
   ]]
  Nothing
  Nothing
  ([imp "Foreign", imp "Foreign.C"] ++ imports)

qimp impName = import_ True impName (Just impName)
imp impName = import_ False impName Nothing

import_ qual name asNameMb = 
  H.ImportDecl l (H.ModuleName name) qual False Nothing 
                 (H.ModuleName <$> asNameMb) Nothing



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
funDefH n t pats rhs = 
    [ typeSigH n t
    , H.FunBind [
          H.Match l (H.Ident n) pats
                 Nothing (H.UnGuardedRhs rhs) (H.BDecls [])]
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
upperAfterUnder [] = []
upperAfterUnder ('_':c:cs) = toUpper c : upperAfterUnder cs
upperAfterUnder (c:cs) = c:upperAfterUnder cs

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
