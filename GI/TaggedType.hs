{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module GI.TaggedType where

import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr

import qualified Language.Haskell.Exts.Syntax as H

import GI.SyntaxBuilder
import GI.Type

data HaskTag
data CTag

data GList a
data GSList a
data GArray a
data GType
data GHash k v
data GInterface
data GError
data File

hReturn :: TaggedType HaskTag a -> H.Exp -> H.Exp
hReturn t =
  case t of
    FileNameType -> returnStr
    PtrType UTF8Type -> returnStr
    _ -> id
  where
    returnStr e =
      H.InfixApp e (H.QConOp (H.UnQual (H.Symbol ">>="))) (evar "peekCString")

hType :: TaggedType HaskTag a -> H.Type
hType t = case t of
  BoolType   -> strType "Bool"
  Int8Type   -> strType "Int8"
  UInt8Type  -> strType "Word8"
  Int16Type  -> strType "Int16"
  UInt16Type -> strType "Word16"
  Int32Type  -> strType "Int32"
  UInt32Type -> strType "Word32"
  Int64Type  -> strType "Int64"
  UInt64Type -> strType "Word64"
  FloatType  -> strType "Float"
  DoubleType -> strType "Double"
  UniCharType -> strType "Char"
  GTypeType   -> strType "GLib.GType"
  UTF8Type    -> strType "CChar"
  FileNameType -> strType "String"
  VoidType     -> strType "()"
  ErrorType    -> strType "GLib.GError"
  
  InterfaceType namespace name -> interfaceType namespace name
  
  PtrType UTF8Type -> strType "String"
  PtrType v -> anyPtrH -- ptrTypeH (hType' v)
  
  ArrayType v -> anyPtrH -- ptrTypeH (hType' v) -- H.TyList (hType' v)
  ListType v -> strType "GLib.List" -- H.TyList (hType' v)
  SListType v -> strType "GLib.SList" -- H.TyList (hType' v)
  HashType k v -> strType "GLib.HashTable"

cType :: TaggedType CTag a -> H.Type
cType t = case t of
  BoolType   -> strType "Bool"
  Int8Type   -> strType "Int8"
  UInt8Type  -> strType "Word8"
  Int16Type  -> strType "Int16"
  UInt16Type -> strType "Word16"
  Int32Type  -> strType "Int32"
  UInt32Type -> strType "Word32"
  Int64Type  -> strType "Int64"
  UInt64Type -> strType "Word64"
  FloatType  -> strType "Float"
  DoubleType -> strType "Double"
  UniCharType -> strType "Char"
  GTypeType   -> strType "GLib.GType"
  UTF8Type    -> strType "CChar"
  FileNameType -> strType "CString"
  VoidType     -> strType "()"
  ErrorType    -> strType "GLib.GError"
  
  InterfaceType namespace name -> interfaceType namespace name
  
  PtrType UTF8Type -> strType "CString"
  PtrType v -> anyPtrH
  
  ArrayType v -> anyPtrH
  ListType v -> strType "GLib.List"
  SListType v -> strType "GLib.SList"
  HashType k v -> strType "GLib.HashTable"

safeTypeName = typeCase . safeName

interfaceType namespace name =
  strType (safeTypeName namespace ++ "." ++ safeTypeName name)

data TaggedType tag type_ where
  BoolType :: TaggedType tag Bool
  
  Int8Type :: TaggedType tag Int8
  UInt8Type :: TaggedType tag Word8
  Int16Type :: TaggedType tag Int16
  UInt16Type :: TaggedType tag Word16
  Int32Type :: TaggedType tag Int32
  UInt32Type :: TaggedType tag Word32
  Int64Type :: TaggedType tag Int64
  UInt64Type :: TaggedType tag Word64
  
  FloatType :: TaggedType tag Float
  DoubleType :: TaggedType tag Double
  
  UniCharType :: TaggedType tag Char
  
  GTypeType :: TaggedType tag GType
  
  UTF8Type :: TaggedType tag CChar
  
  FileNameType :: TaggedType tag File
  
  VoidType :: TaggedType tag ()
  ErrorType :: TaggedType tag GError
  
  InterfaceType :: String -> String -> TaggedType tag GInterface
  
  PtrType :: TaggedType tag a -> TaggedType tag (Ptr a)
  
  ArrayType :: TaggedType tag a -> TaggedType tag (GArray a)
  ListType :: TaggedType tag a -> TaggedType tag (GList a)
  SListType :: TaggedType tag a -> TaggedType tag (GSList a)
  HashType :: TaggedType tag k 
               -> TaggedType tag v
               -> TaggedType tag (GHash k v)



-- We can't derive in the normal way for GADTs but apparently  
-- StandaloneDeriving will work.
deriving instance Eq (TaggedType tag type_)
deriving instance Ord (TaggedType tag type_)
               
data TypeEx tag = forall t . TypeEx {unTypeEx :: TaggedType tag t}

fmapType :: (forall t. TaggedType tag t -> a) -> TypeEx tag -> a
fmapType f (TypeEx t) = f t

toTypedEx :: Type -> TypeEx tag
toTypedEx giType = case giType of
  TBasicType TBoolean  -> TypeEx BoolType
  TBasicType TInt8     -> TypeEx Int8Type
  TBasicType TUInt8    -> TypeEx UInt8Type
  TBasicType TInt16    -> TypeEx Int16Type
  TBasicType TUInt16   -> TypeEx UInt16Type
  TBasicType TInt32    -> TypeEx Int32Type
  TBasicType TUInt32   -> TypeEx UInt32Type
  TBasicType TInt64    -> TypeEx Int64Type
  TBasicType TUInt64   -> TypeEx UInt64Type
  TBasicType TFloat    -> TypeEx FloatType
  TBasicType TDouble   -> TypeEx DoubleType
  TBasicType TUniChar  -> TypeEx UniCharType
  TBasicType TGType    -> TypeEx GTypeType
  TBasicType TUTF8     -> TypeEx UTF8Type
  TBasicType TFileName -> TypeEx FileNameType
  TBasicType TVoid     -> TypeEx VoidType
  TError               -> TypeEx ErrorType
  TInterface ns n      -> TypeEx (InterfaceType ns n)
  TPtr t               ->
    case toTypedEx t of
      TypeEx t' -> TypeEx (PtrType t')
  TArray t               ->
    case toTypedEx t of
      TypeEx t' -> TypeEx (ArrayType t')
  TGList t ->
    case toTypedEx t of
      TypeEx t' -> TypeEx (ListType t')
  TGSList t ->
    case toTypedEx t of
      TypeEx t' -> TypeEx (SListType t')
  TGHash kt vt->
    case toTypedEx kt of
      TypeEx kt' -> 
        case toTypedEx vt of
          TypeEx vt' -> TypeEx (HashType kt' vt')
