
module GI.Type
    ( BasicType(..)
    , Type(..)
    , typeFromTypeInfo
    , typeFromTypeInfoOrFunc
    , io
    , ptr
    , con
    , haskellType
    , foreignType
    ) where

import Data.Int
import Data.Typeable
import Data.Word

import GI.Internal.BaseInfo
import GI.Internal.TypeInfo
import GI.Internal.Types

-- This enum mirrors the definition in gitypes.h.
data BasicType
     = TVoid
     | TBoolean
     | TInt8
     | TUInt8
     | TInt16
     | TUInt16
     | TInt32
     | TUInt32
     | TInt64
     | TUInt64
     | TFloat
     | TDouble
     | TUniChar
     | TGType
     | TUTF8
     | TFileName
    deriving (Eq, Enum, Ord)

instance Show BasicType where
    show TVoid = "void"
    show TBoolean = "bool"
    show TInt8 = "int8"
    show TUInt8 = "uint8"
    show TInt16 = "int16"
    show TUInt16 = "uint16"
    show TInt32 = "int32"
    show TUInt32 = "uint32"
    show TInt64 = "int64"
    show TUInt64 = "uint64"
    show TFloat = "float"
    show TDouble = "double"
    show TUniChar = "unichar"
    show TGType = "gtype"
    show TUTF8  = "char"
    show TFileName = "filename"

data Type
    = TBasicType BasicType
    | TPtr Type
    | TArray Type
    | TInterface String String
    | TGList Type
    | TGSList Type
    | TGHash Type Type
    | TError
    deriving (Eq, Ord)

instance Show Type where
   show (TBasicType bt) = show bt
   show (TPtr t) = show t ++ "*"
   show (TArray t) = show t ++ "[]"
   show (TInterface str1 str2) = str1 ++ "." ++ str2
   show (TGList t) = "glist <" ++ show t ++ ">"
   show (TGSList t) = "gslist <" ++ show t ++ ">"
   show (TGHash t1 t2) = "gslist <" ++ show t1 ++ " -> " ++ show t2 ++ ">"
   show TError = "errorT"

basicTypeFromTypeTag TypeTagVoid = Just TVoid
basicTypeFromTypeTag TypeTagBoolean = Just TBoolean
basicTypeFromTypeTag TypeTagInt8 = Just TInt8
basicTypeFromTypeTag TypeTagInt16 = Just TInt16
basicTypeFromTypeTag TypeTagInt32 = Just TInt32
basicTypeFromTypeTag TypeTagInt64 = Just TInt64
basicTypeFromTypeTag TypeTagUint8 = Just TUInt8
basicTypeFromTypeTag TypeTagUint16 = Just TUInt16
basicTypeFromTypeTag TypeTagUint32 = Just TUInt32
basicTypeFromTypeTag TypeTagUint64 = Just TUInt64
basicTypeFromTypeTag TypeTagFloat = Just TFloat
basicTypeFromTypeTag TypeTagDouble = Just TDouble
basicTypeFromTypeTag TypeTagUnichar = Just TUniChar
basicTypeFromTypeTag TypeTagUtf8 = Just TUTF8
basicTypeFromTypeTag TypeTagFilename = Just TFileName
basicTypeFromTypeTag TypeTagGtype = Just TGType
basicTypeFromTypeTag _ = Nothing

typeFromTypeInfo :: TypeInfo -> Type
typeFromTypeInfo ti =
    case basicTypeFromTypeTag tag of
      Just bt -> if typeInfoIsPointer ti 
                 then TPtr (TBasicType bt) --  (error ("pointer!: " ++ show bt))
                 else TBasicType bt
      Nothing -> case tag of
           TypeTagArray -> TArray p1
           -- TypeTagInterface -> TInterface (typeTagToString . typeInfoTag $ ti)
           TypeTagInterface ->
               let bi = typeInfoInterface ti
                   namespace = baseInfoNamespace bi
                   name = baseInfoName bi
               in TInterface namespace name
           TypeTagGlist -> TGList p1
           TypeTagGslist -> TGSList p1
           TypeTagGhash -> TGHash p1 p2
           -- XXX: Include more information.
           TypeTagError -> TError
           _ -> error $ "implement me: " ++ show (tag, fromEnum tag, fromEnum TypeTagArray)

    where tag = typeInfoTag ti
          p1 = typeFromTypeInfo $ typeInfoParamType ti 0
          p2 = typeFromTypeInfo $ typeInfoParamType ti 1


typeFromTypeInfoOrFunc :: TypeInfo -> Either Type FunctionInfo
typeFromTypeInfoOrFunc ti =
    case basicTypeFromTypeTag tag of
      Just bt -> Left $ if typeInfoIsPointer ti 
                        then TPtr (TBasicType bt) --  (error ("pointer!: " ++ show bt))
                        else TBasicType bt
      Nothing -> case tag of
           TypeTagArray -> Left $ TArray p1
           -- TypeTagInterface -> TInterface (typeTagToString . typeInfoTag $ ti)
           TypeTagInterface ->
               let bi = typeInfoInterface ti
                   namespace = baseInfoNamespace bi
                   name = baseInfoName bi
               in case baseInfoType bi of
                 InfoTypeInterface -> Left $ TInterface namespace name
                 InfoTypeObject -> Left $ TInterface namespace name
                 InfoTypeEnum -> Left $ TInterface namespace name
                 InfoTypeStruct -> Left $ TInterface namespace name
                 InfoTypeFlags -> Left $ TInterface namespace name
                 InfoTypeUnion -> Left $ TInterface namespace name
                 InfoTypeCallback -> Right $ fromBaseInfo bi
                 i -> error $ "typeFromTypeInfo: " ++ show i
           TypeTagGlist -> Left $ TGList p1
           TypeTagGslist -> Left $ TGSList p1
           TypeTagGhash -> Left $ TGHash p1 p2
           -- XXX: Include more information.
           TypeTagError -> Left TError
           _ -> error $ "implement me: " ++ show (tag, fromEnum tag, fromEnum TypeTagArray)

    where tag = typeInfoTag ti
          p1 = typeFromTypeInfo $ typeInfoParamType ti 0
          p2 = typeFromTypeInfo $ typeInfoParamType ti 1



con :: String -> [TypeRep] -> TypeRep
con s xs = mkTyConApp (mkTyCon3 "GI" "Type" s) xs

io :: TypeRep -> TypeRep
io t = "IO" `con` [t]

ptr :: TypeRep -> TypeRep
ptr t = "Ptr" `con` [t]

haskellBasicType TVoid    = typeOf ()
haskellBasicType TBoolean = typeOf True
haskellBasicType TInt8    = typeOf (0 :: Int8)
haskellBasicType TUInt8   = typeOf (0 :: Word8)
haskellBasicType TInt16   = typeOf (0 :: Int16)
haskellBasicType TUInt16  = typeOf (0 :: Word16)
haskellBasicType TInt32   = typeOf (0 :: Int32)
haskellBasicType TUInt32  = typeOf (0 :: Word32)
haskellBasicType TInt64   = typeOf (0 :: Int64)
haskellBasicType TUInt64  = typeOf (0 :: Word64)
-- XXX: Is this correct?
haskellBasicType TGType   = typeOf (0 :: Word)
haskellBasicType TUTF8    = typeOf ""
haskellBasicType TFloat   = typeOf (0 :: Float)
haskellBasicType TDouble  = typeOf (0 :: Double)
haskellBasicType TUniChar = typeOf ('\0' :: Char)
haskellBasicType t        = error $ "haskellBasicType: " ++ show t

haskellType :: Type -> TypeRep
haskellType (TBasicType bt) = haskellBasicType bt
haskellType (TArray a) = "GArray" `con` [haskellType a]
haskellType (TGList a) = "GList" `con` [haskellType a]
haskellType (TGSList a) = "GSList" `con` [haskellType a]
haskellType (TGHash a b) = "GHashTable" `con` [haskellType a, haskellType b]
haskellType TError = "GError" `con` []
-- XXX: Possibly nonsense. Perhaps the interface name needs to be
-- qualified, and its existence (in the typelib we're generating code
-- for, or some other typelib) verified.
haskellType (TInterface _ s) = s `con` []

foreignBasicType TBoolean = "CInt" `con` []
foreignBasicType TUTF8    = "CString" `con` []
foreignBasicType TGType   = "GType" `con` []
foreignBasicType t        = haskellBasicType t

foreignType :: Type -> TypeRep
foreignType (TBasicType t) = foreignBasicType t
foreignType t@(TArray _ ) = haskellType t
foreignType t@(TGList _) = haskellType t
foreignType t@(TGSList _) = haskellType t
foreignType t@(TGHash _ _) = haskellType t
foreignType t@TError = haskellType t
foreignType t@(TInterface _ _) = haskellType t
