
module GI.Value
    ( Value(..)
    , fromArgument
    , valueType
    ) where

import Control.Applicative ((<$>))
import Data.Int
import Data.Word
import Foreign
import Foreign.C

import GI.Type
import GI.Internal.Types

#include <girepository.h>

data Value
    = VVoid
    | VBoolean Bool
    | VInt8 Int8
    | VUInt8 Word8
    | VInt16 Int16
    | VUInt16 Word16
    | VInt32 Int32
    | VUInt32 Word32
    | VInt64 Int64
    | VUInt64 Word64
    | VFloat Float
    | VDouble Double
    | VGType Word32
    | VUTF8 Char
    | VUTF8Ptr String
    | VFileName String
    deriving (Eq, Show)

valueType :: Value -> Type
valueType VVoid           = TBasicType TVoid
valueType (VBoolean _)    = TBasicType TBoolean
valueType (VInt8 _)       = TBasicType TInt8
valueType (VUInt8 _)      = TBasicType TUInt8
valueType (VInt16 _)      = TBasicType TInt16
valueType (VUInt16 _)     = TBasicType TUInt16
valueType (VInt32 _)      = TBasicType TInt32
valueType (VUInt32 _)     = TBasicType TUInt32
valueType (VInt64 _)      = TBasicType TInt64
valueType (VUInt64 _)     = TBasicType TUInt64
valueType (VFloat _)      = TBasicType TFloat
valueType (VDouble _)     = TBasicType TDouble
valueType (VGType _)      = TBasicType TGType
valueType (VUTF8 _)       = TBasicType TUTF8
valueType (VUTF8Ptr _)    = TPtr (TBasicType TUTF8)
valueType (VFileName _)   = TBasicType TFileName

fromArgument :: TypeInfo -> Argument -> Value
fromArgument ti (Argument arg) =
    case typeFromTypeInfo ti of
        TBasicType t -> unsafePerformIO $ basic t
        TPtr (TBasicType TUTF8) -> unsafePerformIO utf8ptr
        t -> error $ "don't know how to decode argument of type " ++
                show t

    where

    basic TInt32 = VInt32 <$> fromIntegral <$> {# get GIArgument->v_int32 #} arg
    -- XXX: Loss of precision?
    basic TDouble = VDouble <$> fromRational <$> toRational <$>  {# get GIArgument->v_double #} arg
    basic t = error $ "a: implement me: " ++ show t

    utf8ptr = VUTF8Ptr <$> (peekCString =<< {# get GIArgument->v_string #} arg)

