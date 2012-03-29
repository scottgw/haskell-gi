
module GI.Internal.ObjectInfo
    ( objectInfoFields
    , objectInfoMethods
    , objectInfoProperties
    , objectInfoParent
    , objectInfoName
    -- , objectInfoSignals
    -- , objectInfoConstants
    -- , objectInfoVFuncs
    -- , objectInfoInterfaces
    -- XXX: lots more stuff missing
    ) where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C

import GI.Util (getList)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_object_info" #}

stupidCast :: ObjectInfoClass oic => oic -> Ptr ()
stupidCast oi = castPtr p
    where (ObjectInfo p) = objectInfo oi

objectInfoFields :: ObjectInfoClass oic => oic -> [FieldInfo]
objectInfoFields oi = unsafePerformIO $
    map (FieldInfo <$> castPtr) <$>
    getList {# call get_n_fields #} {# call get_field #} (stupidCast oi)

objectInfoMethods :: ObjectInfoClass oic => oic -> [FunctionInfo]
objectInfoMethods oi = unsafePerformIO $
    map (FunctionInfo <$> castPtr) <$>
    getList {# call get_n_methods #} {# call get_method #} (stupidCast oi)

objectInfoProperties :: ObjectInfoClass oic => oic -> [PropertyInfo]
objectInfoProperties oi = unsafePerformIO $
    map (PropertyInfo <$> castPtr) <$>
    getList {# call get_n_properties #} {# call get_property #} (stupidCast oi)

objectInfoParent :: ObjectInfoClass oic => oic -> Maybe ObjectInfo
objectInfoParent oi = unsafePerformIO $
    do parent <- {# call get_parent #} (stupidCast oi)
       if parent == nullPtr then return Nothing
                            else return (Just $ ObjectInfo $ castPtr parent)

objectInfoName :: ObjectInfoClass oic => oic -> String
objectInfoName oi = unsafePerformIO $
    {# call get_type_name #} (stupidCast oi) >>= peekCString