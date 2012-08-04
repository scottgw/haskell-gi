
module GI.Internal.StructInfo
    ( structInfoFields
    , structInfoMethods
    -- XXX: Implement these.
    
    -- , structInfoSize
    -- , structInfoAlignment
    -- , structInfoIsGTypeStruct
    )
where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C

import GI.Util (getList)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_struct_info" #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: StructInfoClass sic => sic -> Ptr ()
stupidCast si = castPtr p
  where (StructInfo p) = structInfo si

structInfoFields :: StructInfoClass sic => sic -> [FieldInfo]
structInfoFields si = unsafePerformIO $
    map (FieldInfo <$> castPtr) <$>
    getList {# call get_n_fields #} {# call get_field #} (stupidCast si)

structInfoMethods :: StructInfoClass sic => sic -> [FunctionInfo]
structInfoMethods si = unsafePerformIO $
    map (FunctionInfo <$> castPtr) <$>
    getList {# call get_n_methods #} {# call get_method #} (stupidCast si)
