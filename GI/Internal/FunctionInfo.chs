
module GI.Internal.FunctionInfo
    ( functionInfoSymbol
    -- XXX: Write these.
    , functionInfoFlags
    -- , functionInfoProperty
    -- , functionInfoVFunc
    , FunctionInfoFlag (..)
    )
where

import Foreign
import Foreign.C

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_function_info" #}

{# enum GIFunctionInfoFlags as FunctionInfoFlag {underscoreToCase} with prefix="GI" deriving (Eq) #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: FunctionInfoClass fic => fic -> Ptr ()
stupidCast fi = castPtr p
  where (FunctionInfo p) = functionInfo fi

functionInfoSymbol :: FunctionInfoClass fic => fic -> String
functionInfoSymbol fi = unsafePerformIO $ peekCString =<<
    {# call get_symbol #} (stupidCast fi)


functionInfoFlags :: FunctionInfoClass fic => fic -> [FunctionInfoFlag]
functionInfoFlags fi = map (toEnum . (2^)) flagNums
    where
      flags = unsafePerformIO $  {# call get_flags #} (stupidCast fi)
      hasFlag n = bit n .&. flags /= 0
      flagNums = filter hasFlag [0..5]
               