module GI.Internal.Typelib
  ( getSearchPath
  , getDependencies
  , getLoadedNamespaces
  , getInfos
  , load
  )
where

import Foreign
import Foreign.C

import Control.Applicative ((<$>))
import Control.Monad (when)

import System.Glib.GError
import System.Glib.GList

import GI.Internal.Types
import GI.Util

#include <girepository.h>

{#context prefix="g_irepository"#}

{# pointer *GITypelib as Typelib newtype #}
unTypelib :: Typelib -> Ptr Typelib
unTypelib (Typelib p) = p

{# pointer *GIRepository as Repository newtype #}

nullRepository = Repository nullPtr

getSearchPath :: IO [FilePath]
getSearchPath = do
    paths <- {# call unsafe get_search_path #}
    pathPtrs <- readGSList paths
    mapM peekCString pathPtrs

mapCStrings f ptr = do
  str <- peek ptr
  if str == nullPtr
      then return []
      else do
          -- XXX: O(n) in size
          x <- f str
          xs <- mapCStrings f $ ptr `plusPtr` sizeOf (undefined :: Ptr CString)
          return $ x : xs

peekCStrings = mapCStrings peekCString

getDependencies :: String -> IO [String]
getDependencies namespace =
  withCString namespace $ \cstring -> do
    depsPtr  <- {# call unsafe get_dependencies #} nullRepository cstring
    if depsPtr == nullPtr
      then return []
      else do
        cstrings <- peekArray0 nullPtr depsPtr
        strs     <- mapM peekCString cstrings
        mapM_ free cstrings
        return strs 

getLoadedNamespaces :: IO [String]
getLoadedNamespaces = do
    nsPtrs <- {# call unsafe get_loaded_namespaces #} nullRepository
    nss <- peekCStrings nsPtrs
    _ <- mapCStrings free nsPtrs
    free nsPtrs
    return nss

getInfos :: Typelib -> IO [BaseInfo]
getInfos typelib = do
    nsPtr <- {# call unsafe g_typelib_get_namespace #} typelib
    map (BaseInfo <$> castPtr) <$> getList
        ({# call unsafe get_n_infos #} nullRepository)
        ({# call unsafe get_info #} nullRepository)
        nsPtr

load :: String -> Maybe String -> IO Typelib
load namespace version =
    withCString namespace $ \nsPtr ->
    maybeWithCString version $ \versionPtr ->
    propagateGError $ \gError -> do
        -- _require()'s return is annotated as 'transfer none'. I'm assuming
        -- that we don't need to ref this because it's never going to be freed,
        -- though, so we're fine.
        typelib <- {# call unsafe require #} nullRepository nsPtr versionPtr 0
                                             gError
        when (unTypelib typelib /= nullPtr) $ do
            _ <- {# call unsafe load_typelib #} nullRepository typelib 0 gError
            return ()
        return typelib
