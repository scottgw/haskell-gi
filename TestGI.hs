module Main where

import Language.Haskell.Exts.Pretty (prettyPrint)

import System.Glib.GType

import GI.API --  (loadAPI)
import GI.Internal.Typelib
import GI.Generation.CodeGen
import GI.SyntaxBuilder
import GI.SimpleModule

extraDecls str = case str of
  "GLib" -> emptyPtr "GType"
  "GObject" -> emptyPtr "VaClosureMarshal"
  _ -> []

processAPI' name = processAPI name Nothing

processAPI name versionMb = do
    glibTypeInit
 
    namespace <- loadNamespace name versionMb
  
    deps <- getDependencies name
    print (name, deps)
    let name' = upperFirst name  

    let depTrimmed  = map (takeWhile (/= '-')) deps
  
    -- Don't load already loaded modules
    depNamespaces <- mapM (flip loadNamespace Nothing) depTrimmed
  
    let
      depModuleName = map upperFirst depTrimmed
      withImports = 
        foldl addImport (genModule namespace (namespace : depNamespaces)) depModuleName
      withExtras  = foldl addDecl withImports (extraDecls name)
      str = prettyPrint $ simpleToRealModule name' withExtras
      
    writeFile (name' ++ ".hs") str


main = mapM_ processAPI' [ "GLib"
                         , "GObject"
                         , "GModule"
                         , "Atk"
                         , "cairo"
                         , "Gdk"
                         , "Gio"
                         , "GdkPixbuf"
                         , "Pango"
                         , "xlib"
                         , "Gtk"
                         ]