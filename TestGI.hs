module Main where

import Control.Applicative

import Data.Char (toUpper)

import Language.Haskell.Exts.Pretty (prettyPrint)

-- import System.Glib.Initialize
-- import System.Glib.GError
import System.Glib.GType


import GI.API --  (loadAPI)
import GI.Internal.Typelib
import GI.CodeGen
import GI.SyntaxBuilder
import GI.SimpleModule

onlyButton (APIObject (Named "Gtk" "Widget" _)) = True
onlyButton _ = False


-- isIgnored :: API -> Bool
-- isIgnored (APIFunction (Function n _ _)) = skipFunc n
-- isIgnored _ = False

-- skipFunc :: String -> Bool
-- skipFunc str = str `elem` ignored
--   where
--     ignored = ["gtk_binding_entry_add_signal_from_string"
--               ,"gtk_binding_entry_add_signall"
--               ,"gtk_binding_entry_remove"
--               ,"gtk_binding_entry_skip"
--               ,"gtk_binding_set_find"
--               ,"gtk_paper_size_get_default"
--               ,"gtk_paper_size_get_paper_sizes"
--               ,"gtk_tree_row_reference_deleted"
--               ,"gtk_tree_row_reference_inserted"
--               ]

extraDecls str = case str of
  "GLib" -> emptyPtr "GType"
  "GObject" -> emptyPtr "VaClosureMarshal"
  _ -> []

processAPI name = do
  glibTypeInit
  -- apis <- filter (not . isIgnored) <$> loadAPI name
  apis <- loadAPI name
  deps <- getDependencies name
  print deps
  let name' = upperFirst name  


  let 
    depTrimmed  = map (takeWhile (/= '-') . upperFirst) deps
    withImports = foldl addImport (genModules apis) depTrimmed
    withExtras  = foldl addDecl withImports (extraDecls name)
    str = prettyPrint $ simpleToRealModule name' withExtras
      
  writeFile (name' ++ ".hs") str
--  putStrLn str

