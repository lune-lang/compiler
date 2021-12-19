module Document.Generate (docModules) where

import qualified Data.Map as Map
import Data.Map (Map)

import Syntax.Common
import Syntax.Frontend

docDef :: Def -> String
docDef (def, _, str) =
  case def of
    Annotation {} -> codeHeading str
    Foreign {} -> codeHeading str
    Func {} -> ""
    Expand {} -> codeHeading str
    Type {} -> codeHeading str
    Synonym {} -> codeHeading str
    Infix {} -> codeHeading str
    Syntax {} -> ""
    Documentation markdown -> indent markdown

indent :: String -> String
indent = unlines . map ("  " ++) . lines

codeHeading :: String -> String
codeHeading str = let
  heading = unlines [ "<h4>\n", indent str, "</h4>" ]
  in indent heading

docModule :: (ModName, Module) -> String
docModule (modName, m) = unlines
  [ "# " ++ modName
  , "<details>"
  , "  <summary></summary>\n"
  , concatMap docDef (getDefs m)
  , "</details>\n"
  ]

docModules :: Map ModName Module -> String
docModules = concatMap docModule . Map.toList
