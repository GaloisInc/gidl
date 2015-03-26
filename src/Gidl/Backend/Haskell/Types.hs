
module Gidl.Backend.Haskell.Types where

import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper)
import Gidl.Types
import Ivory.Artifact
import Text.PrettyPrint.Mainland

typeModule :: [String] -> TypeRepr -> Artifact
typeModule modulepath tr@(TypeRepr _ td) =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((typeModuleName tr) ++ ".hs") $
  prettyLazyText 80 $
  stack
    [ text "module"
      <+> tm (typeModuleName tr)
      <+> text "where"
    , empty
    , stack $ map (importDecl tm)
            $ nub
            $ map importType
            $ typeLeaves td
    , empty
    , typeDecl typename td
    ]
  where
  typename = typeModuleName tr
  tm mname = mconcat $ punctuate dot
                     $ map text (modulepath ++ [mname])

typeHaskellType :: TypeRepr -> String
typeHaskellType (TypeRepr tn (StructType _)) = userTypeModuleName tn
typeHaskellType (TypeRepr tn (NewtypeType _)) = userTypeModuleName tn
typeHaskellType (TypeRepr tn (EnumType _)) = userTypeModuleName tn
typeHaskellType (TypeRepr _ (AtomType a)) = case a of
  AtomInt Bits8  -> "Int8"
  AtomInt Bits16 -> "Int16"
  AtomInt Bits32 -> "Int32"
  AtomInt Bits64 -> "Int64"
  AtomWord Bits8  -> "Word8"
  AtomWord Bits16 -> "Word16"
  AtomWord Bits32 -> "Word32"
  AtomWord Bits64 -> "Word64"
  AtomFloat -> "Float"
  AtomDouble -> "Double"
typeHaskellType (TypeRepr _ VoidType) = "()"

typeModuleName :: TypeRepr -> String
typeModuleName (TypeRepr tn (StructType _)) = userTypeModuleName tn
typeModuleName (TypeRepr tn (NewtypeType _)) = userTypeModuleName tn
typeModuleName (TypeRepr tn (EnumType _)) = userTypeModuleName tn
typeModuleName (TypeRepr _ (AtomType _)) = error "do not take typeModuleName of an AtomType"
typeModuleName (TypeRepr _ VoidType) = error "do not take typeModuleName of a VoidType"

userTypeModuleName :: String -> String
userTypeModuleName = first_cap . u_to_camel
  where
  first_cap (s:ss) = (toUpper s) : ss
  first_cap []     = []
  u_to_camel ('_':'t':[]) = []
  u_to_camel ('_':[]) = []
  u_to_camel ('_':a:as) = (toUpper a) : u_to_camel as
  u_to_camel (a:as) = a : u_to_camel as
  u_to_camel [] = []

typeDecl :: TypeName -> Type TypeRepr -> Doc
typeDecl tname (StructType (Struct ss)) = stack
  [ text "data" <+> text tname <+> equals
  , indent 2 $ text tname
  , indent 4 $ encloseStack lbrace (rbrace <+> deriv) comma
      [ text i <+> colon <> colon <+> text (typeHaskellType t)
      | (i,t) <- ss ]
  ]
  where deriv = typeDeriving ["Eq", "Show"]
typeDecl tname (NewtypeType (Newtype n)) = stack
  [ text "newtype" <+> text tname <+> equals
  , indent 2 $ text tname <+> align
        (lbrace <+> text ("un" ++ tname) <+> text "::" <+>
         text (typeHaskellType n) </>
         rbrace <+> typeDeriving ["Eq", "Show"])
  ]
typeDecl tname (EnumType (EnumT _ es)) = stack
  [ text "data" <+> text tname
  , indent 2 $ encloseStack equals deriv (text "|")
      [ text (userTypeModuleName i)
      | (i, _) <- es ]
  , empty
  , text "instance Enum" <+> text tname <+> text "where"
  , indent 2 $ stack $
      [ text "toEnum" <+> ppr e <+> equals <+> text (userTypeModuleName i)
      | (i,e) <- es ] ++
      [ text ("toEnum _ = error \"toEnum: invalid value for " ++ tname ++ "\"") ] ++
      [ text "fromEnum" <+> text (userTypeModuleName i) <+> equals <+> ppr e
      | (i,e) <- es ]
  ]
  where deriv = typeDeriving ["Eq", "Show", "Ord"]
typeDecl tn _ = error ("typeDecl: cannot create decl for built in type " ++ tn)

typeDeriving :: [String] -> Doc
typeDeriving cs = text "deriving" <+> parens (commasep (map text cs))

data ImportType = LibraryType String
                | UserType String
                | NoImport
                deriving (Eq, Show)

importType :: TypeRepr -> ImportType
importType (TypeRepr _ (AtomType a)) =
  case a of
    AtomWord _ -> LibraryType "Data.Word"
    AtomInt _ -> LibraryType "Data.Int"
    _ -> NoImport
importType (TypeRepr _ VoidType) = NoImport
importType (TypeRepr n _) = UserType n

importDecl :: (String -> Doc) -> ImportType -> Doc
importDecl _ (LibraryType p) =
  text "import" <+> text p
importDecl mkpath (UserType t) =
  text "import" <+> mkpath (userTypeModuleName t)
importDecl _ NoImport = empty


encloseStack :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseStack l r p ds = case ds of
  [] -> l </> r
  [d] -> l <+> d </> r
  _ -> align (l <+> (folddoc (\a b -> a </> p <+> b) ds) </> r)

