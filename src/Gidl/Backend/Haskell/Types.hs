
module Gidl.Backend.Haskell.Types where

import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper)
import Gidl.Types
import Ivory.Artifact
import Text.PrettyPrint.Mainland

-- invariant: only make a typeModule from a StructType, NewtypeType, or EnumType
-- i.e. when isUserDefined is true.
typeModule :: [String] -> TypeRepr -> Artifact
typeModule modulepath tr@(TypeRepr _ td) =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((typeModuleName tr) ++ ".hs") $
  prettyLazyText 80 $
  stack
    [ text "{-# LANGUAGE RecordWildCards #-}"
    , empty
    , text "module"
      <+> tm (typeModuleName tr)
      <+> text "where"
    , empty
    , stack (imports ++ [text "import Data.Serialize"])
    , empty
    , typeDecl typename td
    ]
  where
  imports = map (importDecl tm)
          $ nub
          $ map importType
          $ typeLeaves td
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

serializeInstance :: TypeName -> Doc
serializeInstance tname = stack
  [ text "instance Serialize" <+> text tname <+> text "where"
  , indent 2 $ stack
      [ text "put" <+> equals <+> text ("put" ++ tname)
      , text "get" <+> equals <+> text ("get" ++ tname)
      ]
  ]

typeDecl :: TypeName -> Type TypeRepr -> Doc
typeDecl tname (StructType (Struct ss)) = stack
  [ text "data" <+> text tname <+> equals
  , indent 2 $ text tname
  , indent 4 $ encloseStack lbrace (rbrace <+> deriv) comma
      [ text i <+> colon <> colon <+> text (typeHaskellType t)
      | (i,t) <- ss ]
  , empty
  , text ("put" ++ tname) <+> colon <> colon <+> text "Putter" <+> text tname
  , text ("put" ++ tname) <+> text tname <> text "{..}" <+> equals <+> text "do"
  , indent 2 $ stack
      [ text "put" <+> text i
      | (i,_) <- ss ]
  , empty
  , text ("get" ++ tname) <+> colon <> colon <+> text "Get" <+> text tname
  , text ("get" ++ tname) <+> equals <+> text "do"
  , indent 2 $ stack $
      [ text i <+> text "<- get"
      | (i,_) <- ss ] ++
      [ text "return" <+> text tname <> text "{..}" ]
  , empty
  , serializeInstance tname
  ]
  where deriv = typeDeriving ["Eq", "Show"]

typeDecl tname (NewtypeType (Newtype n)) = stack
  [ text "newtype" <+> text tname <+> equals
  , indent 2 $ text tname <+> align
        (lbrace <+> text ("un" ++ tname) <+> text "::" <+>
         text (typeHaskellType n) </>
         rbrace <+> typeDeriving ["Eq", "Show"])
  , empty
  , text ("put" ++ tname) <+> colon <> colon <+> text "Putter" <+> text tname
  , text ("put" ++ tname) <+> parens (text tname <+> text "a") <+> equals <+> text "put a"
  , empty
  , text ("get" ++ tname) <+> colon <> colon <+> text "Get" <+> text tname
  , text ("get" ++ tname) <+> equals <+> text "do"
  , indent 2 $ stack $
      [ text "a" <+> text "<- get"
      , text "return" <+> parens (text tname <+> text "a") ]
  , empty
  , serializeInstance tname
  ]

typeDecl tname (EnumType (EnumT s es)) = stack
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
  , empty
  , text ("put" ++ tname) <+> colon <> colon <+> text "Putter" <+> text tname
  , stack
      [ text ("put" ++ tname) <+> text (userTypeModuleName i) <+> equals <+> 
          text "put" <> text (cerealSize s) <+> ppr e
      | (i,e) <- es ]
  , empty
  , text ("get" ++ tname) <+> colon <> colon <+> text "Get" <+> text tname
  , text ("get" ++ tname) <+> equals <+> text "do"
  , indent 2 $ stack
      [ text "a" <+> text "<- get" <> text (cerealSize s)
      , text "case a of"
      , indent 2 $ stack $
          [ ppr e <+> text "-> return" <+> text (userTypeModuleName i)
          | (i,e) <- es
          ] ++ [text "_ -> fail \"invalid value in get"  <> text tname <> text"\"" ]
      ]
  , empty
  , serializeInstance tname
  ]
  where deriv = typeDeriving ["Eq", "Show", "Ord"]

typeDecl tn _ = error ("typeDecl: cannot create decl for built in type " ++ tn)

cerealSize :: Bits -> String
cerealSize Bits8  = "Word8"
cerealSize Bits16 = "Word16be"
cerealSize Bits32 = "Word32be"
cerealSize Bits64 = "Word64be"


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

isUserDefined :: TypeRepr -> Bool
isUserDefined tr = case importType tr of
  UserType _ -> True
  _ -> False


importDecl :: (String -> Doc) -> ImportType -> Doc
importDecl _ (LibraryType p) =
  text "import" <+> text p
importDecl mkpath (UserType t) =
  text "import" <+> mkpath (userTypeModuleName t)
importDecl _ NoImport = empty


encloseStack :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseStack l r p ds = case ds of
  [] -> empty -- l </> r
  [d] -> l <+> d </> r
  _ -> align (l <+> (folddoc (\a b -> a </> p <+> b) ds) </> r)

