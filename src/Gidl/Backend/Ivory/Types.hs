
module Gidl.Backend.Ivory.Types where

import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper, toLower)
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
    [ text "{-# LANGUAGE DataKinds #-}"
    , text "{-# LANGUAGE TypeOperators #-}"
    , text "{-# LANGUAGE QuasiQuotes #-}"
    , text "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , text "{-# LANGUAGE FlexibleInstances #-}"
    , text "{-# OPTIONS_GHC -fno-warn-orphans #-}"
    , empty
    , text "module"
      <+> tm (typeModuleName tr)
      <+> text "where"
    , empty
    , stack (imports ++
              [ text "import Ivory.Language"
              , text "import Ivory.Serialize"
              ])
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

typeIvoryType :: TypeRepr -> String
typeIvoryType (TypeRepr tn (StructType _)) = "Struct \"" ++ userTypeStructName tn ++ "\""
typeIvoryType (TypeRepr tn (NewtypeType _)) = userTypeModuleName tn
typeIvoryType (TypeRepr tn (EnumType _)) = userTypeModuleName tn
typeIvoryType (TypeRepr _ (AtomType a)) = case a of
  AtomInt Bits8  -> "Sint8"
  AtomInt Bits16 -> "Sint16"
  AtomInt Bits32 -> "Sint32"
  AtomInt Bits64 -> "Sint64"
  AtomWord Bits8  -> "Uint8"
  AtomWord Bits16 -> "Uint16"
  AtomWord Bits32 -> "Uint32"
  AtomWord Bits64 -> "Uint64"
  AtomFloat -> "IFloat"
  AtomDouble -> "IDouble"
typeIvoryType (TypeRepr _ VoidType) = "()" -- XXX this is gonna cause trouble buddy

typeModuleName :: TypeRepr -> String
typeModuleName (TypeRepr tn (StructType _)) = userTypeModuleName tn
typeModuleName (TypeRepr tn (NewtypeType _)) = userTypeModuleName tn
typeModuleName (TypeRepr tn (EnumType _)) = userTypeModuleName tn
typeModuleName (TypeRepr _ (AtomType _)) = error "do not take typeModuleName of an AtomType"
typeModuleName (TypeRepr _ VoidType) = error "do not take typeModuleName of a VoidType"

userTypeModuleName :: String -> String
userTypeModuleName = first_cap . userEnumValueName
  where
  first_cap (s:ss) = (toUpper s) : ss
  first_cap []     = []

userEnumValueName :: String -> String
userEnumValueName = first_lower . u_to_camel
  where
  first_lower (s:ss) = (toLower s) : ss
  first_lower []     = []
  u_to_camel ('_':'t':[]) = []
  u_to_camel ('_':[]) = []
  u_to_camel ('_':a:as) = (toUpper a) : u_to_camel as
  u_to_camel (a:as) = a : u_to_camel as
  u_to_camel [] = []

userTypeStructName :: String -> String
userTypeStructName = first_lower . drop_t_suffix
  where
  first_lower (s:ss) = (toLower s) : ss
  first_lower []     = []
  drop_t_suffix []     = []
  drop_t_suffix ('_':'t':[]) = []
  drop_t_suffix (a:as) = a : drop_t_suffix as

typeDecl :: TypeName -> Type TypeRepr -> Doc
typeDecl tname td@(StructType (Struct ss)) = stack
  [ text "[ivory|"
  , text "struct" <+> structname
  , indent 2 $ encloseStack lbrace rbrace semi
      [ text i <+> colon <> colon <+> text "Stored" <+> text (typeIvoryType t) -- XXX AREA TYPE
      | (i,t) <- ss ]
  , text "|]"
  , empty
  , text (userEnumValueName tname) <> text "TypesModule :: Module"
  , text (userEnumValueName tname) <> text "TypesModule" <+> equals
    <+> text "package" <+> dquotes (structname <> text "_types") <+> text "$ do"
  , indent 2 $ stack
      [ text "defStruct"
        <+> parens (text "Proxy :: Proxy" <+> dquotes structname)
      , text "depend serializeModule"
      , stack is
      ]

  ]
  where
  is = map userIModDependency $ nub $ typeLeaves td
  structname = text (userTypeStructName tname)

typeDecl tname (NewtypeType (Newtype n)) =
  case baseType n of
    TypeRepr _ (StructType _) -> stack
      [ text "type" <+> text tname <+> equals <+> text (typeIvoryType (baseType n)) ]
    _ -> stack
      [ text "newtype" <+> text tname <+> equals
      , indent 2 $ text tname <+> align
            (lbrace <+> text ("un" ++ tname) <+> text "::" <+>
             text (typeIvoryType n) </>
             rbrace <+> typeDeriving (words "IvoryType IvoryVar IvoryExpr IvoryEq IvoryStore IvoryInit IvoryZeroVal"))
      ]

typeDecl tname (EnumType (EnumT s es)) = stack
  [ text "newtype" <+> text tname <+> equals
  , indent 2 $ text tname <+> align
        (lbrace <+> text ("un" ++ tname) <+> text "::" <+>
         text bt </>
         rbrace <+> typeDeriving (words "IvoryType IvoryVar IvoryExpr IvoryEq IvoryStore IvoryInit IvoryZeroVal"))
  , empty
  , stack
      [ stack
        [ empty
        , text (userEnumValueName i) <+> colon <> colon <+> text tname
        , text (userEnumValueName i) <+> equals <+> text tname <+> ppr e
        ]
      | (i,e) <- es ]
  ]
  where
  bt = case s of
    Bits8 -> "Uint8"
    Bits16 -> "Uint16"
    Bits32 -> "Uint32"
    Bits64 -> "Uint64"

typeDecl tn _ = error ("typeDecl: cannot create decl for built in type " ++ tn)

typeDeriving :: [String] -> Doc
typeDeriving cs = text "deriving" <+> parens (commasep (map text cs))

data ImportType = LibraryType String
                | UserType String
                | NoImport
                deriving (Eq, Show)

importType :: TypeRepr -> ImportType
importType (TypeRepr _ (AtomType _)) = NoImport
importType (TypeRepr _ VoidType) = NoImport
importType (TypeRepr n _) = UserType n

isUserDefined :: TypeRepr -> Bool
isUserDefined tr = case importType tr of
  UserType _ -> True
  _ -> False


userIModDependency :: TypeRepr -> Doc
userIModDependency tr = case baseType tr of
  (TypeRepr sn (StructType _)) ->
    text "depend" <+> text (userTypeStructName sn) <> text "TypesModule"
  _ -> empty

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

