
module Gidl.Backend.Ivory.Types where

import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper, toLower)
import Gidl.Types
import Ivory.Artifact
import Text.PrettyPrint.Mainland

-- invariant: only make a typeModule from a StructType, Newtype, or EnumType
-- i.e. when isUserDefined is true.
typeModule :: [String] -> Type -> Artifact
typeModule modulepath t =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((typeModuleName t) ++ ".hs") $
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
      <+> tm (typeModuleName t)
      <+> text "where"
    , empty
    , stack (imports ++
              [ text "import Ivory.Language"
              , text "import Ivory.Serialize"
              ])
    , empty
    , typeDecl t
    ]
  where
  imports = map (importDecl tm)
          $ nub
          $ map (importType . PrimType)
          $ typeLeaves t
  tm mname = mconcat $ punctuate dot
                     $ map text (modulepath ++ [mname])

typeImportedIvoryType :: Type -> String
typeImportedIvoryType t@(PrimType (Newtype tn _)) =
  userTypeModuleName tn ++ "." ++ typeIvoryType t
typeImportedIvoryType t@(PrimType (EnumType tn _ _)) =
  userTypeModuleName tn ++ "." ++ typeIvoryType t
typeImportedIvoryType t = typeIvoryType t

typeIvoryType :: Type -> String
typeIvoryType (StructType tn _) = "Struct \"" ++ userTypeStructName tn ++ "\""
typeIvoryType (PrimType (Newtype tn _)) = userTypeModuleName tn
typeIvoryType (PrimType (EnumType tn _ _)) = userTypeModuleName tn
typeIvoryType (PrimType (AtomType a)) = case a of
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
typeIvoryType (PrimType VoidType) = "()"

typeModuleName :: Type -> String
typeModuleName (StructType tn _) = userTypeModuleName tn
typeModuleName (PrimType (Newtype tn _)) = userTypeModuleName tn
typeModuleName (PrimType (EnumType tn _ _)) = userTypeModuleName tn
typeModuleName (PrimType (AtomType _)) = error "do not take typeModuleName of an AtomType"
typeModuleName (PrimType VoidType) = error "do not take typeModuleName of a VoidType"

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

typeDecl :: Type -> Doc
typeDecl (StructType tname ss) = stack
  [ text "[ivory|"
  , text "struct" <+> structname
  , indent 2 $ encloseStack lbrace rbrace semi
      [ text i <+> colon <> colon
       <+> text "Stored" <+> text (typeImportedIvoryType (PrimType t))
      | (i,t) <- ss ]
  , text "|]"
  , empty
  , packRep <+> colon <> colon <+> text "WrappedPackRep" <+> storedType
  , packRep <+> equals <+> text "wrapPackRep" <+> dquotes structname <+> text "$"
  , indent 2 $ text "packStruct" <+> encloseStack lbracket rbracket comma
                [ text "packLabel" <+> text i
                | (i,_) <- ss]
  , empty
  , text "instance Packable" <+> storedType <+> text "where"
  , indent 2 $ text "packRep" <+> equals <+> text "wrappedPackRep" <+> packRep
  , empty
  , text (userEnumValueName tname) <> text "TypesModule :: Module"
  , text (userEnumValueName tname) <> text "TypesModule" <+> equals
    <+> text "package" <+> dquotes (structname <> text "_types") <+> text "$ do"
  , indent 2 $ stack
      [ text "defStruct"
        <+> parens (text "Proxy :: Proxy" <+> dquotes structname)
      , text "depend serializeModule"
      , text "wrappedPackMod" <+> packRep
      ]

  ]
  where
  storedType = parens (text "Struct" <+> dquotes structname)
  structname = text (userTypeStructName tname)
  packRep = text "pack" <> text (userTypeModuleName tname)

typeDecl (PrimType (Newtype tname n)) = stack
  [ text "newtype" <+> text typename <+> equals
  , indent 2 $ text typename <+> align
      (lbrace <+> text ("un" ++ typename ) <+> text "::"
       <+> text (typeImportedIvoryType (PrimType n))
       </> rbrace <+> typeDeriving (words ("IvoryType IvoryVar IvoryExpr " ++
                       "IvoryEq IvoryStore IvoryInit IvoryZeroVal Num")))
  , empty
  , packRep <+> colon <> colon <+> text "WrappedPackRep" <+> storedType
  , packRep <+> equals <+> text "wrapPackRep" <+> dquotes (text typename) <+> text "$"
  , indent 2 $ text "repackV" <+> text typename <+> text ("un" ++ typename) <+> text "packRep"
  , empty
  , text "instance Packable" <+> storedType <+> text "where"
  , indent 2 $ text "packRep" <+> equals <+> text "wrappedPackRep" <+> packRep
  , empty
  , text (userEnumValueName tname) <> text "TypesModule :: Module"
  , text (userEnumValueName tname) <> text "TypesModule" <+> equals
    <+> text "package"
    <+> dquotes (text (userTypeStructName tname) <> text "_types")
    <+> text "$ do"
  , indent 2 $ stack
      [ text "depend serializeModule"
      , text "wrappedPackMod" <+> packRep
      ]
  ]
  where
  typename = userTypeModuleName tname
  storedType = parens (text "Stored" <+> text typename)
  packRep = text "pack" <> text typename

typeDecl (PrimType (EnumType tname s es)) = stack
  [ text "newtype" <+> text typename <+> equals
  , indent 2 $ text typename <+> align
        (lbrace <+> text ("un" ++ typename) <+> text "::" <+>
         text bt </>
         rbrace <+> typeDeriving (words ("IvoryType IvoryVar IvoryExpr IvoryEq "
                                   ++ "IvoryStore IvoryInit IvoryZeroVal")))
  , empty
  , stack
      [ stack
        [ empty
        , text (userEnumValueName i) <+> colon <> colon <+> text typename
        , text (userEnumValueName i) <+> equals <+> text typename <+> ppr e
        ]
      | (i,e) <- es ]
  , empty
  , packRep <+> colon <> colon <+> text "WrappedPackRep" <+> storedType
  , packRep <+> equals <+> text "wrapPackRep" <+> dquotes (text typename) <+> text "$"
  , indent 2 $ text "repackV" <+> text typename  <+> text ("un" ++ typename) <+> text "packRep"
  , empty
  , text "instance Packable" <+> storedType <+> text "where"
  , indent 2 $ text "packRep" <+> equals <+> text "wrappedPackRep" <+> packRep
  , empty
  , text (userEnumValueName tname) <> text "TypesModule :: Module"
  , text (userEnumValueName tname) <> text "TypesModule" <+> equals
    <+> text "package"
    <+> dquotes (text (userTypeStructName tname) <> text "_types")
    <+> text "$ do"
  , indent 2 $ stack
      [ text "depend serializeModule"
      , text "wrappedPackMod" <+> packRep
      ]
  ]
  where
  typename = userTypeModuleName tname
  packRep = text "pack" <> text typename
  storedType = parens (text "Stored" <+> text typename)
  bt = case s of
    Bits8 -> "Uint8"
    Bits16 -> "Uint16"
    Bits32 -> "Uint32"
    Bits64 -> "Uint64"

typeDecl a = error ("typeDecl: broken invariant, cannot create type for " ++ show a)

typeDeriving :: [String] -> Doc
typeDeriving cs = text "deriving" <+> parens (commasep (map text cs))

data ImportType = LibraryType String
                | UserType String
                | NoImport
                deriving (Eq, Show)

importType :: Type -> ImportType
importType (StructType n _) = UserType n
importType (PrimType (EnumType n _ _)) = UserType n
importType (PrimType (Newtype n _)) = UserType n
importType (PrimType (AtomType _)) = NoImport
importType (PrimType VoidType) = NoImport

isUserDefined :: Type -> Bool
isUserDefined t = case importType t of
  UserType _ -> True
  _ -> False


importDecl :: (String -> Doc) -> ImportType -> Doc
importDecl _ (LibraryType p) =
  text "import" <+> text p
importDecl mkpath (UserType t) =
  text "import qualified" <+> mkpath (userTypeModuleName t)
      <+> text "as" <+> text (userTypeModuleName t)
importDecl _ NoImport = empty


encloseStack :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseStack l r p ds = case ds of
  [] -> empty -- l </> r
  [d] -> l <+> d </> r
  _ -> align (l <+> (folddoc (\a b -> a </> p <+> b) ds) </> r)

