{-# LANGUAGE CPP #-}

module Gidl.Backend.Haskell.Types where

import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper)
import Gidl.Types
import Ivory.Artifact
#if MIN_VERSION_mainland_pretty(0,6,0)
import Text.PrettyPrint.Mainland.Class
#endif
import Text.PrettyPrint.Mainland

-- invariant: only make a typeModule from a StructType, NewtypeType, or EnumType
-- i.e. when isUserDefined is true.
typeModule :: Bool -> [String] -> Type -> Artifact
typeModule useAeson modulepath t =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((typeModuleName t) ++ ".hs") $
  prettyLazyText 1000 $
  stack $
    [ text "{-# LANGUAGE RecordWildCards #-}"
    , text "{-# LANGUAGE DeriveDataTypeable #-}"
    , text "{-# LANGUAGE DeriveGeneric #-}"
    , empty
    , text "module"
      <+> tm (typeModuleName t)
      <+> text "where"
    , empty
    , stack (imports ++
              [ text "import Data.Aeson (ToJSON,FromJSON)" | useAeson ] ++
              [ text "import Data.Serialize"
              , text "import Data.Typeable"
              , text "import Data.Data"
              , text "import GHC.Generics (Generic)"
              , text "import qualified Test.QuickCheck as Q"
              ])
    , empty
    , typeDecl t
    ] ++
    [ toJSONInstance   (typeModuleName t) | useAeson ] ++
    [ fromJSONInstance (typeModuleName t) | useAeson ]
  where
  imports = map (importDecl tm)
          $ nub
          $ map importType
          $ typeLeaves t
  tm mname = mconcat $ punctuate dot
                     $ map text (modulepath ++ [mname])

  --typename = typeModuleName t
typeHaskellType :: Type -> String
typeHaskellType (StructType tn _) = userTypeModuleName tn
typeHaskellType (PrimType (Newtype tn _)) = userTypeModuleName tn
typeHaskellType (PrimType (EnumType tn _ _)) = userTypeModuleName tn
typeHaskellType (PrimType  (AtomType a)) = case a of
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

typeModuleName :: Type -> String
typeModuleName (StructType tn _) = userTypeModuleName tn
typeModuleName (PrimType (Newtype tn _)) = userTypeModuleName tn
typeModuleName (PrimType (EnumType tn _ _)) = userTypeModuleName tn
typeModuleName (PrimType (AtomType _)) = error "do not take typeModuleName of an AtomType"

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

arbitraryInstance :: TypeName -> Doc
arbitraryInstance tname = stack
  [ text "instance Q.Arbitrary" <+> text tname <+> text "where"
  , indent 2 $ stack
      [ text "arbitrary" <+> equals <+> text ("arbitrary" ++ tname)
      ]
  ]

-- | Produce a ToJSON instance.
--
-- NOTE: this instance relies on a GHC that supports Generics.
toJSONInstance :: TypeName -> Doc
toJSONInstance tname = nest 2 (text "instance ToJSON" <+> text tname)

-- | Produce a FromJSON instance.
--
-- NOTE: this instance relies on a GHC that supports Generics.
fromJSONInstance :: TypeName -> Doc
fromJSONInstance tname = nest 2 (text "instance FromJSON" <+> text tname)

typeDecl :: Type -> Doc
typeDecl t@(StructType _ ss) = stack
  [ text "data" <+> text tname <+> equals
  , indent 2 $ text tname
  , indent 4 $ encloseStack lbrace (rbrace <+> deriv) comma
      [ text i <+> colon <> colon <+> text (typeHaskellType st)
      | (i,st) <- ss ]
  , empty
  , text ("put" ++ tname) <+> colon <> colon <+> text "Putter" <+> text tname
  , text ("put" ++ tname) <+> text tname <> text "{..}" <+> equals <+> text "do"
  , indent 2 $ stack
      [ typePutter st <+> text i
      | (i,st) <- ss ]
  , empty
  , text ("get" ++ tname) <+> colon <> colon <+> text "Get" <+> text tname
  , text ("get" ++ tname) <+> equals <+> text "do"
  , indent 2 $ stack $
      [ text i <+> text "<-" <+> typeGetter st
      | (i,st) <- ss ] ++
      [ text "return" <+> text tname <> text "{..}" ]
  , empty
  , serializeInstance tname
  , empty
  , text ("arbitrary" ++ tname) <+> colon <> colon <+> text "Q.Gen" <+> text tname
  , text ("arbitrary" ++ tname) <+> equals <+> text "do"
  , indent 2 $ stack $
      [ text i <+> text "<- Q.arbitrary"
      | (i,_) <- ss ] ++
      [ text "return" <+> text tname <> text "{..}" ]
  , empty
  , arbitraryInstance tname
  ]
  where
  tname = typeModuleName t
  deriv = typeDeriving ["Eq", "Show", "Data", "Typeable", "Generic"]

typeDecl t@(PrimType (Newtype _ n)) = stack
  [ text "newtype" <+> text tname <+> equals
  , indent 2 $ text tname <+> align
        (lbrace <+> text ("un" ++ tname) <+> text "::" <+>
         text (typeHaskellType (PrimType n)) </>
         rbrace <+> typeDeriving ["Eq", "Show", "Data", "Typeable", "Generic"])
  , empty
  , text ("put" ++ tname) <+> colon <> colon <+> text "Putter" <+> text tname
  , text ("put" ++ tname) <+> parens (text tname <+> text "a") <+> equals
      <+> primTypePutter n <+> text "a"
  , empty
  , text ("get" ++ tname) <+> colon <> colon <+> text "Get" <+> text tname
  , text ("get" ++ tname) <+> equals <+> text "do"
  , indent 2 $ stack $
      [ text "a <-" <+> primTypeGetter n
      , text "return" <+> parens (text tname <+> text "a") ]
  , empty
  , serializeInstance tname
  , empty
  , text ("arbitrary" ++ tname) <+> colon <> colon <+> text "Q.Gen" <+> text tname
  , text ("arbitrary" ++ tname) <+> equals <+> text "do"
  , indent 2 $ stack $
      [ text "a" <+> text "<- Q.arbitrary"
      , text "return" <+> parens (text tname <+> text "a") ]
  , empty
  , arbitraryInstance tname
  ]
  where
  tname = typeModuleName t

typeDecl t@(PrimType (EnumType _ s es)) = stack
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
          primTypePutter (sizedPrim s) <+> ppr e
      | (i,e) <- es ]
  , empty
  , text ("get" ++ tname) <+> colon <> colon <+> text "Get" <+> text tname
  , text ("get" ++ tname) <+> equals <+> text "do"
  , indent 2 $ stack
      [ text "a <-" <+> primTypeGetter (sizedPrim s)
      , text "case a of"
      , indent 2 $ stack $
          [ ppr e <+> text "-> return" <+> text (userTypeModuleName i)
          | (i,e) <- es
          ] ++ [text "_ -> fail \"invalid value in get"  <> text tname <> text"\"" ]
      ]
  , empty
  , serializeInstance tname
  , empty
  , text ("arbitrary" ++ tname) <+> colon <> colon <+> text "Q.Gen" <+> text tname
  , text ("arbitrary" ++ tname) <+> equals
  , indent 2 $ text "Q.elements" <+> encloseStack lbracket rbracket comma
                                      [ text (userTypeModuleName i) | (i,_e) <- es ]
  , empty
  , arbitraryInstance tname
  ]
  where
  deriv = typeDeriving ["Eq", "Show", "Ord", "Data", "Typeable", "Generic"]
  tname = typeModuleName t

typeDecl t = error ("typeDecl: cannot create Haskell decl for type " ++ show t)

typePutter :: Type -> Doc
typePutter (PrimType p) = primTypePutter p
typePutter struct = text "put" <> text (typeModuleName struct)

primTypePutter :: PrimType -> Doc
primTypePutter (Newtype tn _) = text "put" <> text (userTypeModuleName tn)
primTypePutter (EnumType "bool_t" _ _) = text "put"
primTypePutter (EnumType tn _ _) = text "put" <> text (userTypeModuleName tn)
primTypePutter (AtomType (AtomInt _)) = text "put"
primTypePutter (AtomType (AtomWord Bits8)) = text "putWord8"
primTypePutter (AtomType (AtomWord Bits16)) = text "putWord16be"
primTypePutter (AtomType (AtomWord Bits32)) = text "putWord32be"
primTypePutter (AtomType (AtomWord Bits64)) = text "putWord64be"
primTypePutter (AtomType AtomFloat) = text "putFloat32be"
primTypePutter (AtomType AtomDouble) = text "putFloat64be"


typeGetter :: Type -> Doc
typeGetter (PrimType p) = primTypeGetter p
typeGetter struct = text "get" <> text (typeModuleName struct)

primTypeGetter :: PrimType -> Doc
primTypeGetter (Newtype tn _) = text "get" <> text (userTypeModuleName tn)
primTypeGetter (EnumType "bool_t" _ _) = text "get"
primTypeGetter (EnumType tn _ _) = text "get" <> text (userTypeModuleName tn)
primTypeGetter (AtomType (AtomInt _)) = text "get"
primTypeGetter (AtomType (AtomWord Bits8)) = text "getWord8"
primTypeGetter (AtomType (AtomWord Bits16)) = text "getWord16be"
primTypeGetter (AtomType (AtomWord Bits32)) = text "getWord32be"
primTypeGetter (AtomType (AtomWord Bits64)) = text "getWord64be"
primTypeGetter (AtomType AtomFloat) = text "getFloat32be"
primTypeGetter (AtomType AtomDouble) = text "getFloat64be"

sizedPrim :: Bits -> PrimType
sizedPrim b = AtomType (AtomWord b)

typeDeriving :: [String] -> Doc
typeDeriving cs = text "deriving" <+> parens (commasep (map text cs))

data ImportType = LibraryType String
                | UserType String
                | NoImport
                deriving (Eq, Show)

importType :: Type -> ImportType
importType (StructType n _) = UserType n
importType (PrimType (EnumType "bool_t" _ _)) = NoImport
importType (PrimType (EnumType n _ _)) = UserType n
importType (PrimType (Newtype n _)) = UserType n
importType (PrimType (AtomType a)) =
  case a of
    AtomWord _ -> LibraryType "Data.Word"
    AtomInt _ -> LibraryType "Data.Int"
    _ -> NoImport

isUserDefined :: Type -> Bool
isUserDefined tr = case importType tr of
  UserType _ -> True
  _ -> False


importDecl :: (String -> Doc) -> ImportType -> Doc
importDecl _ (LibraryType p) =
  text "import" <+> text p
importDecl mkpath (UserType t) =
  text "import" <+> mkpath (userTypeModuleName t)
importDecl _ NoImport = empty

qualifiedImportDecl :: (String -> Doc) -> ImportType -> Doc
qualifiedImportDecl _ (LibraryType p) =
  text "import" <+> text p
qualifiedImportDecl mkpath (UserType t) =
  text "import qualified" <+> mkpath (userTypeModuleName t) <+> text "as"
   <+> text (userTypeModuleName t)
qualifiedImportDecl _ NoImport = empty


encloseStack :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseStack l r p ds = case ds of
  [] -> empty -- l </> r
  [d] -> l <+> d </> r
  _ -> align (l <+> (folddoc (\a b -> a </> p <+> b) ds) </> r)

