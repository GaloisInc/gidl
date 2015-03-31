
module Gidl.Backend.Haskell.Types where

import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper)
import Gidl.Types
import Ivory.Artifact
import Text.PrettyPrint.Mainland

-- invariant: only make a typeModule from a StructType, NewtypeType, or EnumType
-- i.e. when isUserDefined is true.
typeModule :: [String] -> Type -> Artifact
typeModule modulepath t =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((typeModuleName t) ++ ".hs") $
  prettyLazyText 80 $
  stack
    [ text "{-# LANGUAGE RecordWildCards #-}"
    , text "{-# LANGUAGE DeriveDataTypeable #-}"
    , empty
    , text "module"
      <+> tm (typeModuleName t)
      <+> text "where"
    , empty
    , stack (imports ++
              [ text "import Data.Serialize"
              , text "import Data.Typeable"
              , text "import Data.Data"
              , text "import qualified Test.QuickCheck as Q"
              ])
    , empty
    , typeDecl typename t
    ]
  where
  imports = map (importDecl tm)
          $ nub
          $ map (importType . PrimType)
          $ typeLeaves t
  typename = typeModuleName t
  tm mname = mconcat $ punctuate dot
                     $ map text (modulepath ++ [mname])

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
typeHaskellType (PrimType VoidType) = "()"

typeModuleName :: Type -> String
typeModuleName (StructType tn _) = userTypeModuleName tn
typeModuleName (PrimType (Newtype tn _)) = userTypeModuleName tn
typeModuleName (PrimType (EnumType tn _ _)) = userTypeModuleName tn
typeModuleName (PrimType (AtomType _)) = error "do not take typeModuleName of an AtomType"
typeModuleName (PrimType VoidType) = error "do not take typeModuleName of a VoidType"

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

typeDecl :: String -> Type -> Doc
typeDecl tname (StructType _ ss) = stack
  [ text "data" <+> text tname <+> equals
  , indent 2 $ text tname
  , indent 4 $ encloseStack lbrace (rbrace <+> deriv) comma
      [ text i <+> colon <> colon <+> text (typeHaskellType (PrimType t))
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
  where deriv = typeDeriving ["Eq", "Show", "Data", "Typeable"]

typeDecl tname (PrimType (Newtype _ n)) = stack
  [ text "newtype" <+> text tname <+> equals
  , indent 2 $ text tname <+> align
        (lbrace <+> text ("un" ++ tname) <+> text "::" <+>
         text (typeHaskellType (PrimType n)) </>
         rbrace <+> typeDeriving ["Eq", "Show", "Data", "Typeable"])
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
  , text ("arbitrary" ++ tname) <+> colon <> colon <+> text "Q.Gen" <+> text tname
  , text ("arbitrary" ++ tname) <+> equals <+> text "do"
  , indent 2 $ stack $
      [ text "a" <+> text "<- Q.arbitrary"
      , text "return" <+> parens (text tname <+> text "a") ]
  , empty
  , arbitraryInstance tname
  ]

typeDecl tname (PrimType (EnumType _ s es)) = stack
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
  , empty
  , text ("arbitrary" ++ tname) <+> colon <> colon <+> text "Q.Gen" <+> text tname
  , text ("arbitrary" ++ tname) <+> equals
  , indent 2 $ text "Q.elements" <+> encloseStack lbracket rbracket comma
                                      [ text (userTypeModuleName i) | (i,_e) <- es ]
  , empty
  , arbitraryInstance tname
  ]
  where deriv = typeDeriving ["Eq", "Show", "Ord", "Data", "Typeable"]

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

importType :: Type -> ImportType
importType (StructType n _) = UserType n
importType (PrimType (EnumType n _ _)) = UserType n
importType (PrimType (Newtype n _)) = UserType n
importType (PrimType (AtomType a)) =
  case a of
    AtomWord _ -> LibraryType "Data.Word"
    AtomInt _ -> LibraryType "Data.Int"
    _ -> NoImport
importType (PrimType VoidType) = NoImport

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


encloseStack :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseStack l r p ds = case ds of
  [] -> empty -- l </> r
  [d] -> l <+> d </> r
  _ -> align (l <+> (folddoc (\a b -> a </> p <+> b) ds) </> r)

