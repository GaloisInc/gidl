
module Gidl.Types.AST where

type Identifier = String
type TypeName = String
data TypeEnv
  = TypeEnv [(TypeName, Type)]
  deriving (Eq, Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv []

data Type
  = StructType Struct
  | NewtypeType Newtype
  | EnumType EnumT
  | AtomType Atom
  deriving (Eq, Show)

data Atom
  = AtomInt Bits
  | AtomWord Bits
  | AtomFloat
  | AtomDouble
--  | AtomString Int
  deriving (Eq, Show)

data Bits
  = Bits8
  | Bits16
  | Bits32
  | Bits64
  deriving (Eq, Show)

data Struct
  = Struct [(Identifier, TypeName)]
  deriving (Eq, Show)

data Newtype
  = Newtype TypeName
  deriving (Eq, Show)

data EnumT
  = EnumT Bits [(Identifier, Integer)]
  deriving (Eq, Show)

