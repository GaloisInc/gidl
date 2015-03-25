
module Gidl.Types.AST where

type Identifier = String
type TypeName = String
data TypeEnv
  = TypeEnv [(TypeName, Type TypeName)]
  deriving (Eq, Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv []


data Type t
  = StructType (Struct t)
  | NewtypeType (Newtype t)
  | EnumType EnumT
  | AtomType Atom
  | VoidType
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

data Struct t
  = Struct [(Identifier, t)]
  deriving (Eq, Show)

data Newtype t
  = Newtype t
  deriving (Eq, Show)

data EnumT
  = EnumT Bits [(Identifier, Integer)]
  deriving (Eq, Show)

