
module Gidl.Types.AST where

type Identifier = String
type TypeName = String
data TypeEnv
  = TypeEnv [(TypeName, Type)]
  deriving (Eq, Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv []

data Type
  = StructType String [(Identifier, Type)]
  | PrimType PrimType
  deriving (Eq, Show)

data PrimType
  = Newtype  String PrimType
  | EnumType String Bits [(Identifier, Integer)]
  | AtomType Atom
  deriving (Eq, Show)

data Atom
  = AtomInt Bits
  | AtomWord Bits
  | AtomFloat
  | AtomDouble
  deriving (Eq, Show)

data Bits
  = Bits8
  | Bits16
  | Bits32
  | Bits64
  deriving (Eq, Show)

