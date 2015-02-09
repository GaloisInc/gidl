
module Gidl.Types.AST where

import Data.Monoid

type Identifier = String
type TypeName = String
data TypeEnv
  = TypeEnv [(TypeName, Type)]
  deriving (Eq, Show)

instance Monoid TypeEnv where
  (TypeEnv a) `mappend` (TypeEnv b) = TypeEnv (a ++ b)
  mempty = TypeEnv []

data Type
  = StructType Struct
  | NewtypeType Newtype
  | EnumType EnumT
  | AtomType Atom
--  | MaybeType Type
--  | EitherType Type Type
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

