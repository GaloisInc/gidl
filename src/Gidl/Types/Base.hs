
module Gidl.Types.Base
  ( uint8_t
  , uint16_t
  , uint32_t
  , uint64_t
  , sint8_t
  , sint16_t
  , sint32_t
  , sint64_t
  , bool_t
  , float_t
  , double_t
  , sequence_num_t
  , baseTypeEnv
  ) where

import Gidl.Types.AST

uint8_t  :: Type
uint8_t  = PrimType (AtomType (AtomWord Bits8))
uint16_t :: Type
uint16_t = PrimType (AtomType (AtomWord Bits16))
uint32_t :: Type
uint32_t = PrimType (AtomType (AtomWord Bits32))
uint64_t :: Type
uint64_t = PrimType (AtomType (AtomWord Bits64))

sint8_t  :: Type
sint8_t  = PrimType (AtomType (AtomInt  Bits8))
sint16_t :: Type
sint16_t = PrimType (AtomType (AtomInt  Bits16))
sint32_t :: Type
sint32_t = PrimType (AtomType (AtomInt  Bits32))
sint64_t :: Type
sint64_t = PrimType (AtomType (AtomInt  Bits64))

bool_t :: Type
bool_t = PrimType (EnumType "bool_t" Bits8 [("false", 0), ("true", 1)])

float_t :: Type
float_t = PrimType (AtomType AtomFloat)

double_t :: Type
double_t = PrimType (AtomType AtomDouble)

sequence_num_t :: Type
sequence_num_t = PrimType (Newtype "sequence_num_t" (AtomType (AtomWord Bits32)))

baseTypeEnv :: TypeEnv
baseTypeEnv = TypeEnv
  [ ( "uint8_t" , uint8_t)
  , ( "uint16_t", uint16_t)
  , ( "uint32_t", uint32_t)
  , ( "uint64_t", uint64_t)
  , ( "sint8_t" , sint8_t)
  , ( "sint16_t", sint16_t)
  , ( "sint32_t", sint32_t)
  , ( "sint64_t", sint64_t)
  , ( "bool_t"  , bool_t)
  , ( "float_t" , float_t)
  , ( "double_t", double_t)
  , ( "sequence_num_t", sequence_num_t)
  ]

