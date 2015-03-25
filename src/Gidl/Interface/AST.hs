
module Gidl.Interface.AST where

import Gidl.Types.AST

data InterfaceEnv
  = InterfaceEnv [(InterfaceName, Interface InterfaceName TypeName)]
  deriving (Eq, Show)

emptyInterfaceEnv :: InterfaceEnv
emptyInterfaceEnv = InterfaceEnv []

type InterfaceName = String
type MethodName = String

data Interface i t
  = Interface [i] [(MethodName, Method t)]
  deriving (Eq, Show)

data Method t
  = AttrMethod Perm t
  | StreamMethod Integer t
  deriving (Eq, Show)

data Perm
  = Read
  | Write
  | ReadWrite
  deriving (Eq, Show)


