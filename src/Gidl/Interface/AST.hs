
module Gidl.Interface.AST where

import Gidl.Types.AST

data InterfaceEnv
  = InterfaceEnv [(InterfaceName, Interface)]
  deriving (Eq, Show)

emptyInterfaceEnv :: InterfaceEnv
emptyInterfaceEnv = InterfaceEnv []

type InterfaceName = String
type MethodName = String

data Interface
  = Interface String [Interface] [(MethodName, Method)]
  deriving (Eq, Show)

data Method
  = AttrMethod Perm Type
  | StreamMethod Integer Type
  deriving (Eq, Show)

data Perm
  = Read
  | Write
  | ReadWrite
  deriving (Eq, Show)

readable :: Perm -> Bool
readable Read = True
readable ReadWrite = True
readable _ = False

writable :: Perm -> Bool
writable Write = True
writable ReadWrite = True
writable _ = False
