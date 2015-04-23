
module Gidl.Interface
  ( module Gidl.Interface.AST
  , lookupInterface
  , insertInterface
  , interfaceParents
  , interfaceMethods
  ) where

import Gidl.Interface.AST

lookupInterface :: InterfaceName -> InterfaceEnv -> Maybe Interface
lookupInterface iname (InterfaceEnv ie) = lookup iname ie

insertInterface :: Interface -> InterfaceEnv -> InterfaceEnv
insertInterface i e@(InterfaceEnv ie) = case lookupInterface iname e of
  Nothing -> InterfaceEnv ((iname,i):ie)
  Just _ -> error ("insertInterface invariant broken: interface " ++ iname ++ "already exists")
  where (Interface iname _ _) = i

interfaceParents :: Interface -> [Interface]
interfaceParents (Interface _ parents _) = parents

interfaceMethods :: Interface -> [(MethodName, Method)]
interfaceMethods (Interface _ ps ms) = ms ++ concatMap interfaceMethods ps

