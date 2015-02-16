
module Gidl.Interface
  ( module Gidl.Interface.AST
  , lookupInterface
  , insertInterface
  ) where

import Gidl.Interface.AST

lookupInterface :: InterfaceName -> InterfaceEnv -> Maybe Interface
lookupInterface iname (InterfaceEnv ie) = lookup iname ie

insertInterface :: InterfaceName -> Interface -> InterfaceEnv -> InterfaceEnv
insertInterface iname i e@(InterfaceEnv ie) = case lookupInterface iname e of
  Nothing -> InterfaceEnv ((iname,i):ie)
  Just _ -> error ("insertInterface invariant broken: interface " ++ iname ++ "already exists")

