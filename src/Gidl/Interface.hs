
module Gidl.Interface
  ( module Gidl.Interface.AST
  , lookupInterface
  , insertInterface
  , interfaceParents
  , interfaceTypes
  , interfaceMethods
  ) where

import Data.List (nub)
import Gidl.Interface.AST
import Gidl.Types

lookupInterface :: InterfaceName -> InterfaceEnv -> Maybe Interface
lookupInterface iname (InterfaceEnv ie) = lookup iname ie

insertInterface :: InterfaceName -> Interface -> InterfaceEnv -> InterfaceEnv
insertInterface iname i e@(InterfaceEnv ie) = case lookupInterface iname e of
  Nothing -> InterfaceEnv ((iname,i):ie)
  Just _ -> error ("insertInterface invariant broken: interface " ++ iname ++ "already exists")

interfaceParents :: Interface -> [Interface]
interfaceParents (Interface parents _) = parents

interfaceTypes :: Interface -> [Type]
interfaceTypes ir = nub (map (methodT . snd) ms)
  where
  ms = interfaceMethods ir
  methodT :: Method -> Type
  methodT (AttrMethod _ ty) = ty
  methodT (StreamMethod _ ty) = ty

interfaceMethods :: Interface -> [(MethodName, Method)]
interfaceMethods (Interface ps ms) = ms ++ concatMap interfaceMethods ps

