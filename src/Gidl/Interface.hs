
module Gidl.Interface
  ( module Gidl.Interface.AST
  , lookupInterface
  , insertInterface
  , interfaceTypes
  , interfaceParents
  ) where

import Data.List (nub)
import Data.Maybe (fromJust)
import Gidl.Interface.AST
import Gidl.Types

lookupInterface :: InterfaceName -> InterfaceEnv -> Maybe Interface
lookupInterface iname (InterfaceEnv ie) = lookup iname ie

insertInterface :: InterfaceName -> Interface -> InterfaceEnv -> InterfaceEnv
insertInterface iname i e@(InterfaceEnv ie) = case lookupInterface iname e of
  Nothing -> InterfaceEnv ((iname,i):ie)
  Just _ -> error ("insertInterface invariant broken: interface " ++ iname ++ "already exists")

interfaceParents :: Interface -> [InterfaceName]
interfaceParents (Interface parents _) = parents

interfaceTypes :: InterfaceName -> InterfaceEnv -> TypeEnv -> [TypeName]
interfaceTypes iname ie te = nub $
  concatMap aux ms 
  where
  (Interface _ ms) = fromJust (lookupInterface iname ie)
  aux = typeLeaves
      . fromJust
      . (\tn -> lookupTypeName tn te)
      . methodTN
      . snd
  methodTN :: Method -> TypeName
  methodTN (AttrMethod _ tn) = tn
  methodTN (StreamMethod _ tn) = tn


