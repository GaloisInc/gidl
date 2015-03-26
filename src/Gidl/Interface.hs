
module Gidl.Interface
  ( module Gidl.Interface.AST
  , InterfaceDescr
  , InterfaceRepr(..)
  , interfaceDescrToRepr
  , lookupInterface
  , insertInterface
  , interfaceParents
  , interfaceTypes
  , interfaceMethods
  ) where

import Data.List (nub)
import Data.Maybe (fromJust)
import Gidl.Interface.AST
import Gidl.Types

type InterfaceDescr = Interface InterfaceName TypeName

lookupInterface :: InterfaceName -> InterfaceEnv -> Maybe InterfaceDescr
lookupInterface iname (InterfaceEnv ie) = lookup iname ie

insertInterface :: InterfaceName -> InterfaceDescr -> InterfaceEnv -> InterfaceEnv
insertInterface iname i e@(InterfaceEnv ie) = case lookupInterface iname e of
  Nothing -> InterfaceEnv ((iname,i):ie)
  Just _ -> error ("insertInterface invariant broken: interface " ++ iname ++ "already exists")

interfaceParents :: Interface i t -> [i]
interfaceParents (Interface parents _) = parents

interfaceTypes :: InterfaceName -> InterfaceEnv -> TypeEnv -> [TypeName]
interfaceTypes iname ie te = nub (concatMap aux ms)
  where
  (Interface _ ms) = fromJust (lookupInterface iname ie)
  aux = typeLeaves
      . fromJust
      . (\tn -> lookupTypeName tn te)
      . methodTN
      . snd
  methodTN :: Method TypeName -> TypeName
  methodTN (AttrMethod _ tn) = tn
  methodTN (StreamMethod _ tn) = tn


data InterfaceRepr = InterfaceRepr InterfaceName (Interface InterfaceRepr TypeRepr)
                     deriving (Eq, Show)

interfaceDescrToRepr :: InterfaceName -> InterfaceEnv -> TypeEnv -> InterfaceRepr
interfaceDescrToRepr iname ie te = InterfaceRepr iname ir
  where
  ir = case fromJust $ lookupInterface iname ie of
      Interface is ms -> Interface (map recur is)
                           [ (mn, methodDescrToRepr te md) | (mn, md) <- ms ]
  recur i = interfaceDescrToRepr i ie te


methodDescrToRepr :: TypeEnv -> Method TypeName -> Method TypeRepr
methodDescrToRepr te (AttrMethod p tn) = AttrMethod p (typeDescrToRepr tn te)
methodDescrToRepr te (StreamMethod r tn) = StreamMethod r (typeDescrToRepr tn te)

interfaceMethods :: InterfaceRepr -> [(MethodName, Method TypeRepr)]
interfaceMethods ir = ms ++ concatMap interfaceMethods ps
  where
  (InterfaceRepr _ (Interface ps ms)) =  ir

