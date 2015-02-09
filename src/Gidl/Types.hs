module Gidl.Types
  ( module Gidl.Types.AST
  , module Gidl.Types.Base
  , lookupTypeName
  , insertType
  ) where
import Gidl.Types.AST
import Gidl.Types.Base

lookupTypeName :: TypeName -> TypeEnv -> Maybe Type
lookupTypeName tn te =
  case aux te of
    Just a -> Just a
    Nothing -> case aux baseTypeEnv of
      Just a -> Just a
      Nothing -> Nothing
  where
  aux (TypeEnv e) = lookup tn e

insertType :: TypeName -> Type -> TypeEnv -> TypeEnv
insertType tn t e@(TypeEnv te) = case lookupTypeName tn e of
  Nothing -> TypeEnv ((tn,t):te)
  Just _ -> error ("insertType invariant broken: type " ++ tn ++ " already exists")

