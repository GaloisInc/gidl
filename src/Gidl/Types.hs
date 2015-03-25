module Gidl.Types
  ( module Gidl.Types.AST
  , module Gidl.Types.Base
  , TypeDescr
  , TypeRepr(..)
  , lookupTypeName
  , insertType
  , typeLeaves
  , typeDescrToRepr
  , sizeOf
  , voidTypeRepr
  ) where

import Data.List (nub)
import Data.Maybe (fromJust)
import Gidl.Types.AST
import Gidl.Types.Base

lookupTypeName :: TypeName -> TypeEnv -> Maybe TypeDescr
lookupTypeName tn te =
  case aux te of
    Just a -> Just a
    Nothing -> case aux baseTypeEnv of
      Just a -> Just a
      Nothing -> Nothing
  where
  aux (TypeEnv e) = lookup tn e

insertType :: TypeName -> TypeDescr -> TypeEnv -> TypeEnv
insertType tn t e@(TypeEnv te) = case lookupTypeName tn e of
  Nothing -> TypeEnv ((tn,t):te)
  Just _ -> error ("insertType invariant broken: type " ++ tn ++ " already exists")

typeLeaves :: (Eq t) => Type t -> [t]
typeLeaves (StructType (Struct s)) = nub (map snd s)
typeLeaves (NewtypeType (Newtype tn)) = [tn]
typeLeaves (EnumType _) = []
typeLeaves (AtomType _) = []
typeLeaves VoidType = []


type TypeDescr = Type TypeName
data TypeRepr = TypeRepr TypeName (Type TypeRepr)
                deriving (Eq, Show)

voidTypeRepr :: TypeRepr
voidTypeRepr = TypeRepr "void" VoidType

-- invariant: TypeName exists in a well-formed TypeEnv
typeDescrToRepr :: TypeName -> TypeEnv -> TypeRepr
typeDescrToRepr tn te = TypeRepr tn tr
  where
  tr = case fromJust $ lookupTypeName tn te of
        EnumType e -> EnumType e
        AtomType a -> AtomType a
        NewtypeType (Newtype ntn) ->
          NewtypeType (Newtype (typeDescrToRepr ntn te))
        StructType (Struct s) ->
          StructType (Struct [(i, typeDescrToRepr stn te) | (i, stn) <- s])
        VoidType -> VoidType


sizeOf :: TypeRepr -> Integer
sizeOf (TypeRepr _ (StructType (Struct s))) = sum [ sizeOf tr | (_, tr) <- s ]
sizeOf (TypeRepr _ (NewtypeType (Newtype tr))) = sizeOf tr
sizeOf (TypeRepr _ (EnumType (EnumT bs _))) = bitsSize bs
sizeOf (TypeRepr _ (AtomType (AtomInt bs))) = bitsSize bs
sizeOf (TypeRepr _ (AtomType (AtomWord bs))) = bitsSize bs
sizeOf (TypeRepr _ (AtomType AtomFloat)) = 4
sizeOf (TypeRepr _ (AtomType AtomDouble)) = 8
sizeOf (TypeRepr _ VoidType) = 0

bitsSize :: Bits -> Integer
bitsSize Bits8  = 1
bitsSize Bits16 = 2
bitsSize Bits32 = 4
bitsSize Bits64 = 8
