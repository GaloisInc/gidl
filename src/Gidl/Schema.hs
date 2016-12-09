
module Gidl.Schema where

import Data.Word
import Data.List (nub)
import Data.Hashable
import Gidl.Types
import Gidl.Interface

type MsgId = Word32
data Message = Message String Type
             deriving (Eq, Show)
data Schema = Schema String [(MsgId, Message)]
            deriving (Eq, Show)

-- set of all types required to implement a schema
schemaTypes :: Schema -> [Type]
schemaTypes (Schema _ ms) = nub (concat (map aux ms))
  where aux (_, (Message _ t)) = childTypes t

interfaceTypes :: Interface -> [Type]
interfaceTypes i = nub (ptypes ++ ctypes)
  where
  ptypes = schemaTypes (producerSchema i)
  ctypes = schemaTypes (consumerSchema i)

producerSchema :: Interface -> Schema
producerSchema ir = Schema "Producer" [(mkMsgId m, m) | m <- messages ]
  where
  messages = concatMap producerMessages (interfaceMethods ir)

producerMessages :: (MethodName,Method) -> [Message]
producerMessages (streamname, (StreamMethod _ tr)) =
  [ Message streamname tr ]
producerMessages (attrname, (AttrMethod perm tr)) =
  [ setResponseMessage attrname tr | writable perm ] ++
  [ getResponseMessage attrname tr | readable perm ]

consumerSchema :: Interface -> Schema
consumerSchema ir = Schema "Consumer" [(mkMsgId m, m) | m <- messages ]
  where
  messages = concatMap consumerMessages (interfaceMethods ir)

consumerMessages :: (MethodName,Method) -> [Message]
consumerMessages (_, (StreamMethod _ _)) = [] -- XXX eventaully add set rate?
consumerMessages (attrname, (AttrMethod perm tr)) =
  [ setRequestMessage attrname tr | writable perm ] ++
  [ getRequestMessage attrname tr | readable perm ]

setRequestMessage :: MethodName -> Type -> Message
setRequestMessage n t = Message (n ++ "_set_req") (sequenceNumStruct t)

setResponseMessage :: MethodName -> Type -> Message
setResponseMessage n _ = Message (n ++ "_set_resp") sequence_num_t

getRequestMessage :: MethodName -> Type -> Message
getRequestMessage n _ = Message (n ++ "_get_req") sequence_num_t

getResponseMessage :: MethodName -> Type -> Message
getResponseMessage n t = Message (n ++ "_get_resp") (sequenceNumStruct t)

sequenceNumStruct :: Type -> Type
sequenceNumStruct t = StructType ("sequence_numbered_" ++ (typeName t))
                          [ ("seqnum", sequence_num_t)
                          , ("val", t) ]

mkMsgId :: Message -> MsgId
mkMsgId = fromIntegral . hash . show

