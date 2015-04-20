
module Gidl.Schema where

import Data.Word
import Data.Hashable
import Gidl.Types
import Gidl.Interface

type MsgId = Word32
data Message = Message String Type
             deriving (Eq, Show)
data Schema = Schema String [(MsgId, Message)]
            deriving (Eq, Show)


producerSchema :: Interface -> Schema
producerSchema ir = Schema "Producer" [(mkMsgId m, m) | m <- messages ]
  where
  messages = concatMap producerMessages (interfaceMethods ir)

producerMessages :: (MethodName,Method) -> [Message]
producerMessages (streamname, (StreamMethod _ tr)) =
  [ Message streamname tr ]
producerMessages (_ , (AttrMethod Write _)) = []
producerMessages (attrname, (AttrMethod  _ tr)) =
  [ Message (attrname ++ "_val") tr ]

consumerSchema :: Interface -> Schema
consumerSchema ir = Schema "Consumer" [(mkMsgId m, m) | m <- messages ]
  where
  messages = concatMap consumerMessages (interfaceMethods ir)

consumerMessages :: (MethodName,Method) -> [Message]
consumerMessages (_, (StreamMethod _ _)) = [] -- XXX eventaully add set rate?
consumerMessages (attrname, (AttrMethod Write tr)) =
  [ Message (attrname ++ "_set") tr ]
consumerMessages (attrname, (AttrMethod Read _)) =
  [ Message (attrname ++ "_get")  (PrimType VoidType) ]
consumerMessages (attrname, (AttrMethod ReadWrite tr)) =
  [ Message (attrname ++ "_set") tr
  , Message (attrname ++ "_get") (PrimType VoidType)
  ]


mkMsgId :: Message -> MsgId
mkMsgId = fromIntegral . hash . show

