
module Gidl.Schema where

import Data.Word
import Data.Hashable
import Gidl.Types
import Gidl.Interface

type MsgId = Word32
data Message = Message String TypeRepr
             deriving (Eq, Show)
data Schema = Schema [(MsgId, Message)]
            deriving (Eq, Show)


producerSchema :: InterfaceRepr -> Schema
producerSchema ir = Schema [(mkMsgId m, m) | m <- messages ]
  where
  messages = concatMap mkMessages (interfaceMethods ir)
  mkMessages (streamname, (StreamMethod _ tr)) =
    [ Message streamname tr ]
  mkMessages (_ , (AttrMethod Write _)) = []
  mkMessages (attrname, (AttrMethod  _ tr)) =
    [ Message (attrname ++ "_val") tr ]

consumerSchema :: InterfaceRepr -> Schema
consumerSchema ir = Schema [(mkMsgId m, m) | m <- messages ]
  where
  messages = concatMap mkMessages (interfaceMethods ir)

  mkMessages (_, (StreamMethod _ _)) = [] -- XXX eventaully add set rate?
  mkMessages (attrname, (AttrMethod Write tr)) =
    [ Message (attrname ++ "_set") tr ]
  mkMessages (attrname, (AttrMethod Read _)) =
    [ Message (attrname ++ "_get") voidTypeRepr ]
  mkMessages (attrname, (AttrMethod ReadWrite tr)) =
    [ Message (attrname ++ "_set") tr
    , Message (attrname ++ "_get") voidTypeRepr
    ]


mkMsgId :: Message -> MsgId
mkMsgId = fromIntegral . hash . show

