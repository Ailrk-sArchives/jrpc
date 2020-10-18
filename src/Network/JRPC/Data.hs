{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.JRPC.Data where

-- https://www.jsonrpc.org/specification#request_object

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)
import Control.DeepSeq


data Version = JSONRPC2_0 | JSONRPC1_0
  deriving (Show, Eq, Read, Generic, NFData, FromJSON, ToJSON)

newtype Method = Method Text
  deriving (Show, Eq, Read, Generic, NFData, FromJSON, ToJSON)

data Id = IdStr Text | IdNum Int
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)

-- | Client side request can either be `Request` or `Notification`
-- the only difference is `Request` need to have an unique id.
data Request =
    Request       { unReqJsonRPC :: !Version
                  , unReqMethod :: !Method
                  , unReqParams :: !Value
                  , unReqId :: !Id
                  }
   | Notification { unReqJsonRPC :: !Version  -- notification has no id.
                  , unReqMethod :: !Method
                  , unReqParams :: !Value
                  }
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)


-- | Response can either return a result or error message.
data Response =
    Response { unResJsonRPC :: !Version
             , unResult :: !Value
             , unResId :: !Id
             }
  | OnError  { unResJsonRPC :: !Version
             , unResError :: !Value
             , unResId :: !Id
             }
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)

-- instance NFData Request where
--   rnf (Request v m p id) = rnf v `seq` rnf m `seq` rnf p `seq` rnf id
--   rnf (Notification v m p) = rnf v `seq` rnf m `seq` rnf p
