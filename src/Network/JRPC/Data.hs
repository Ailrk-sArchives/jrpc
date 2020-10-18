{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.JRPC.Data where

-- https://www.jsonrpc.org/specification#request_object

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)
import Control.DeepSeq

-- | Version
data Version = JSONRPC2_0 | JSONRPC1_0
  deriving (Show, Eq, Read, Generic, NFData)

instance FromJSON Version where
  parseJSON "1.0" = pure JSONRPC1_0
  parseJSON "2.0" = pure JSONRPC2_0
  parseJSON _ = error "wrong version"

instance ToJSON Version where
  toJSON JSONRPC1_0 = "1.0"
  toJSON JSONRPC2_0 = "2.0"


data Coord = Coord { x :: Double, y :: Double } deriving Generic
instance FromJSON Coord
instance ToJSON Coord

-- | Method
newtype Method = Method Text
  deriving (Show, Eq, Read, Generic, NFData, FromJSON, ToJSON)


-- | Id
data Id = IdStr Text | IdNum Int
  deriving (Eq, Show, Generic, NFData)

instance FromJSON Id where
  parseJSON (String s) = pure $ IdStr s
  parseJSON (Number (I n)) = pure $ IdNum n


instance ToJSON Id where
  toJSON (IdStr s) = toJSON s
  toJSON (IdNum n) = toJSON n


-- | Request Object
-- Client side request can either be `Request` or `Notification`
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


-- | Response Object
-- It can either return a result or error message.
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


-- | Error Object
data Error =
    ErrorV2 { unErrorCode :: !Int
            , unErrorMessage :: !Text
            , unErrorData :: !Value
            }
  | ErrorV1 { unErrorData :: !Value }
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)
