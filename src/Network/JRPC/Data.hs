{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.JRPC.Data where

-- https://www.jsonrpc.org/specification#request_object

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics (Generic)
import GHC.Exts
import Data.Foldable
import Control.DeepSeq


--------------
--  Basic   --
--------------
-- | Version
data Version = JSONRPC2_0 | JSONRPC1_0
  deriving (Show, Eq, Read, Generic)
instance NFData Version

parseVersion :: Object -> Parser Version
parseVersion v = do
  val <- v .:? "jsonrpc"
  return $ case val of
           Just ("2.0" :: Text) -> JSONRPC2_0
           _ -> JSONRPC1_0

instance FromJSON Version where
  parseJSON (String n) = case n of
                           "2.0" -> pure JSONRPC2_0
                           _ -> pure JSONRPC1_0

instance ToJSON Version where
  toJSON JSONRPC2_0 = "2.0"
  toJSON JSONRPC1_0 = "1.0"

-- | Method
newtype Method = Method Text deriving (Show, Eq, Read, Generic)
instance NFData Method
instance FromJSON Method
instance ToJSON Method

-- | Id
data Id = IdStr Text | IdNum Int
  deriving (Eq, Show, Generic)
instance NFData Id

instance FromJSON Id where
  parseJSON (String s) = pure $ IdStr s
  parseJSON (Number n) = pure $ IdNum $ floor n
  parseJSON _ = mempty

instance ToJSON Id where
  toJSON (IdStr s) = toJSON s
  toJSON (IdNum n) = toJSON n


---------------
--  Request  --
---------------
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
  deriving (Eq, Show, Generic)

instance NFData Request

parseRequest :: Object -> Parser Request
parseRequest o = do
  v <- parseVersion o
  m <- o .: "methods"
  p <- o .:? "params" .!= Null
  i <- o .:? "id"
  return $ case i of
             Just id -> Request v m p id
             _ -> Notification v m p

instance FromJSON Request where
  parseJSON = withObject "Request" parseRequest

instance ToJSON Request where
  toJSON (Request v m p i)
    | p == Null = object $ [ "jsonrpc" .= v , "method" .= m , "id" .= i ]
    | otherwise = object $ [ "jsonrpc" .= v
                           , "methods" .= m
                           , "params" .= p
                           , "id" .= i
                           ]
  toJSON (Notification v m p) = object $
    [ "jsonrpc" .= v
    , "methods" .= m
    , "params" .= p
    ]


----------------
--  Response  --
----------------
-- | Response Object
-- It can either return a result or error message.
data Response =
    Response { unResJsonRPC :: !Version
             , unResult :: !Value
             , unResId :: !Id
             }
  | OnError  { unResJsonRPC :: !Version
             , unResError :: !Error
             , unResId :: !Id
             }
  deriving (Eq, Show, Generic)

instance NFData Response

parseResponse :: Object -> Parser Response
parseResponse o = do
  v <- parseVersion o
  r <- o .:? "result"
  e <- o .:? "error"
  id <- o .: "id"
  return $ case (r, e) of
             (Just r', Nothing) -> Response v r' id
             (Nothing, Just e') -> OnError v e' id
             _ -> error "Impossible response"

instance FromJSON Response where
  parseJSON = withObject "Response" parseResponse

instance ToJSON Response where
  toJSON (Response v r i) = object $
    [ "jsonrpc" .= v
    , "result" .= r
    , "id" .= i]
  toJSON (OnError v e i ) = object $
    [ "jsonrpc" .= v
    , "error" .= e
    , "id" .= i]


-------------------
--  ErrorObject  --
-------------------
data Error =
    ErrorV2 { unErrorCode :: !Int
            , unErrorMessage :: !Text
            , unErrorData :: !Value
            }
  | ErrorV1 { unErrorData :: !Value }
  deriving (Eq, Show, Generic)

instance NFData Error

-- | Check if the error code is valid
isValidErrorCode :: Int -> Bool
isValidErrorCode code = code == (-32700)
  || (code >= (-32603) && code <= (-32600))
  ||  (code >= (-32099) && code <= (-32000))

-- | Create ErrorV2
errorobj2 :: Int -> Text -> (Value -> Error)
errorobj2 n info = ErrorV2 n (msg info)
  where
    showErr :: Int -> Text -> Text -> Text
    showErr errCode errTitle errMsg = pack "["
      <> (pack  $ show errCode) <> pack "] " <> errTitle
      <> pack ": " <> errMsg
    msg  = case n of
             (-32700) -> showErr n "Parse Error"
             (-32600) -> showErr n "Invalid Request"
             (-32601) -> showErr n "Method Not Found"
             (-32602) -> showErr n "Invalid Params"
             (-32603) -> showErr n "Internal error"
             _ -> if (n >= (-32099) && n <= (-32000))
                     then showErr n "Server error"
                     else const "Unkown error code"

parseError o = do
  code <- o .: "code"
  if isValidErrorCode code
     then asum [ ErrorV2 <$> pure code
                         <*> o .: "message"
                         <*> o .: "data"
               , ErrorV1 <$> o .: "data"]
     else mempty

instance FromJSON Error where
  parseJSON = withObject "Error" parseError

instance ToJSON Error where
  toJSON (ErrorV1 n) = object [ "data" .= n ]
  toJSON (ErrorV2 c m n)  = object [ "code" .= c
                                   , "message" .= m
                                   , "data" .= n
                                   ]


---------------
--  Message  --
---------------

-- | Batch
