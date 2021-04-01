{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.JRPC.Data where

-- ( Version (..),
--   Id (..),
--   Method (..),
--   Request (..),
--   Response (..),
--   Error (..),
--   ErrorCode (..),
--   mkError2,
--   mkError,
-- )

-- https://www.jsonrpc.org/specification#request_object
{- This module defines request, response and error data types plus
   their aeson instances.
   Besides that, we have typeclass FromRequest, ToRequest, fromResponse,
   toResponse that can be helpful when you wan to convert
-}

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Text
import GHC.Generics (Generic)

-- --------------------------------------------------------------------------
-- Base
-- --------------------------------------------------------------------------

-- | Version
data Version = JSONRPC2_0 | JSONRPC1_0
  deriving stock (Show, Eq, Read, Generic)

instance NFData Version

-- get the version from a jsonrpc message.
parseVersion :: Object -> Parser Version
parseVersion v = do
  val <- v .:? "jsonrpc"
  case val of
    Just ("2.0" :: Text) -> pure JSONRPC2_0
    _ -> pure JSONRPC1_0

instance FromJSON Version where
  parseJSON (String n) = case n of
    "2.0" -> pure JSONRPC2_0
    _ -> pure JSONRPC1_0

instance ToJSON Version where
  toJSON JSONRPC2_0 = "2.0"
  toJSON JSONRPC1_0 = "1.0"

-- | Method
newtype Method = Method Text
  deriving stock (Show, Eq, Read, Generic)
  deriving newtype (NFData, FromJSON, ToJSON)

-- | Id can be either string or integer.
data Id = IdStr Text | IdNum Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

instance FromJSON Id where
  parseJSON (String s) = pure . IdStr $ s
  parseJSON (Number n) = pure . IdNum . floor $ n
  parseJSON _ = mempty

instance ToJSON Id where
  toJSON (IdStr s) = toJSON s
  toJSON (IdNum n) = toJSON n

-- --------------------------------------------------------------------------
--  Request
-- --------------------------------------------------------------------------

-- | Request Object
--
-- Client side request can either be `Request` or `Notification`
-- Notification will not receive a response.
data Request
  = Request
      { reqJsonRPC :: !Version,
        reqMethod :: !Method,
        reqParams :: !Value,
        reqId :: !Id
      }
  | Notification
      { reqJsonRPC :: !Version,
        reqMethod :: !Method,
        reqParams :: !Value
      }
  deriving stock (Eq, Show, Generic)

instance NFData Request

-- | parse
parseRequest :: Object -> Parser Request
parseRequest o = do
  reqJsonRPC <- parseVersion o
  reqMethod <- o .: "methods"
  reqParams <- o .:? "params" .!= Null
  reqId' <- o .:? "id"
  case reqId' of
    Just reqId -> pure Request {..}
    _ -> pure Notification {..}

instance FromJSON Request where
  parseJSON = withObject "Request" parseRequest

instance ToJSON Request where
  toJSON Request {..}
    | reqParams == Null =
      object
        ["jsonrpc" .= reqJsonRPC, "method" .= reqMethod, "id" .= reqId]
    | otherwise =
      object
        [ "jsonrpc" .= reqJsonRPC,
          "methods" .= reqMethod,
          "params" .= reqParams,
          "id" .= reqId
        ]
  toJSON Notification {..} =
    object
      ["jsonrpc" .= reqJsonRPC, "methods" .= reqMethod, "params" .= reqParams]

-- --------------------------------------------------------------------------
--  Response
-- --------------------------------------------------------------------------

-- | `Response` on success, `OnError` on error.
data Response
  = Response
      { resJsonRPC :: !Version,
        resResult :: !Value,
        resId :: !Id
      }
  | OnError
      { resJsonRPC :: !Version,
        resError :: !Error,
        resId :: !Id
      }
  deriving stock (Eq, Show, Generic)

instance NFData Response

parseResponse :: Object -> Parser Response
parseResponse o = do
  resJsonRPC <- parseVersion o
  resResult' <- o .:? "result"
  resError' <- o .:? "error"
  resId <- o .: "id"
  case (resResult', resError') of
    (Just resResult, Nothing) -> pure Response {..}
    (Nothing, Just resError) -> pure OnError {..}
    _ -> error "Unknown response"

instance FromJSON Response where
  parseJSON = withObject "Response" parseResponse

instance ToJSON Response where
  toJSON Response {..} =
    object
      ["jsonrpc" .= resJsonRPC, "result" .= resResult, "id" .= resId]
  toJSON OnError {..} =
    object
      ["jsonrpc" .= resJsonRPC, "error" .= resError, "id" .= resId]
  {-# INLINE toJSON #-}

-- --------------------------------------------------------------------------
--  ErrorObject
-- --------------------------------------------------------------------------

data ErrorCode
  = ParseError
  | InvalidRequest
  | InvalidParams
  | MethodNotFound
  | InternalError
  | ServerError Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

ecode =
  [ (ParseError, -32700),
    (InvalidRequest, -32600),
    (MethodNotFound, -32601),
    (InvalidParams, -32602),
    (InternalError, -32603)
  ]

-- build isomorphism between error code and their adt representation.
codeMapFrom = HM.fromList ecode

codeMapTo :: HM.HashMap Int ErrorCode
codeMapTo = HM.fromList $ (\(a, b) -> (b, a)) <$> ecode

fromErrorcode :: ErrorCode -> Int
fromErrorcode = \case
  ServerError n -> n
  other -> codeMapFrom HM.! other

toErrorcode :: Int -> Maybe ErrorCode
toErrorcode n
  | isValidEcode n =
    Just $
      if n >= (-32099) && n <= (-32000)
        then ServerError n
        else codeMapTo HM.! n
  | otherwise = Nothing

instance NFData ErrorCode

instance FromJSON ErrorCode where
  parseJSON = withText "ErrorCode" $ \t -> do
    let v = read . unpack $ t :: Int
    maybe mempty pure (toErrorcode v)

instance ToJSON ErrorCode where
  toJSON = toJSON . fromErrorcode

-- | Check if the error code is valid
isValidEcode :: Int -> Bool
isValidEcode c =
  c == (-32700)
    || (c >= (-32603) && c <= (-32600))
    || (c >= (-32099) && c <= (-32000))

-- | Error type
data Error
  = ErrorV2
      { errorCode :: !ErrorCode,
        errorMessage :: !Text,
        errorData :: !Value
      }
  | ErrorV1 {errorData :: !Value}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

parseError :: Object -> Parser Error
parseError o = do
  codeNum <- o .: "code"
  let code = toErrorcode codeNum
  case code of
    Just c ->
      if isValidEcode codeNum
        then
          asum
            [ pure (ErrorV2 c) <*> o .: "message" <*> o .: "data",
              ErrorV1 <$> o .: "data"
            ]
        else mempty
    Nothing -> mempty

instance FromJSON Error where
  parseJSON = withObject "Error" parseError

instance ToJSON Error where
  toJSON (ErrorV1 n) = object ["data" .= n]
  toJSON ErrorV2 {..} =
    object
      [ "code" .= errorCode,
        "message" .= errorMessage,
        "data" .= errorData
      ]

-- | Helper function to create error object.
-- It only creates error for jsonrpc 2.0
-- >>> mkError2 ParseError "good"
mkError2 :: ErrorCode -> Text -> (Value -> Error)
mkError2 e info = ErrorV2 e (msg info)
  where
    msg = showErr (pack . show $ e)
      where
        showErr errTitle errMsg =
          pack "["
            <> pack (show (fromErrorcode e))
            <> pack "] "
            <> errTitle
            <> pack ": "
            <> errMsg

mkError = mkError2

---------------
--  Message  --
---------------

-- | Batch
