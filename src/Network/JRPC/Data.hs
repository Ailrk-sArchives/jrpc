{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.JRPC.Data
  ( Version(..)
  , Id(..)
  , Method(..)
  , Request(..)
  , Response(..)
  , Error(..)
  , ErrorCode(..)
  , errorobj2
  )
    where

-- https://www.jsonrpc.org/specification#request_object

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.Hashable       (Hashable)
import           Data.Text
import           GHC.Generics        (Generic)

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
    _                    -> JSONRPC1_0

instance FromJSON Version where
  parseJSON (String n) = case n of
    "2.0" -> pure JSONRPC2_0
    _     -> pure JSONRPC1_0

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

instance Hashable Id

instance FromJSON Id where
  parseJSON (String s) = pure $ IdStr s
  parseJSON (Number n) = pure $ IdNum $ floor n
  parseJSON _          = mempty

instance ToJSON Id where
  toJSON (IdStr s) = toJSON s
  toJSON (IdNum n) = toJSON n

---------------
--  Request  --
---------------

-- | Request Object
-- Client side request can either be `Request` or `Notification`
-- the only difference is `Request` need to have an unique id.
data Request
  = Request { reqJsonRPC :: !Version
            , reqMethod  :: !Method
            , reqParams  :: !Value
            , reqId      :: !Id
            }
  | Notification { reqJsonRPC :: !Version
                 , reqMethod  :: !Method
                 , reqParams  :: !Value
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
    _       -> Notification v m p

instance FromJSON Request where
  parseJSON = withObject "Request" parseRequest

instance ToJSON Request where
  toJSON (Request v m p i)
    | p == Null = object $ ["jsonrpc" .= v, "method" .= m, "id" .= i]
    | otherwise =
      object $
        [ "jsonrpc" .= v,
          "methods" .= m,
          "params" .= p,
          "id" .= i
        ]
  toJSON (Notification v m p) =
    object $
      [ "jsonrpc" .= v,
        "methods" .= m,
        "params" .= p
      ]

----------------
--  Response  --
----------------

-- | `Response` on success, `OnError` on error.
data Response
  = Response
      { resJsonRPC :: !Version
      , result     :: !Value
      , resId      :: !Id
      }
  | OnError
      { resJsonRPC :: !Version
      , resError   :: !Error
      , resId      :: !Id
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
    _                  -> error "Impossible response"

instance FromJSON Response where
  parseJSON = withObject "Response" parseResponse

instance ToJSON Response where
  toJSON (Response v r i) =
    object $
      ["jsonrpc" .= v, "result" .= r, "id" .= i]
  toJSON (OnError v e i) =
    object $
      ["jsonrpc" .= v, "error" .= e, "id" .= i]

-------------------
--  ErrorObject  --
-------------------
data ErrorCode = ParseError | InvalidRequest | InvalidParams | MethodNotFound | InternalError | ServerError Int
  deriving (Eq, Show, Generic, Hashable)

ecode = [ (ParseError,  (-32700))
        , (InvalidRequest,  (-32600))
        , (MethodNotFound,  (-32601))
        , (InvalidParams,  (-32602))
        , (InternalError,  (-32603))
        ]
codeMapFrom = HM.fromList ecode
codeMapTo = HM.fromList $ (\(a, b) -> (b, a)) <$> ecode

fromEcode :: ErrorCode -> Int
fromEcode = \case
  ServerError n -> n
  other         -> codeMapFrom HM.! other

-- >>> toEcode (-32098)
-- <command line>: /nix/store/9df65igwjmf2wbw0gbrrgair6piqjgmi-glibc-2.31/lib/librt.so: symbol pthread_sigmask version GLIBC_2.2.5 not defined in file libpthread.so.0 with link time reference
toEcode :: Int -> Maybe ErrorCode
toEcode n | isValidEcode n = Just $ if n >= (-32099) && n <= (-32000)
                                       then ServerError n
                                       else codeMapTo HM.! n
  | otherwise = Nothing

instance NFData ErrorCode

instance FromJSON ErrorCode where
  parseJSON = withText "ErrorCode" $ \t -> let v = read . unpack $ t :: Int
                                            in case toEcode v of
                                                 Just c  -> pure c
                                                 Nothing -> mempty

instance ToJSON ErrorCode where
  toJSON = toJSON . fromEcode

-- | Check if the error code is valid
isValidEcode :: Int -> Bool
isValidEcode c = c == (-32700)
    || (c >= (-32603) && c <= (-32600))
    || (c >= (-32099) && c <= (-32000))

parseError o = do
  codeNum <- o .: "code"
  let code = toEcode codeNum
  case code of
    Just c -> if isValidEcode codeNum
                 then asum [ ErrorV2 <$> pure c <*> o .: "message" <*> o .: "data"
                           , ErrorV1 <$> o .: "data" ]
                 else mempty
    Nothing -> mempty

-- | Best way to creat Error is through constructor errorobj2.
-- errorobj2 will ensure valid error code.
data Error
  = ErrorV2
      { errorCode    :: !ErrorCode,
        errorMessage :: !Text,
        errorData    :: !Value
      }
  | ErrorV1 {errorData :: !Value}
  deriving (Eq, Show, Generic)
instance NFData Error

instance FromJSON Error where
  parseJSON = withObject "Error" parseError

instance ToJSON Error where
  toJSON (ErrorV1 n) = object ["data" .= n]
  toJSON (ErrorV2 c m n) =
    object
      [ "code" .= c,
        "message" .= m,
        "data" .= n
      ]

-- | Create ErrorV2
-- >>> errorobj2 ParseError "good"
errorobj2 :: ErrorCode -> Text -> (Value -> Error)
errorobj2 e info = ErrorV2 e (msg n info)
  where
    n = fromEcode e
    msg n = showErr n (pack . show $ e)
      where
        showErr errCode errTitle errMsg =
          pack "["
            <> (pack $ show errCode)
            <> pack "] "
            <> errTitle
            <> pack ": "
            <> errMsg


---------------
--  Message  --
---------------
-- | Batch

