{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Text
import           GHC.Exts
import           Network.JRPC
import           Test.Hspec

main :: IO ()
main = hspec $ do
  requestSpec
  errorobjSpec


requestSpec :: Spec
requestSpec =
    describe "Request" $ do
      it "Request encode" $ encode req1 `shouldBe` req1str
      it "Request decode"  $
        (decode req1str :: Maybe Request) `shouldBe` Just req1
    where
      req1str = "{\"jsonrpc\":\"2.0\",\"params\":[1,\"a\"],\"methods\":\"add\",\"id\":2}"
      req1 = Request { reqJsonRPC = JSONRPC2_0
                     , reqMethod = Method "add"
                     , reqParams = Array $ fromList [Number 1, String "a"]
                     , reqId = IdNum 2
                     }

errorobjSpec :: Spec
errorobjSpec =
  describe "Error Obj" $ do
    it "Error V2 encode" $ encode err1 `shouldBe` err1str
    it "Error V2 decode" $ (decode err1str :: Maybe Error) `shouldBe` Just err1
  where
    err1str = "{\"data\":1,\"code\":-32700,\"message\":\"[-32700] ParseError: This is delibrate\"}"
    err1 = errorobj ParseError "This is delibrate" $ Number 1

responseSpec :: Spec
responseSpec =
  describe "Response" $ do
    it "Resopnse encode" $ encode res1 `shouldBe` res1str
    it "Resopnse decode" $ (decode res1str :: Maybe Response) `shouldBe` Just res1
    it "OnErr encode" $ encode onErr1 `shouldBe` onErr1Str
    it "OnErr decode" $ (decode onErr1Str :: Maybe Response) `shouldBe` Just onErr1
  where
    res1 :: Response
    res1str = "{\"result\":1,\"jsonrpc\":\"2.0\",\"id\":20}"
    onErr1Str = "{\"error\":{\"data\":1,\"code\":-32700,\"message\":\"[-32700] ParseError: This is delibrate\"},\"jsonrpc\":\"2.0\",\"id\":20}"

    res1 = Response { resJsonRPC = JSONRPC2_0
                    , resResult = Number 1.0
                    , resId = IdNum 20
                    }
    onErr1 = OnError { resJsonRPC = JSONRPC2_0
                     , resError =
                       errorobj ParseError "This is delibrate" $ Number 1
                     , resId = IdNum 20
                     }
