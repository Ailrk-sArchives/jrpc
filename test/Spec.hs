{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Network.JRPC
import Data.Aeson
import Data.Text
import GHC.Exts

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
      req1 = Request { unReqJsonRPC = JSONRPC2_0
                     , unReqMethod = Method "add"
                     , unReqParams = Array $ fromList [Number 1, String "a"]
                     , unReqId = IdNum 2
                     }

errorobjSpec :: Spec
errorobjSpec =
  describe "Error Obj" $ do
    it "Error V2 encode" $ encode err1 `shouldBe` err1str
    it "Error V2 decode" $ (decode err1str :: Maybe Error) `shouldBe` Just err1
  where
    err1str = "{\"data\":1,\"code\":-32700,\"message\":\"[-32700] Parse Error: This is delibrate\"}"
    err1 = errorobj2 (-32700) "This is delibrate" $ Number 1

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
    onErr1Str = "{\"error\":{\"data\":1,\"code\":-32700,\"message\":\"[-32700] Parse Error: This is delibrate\"},\"jsonrpc\":\"2.0\",\"id\":20}"

    res1 = Response { unResJsonRPC = JSONRPC2_0
                    , unResult = Number 1.0
                    , unResId = IdNum 20
                    }
    onErr1 = OnError { unResJsonRPC = JSONRPC2_0
                     , unResError =
                       errorobj2 (-32700) "This is delibrate" $ Number 1
                     , unResId = IdNum 20
                     }
