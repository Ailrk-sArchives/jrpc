{-# LANGUAGE OverloadedStrings #-}

module Spec where

import Test.Hspec
import Network.JRPC
import Data.Aeson
import GHC.Exts

main :: IO ()
main = hspec $ do
  requestSpec


requestSpec :: Spec
requestSpec = do
  it "Request" $ encode req1 `shouldBe` req1str
  where
    req1str = "{\"jsonrpc\":\"2.0\",\"params\":[1,\"a\"],\"methods\":\"add\",\"id\":2}"
    req1 = Request { unReqJsonRPC = JSONRPC2_0
                   , unReqMethod = Method "add"
                   , unReqParams = Array $ fromList [Number 1, String "a"]
                   , unReqId = IdNum 2
                   }
