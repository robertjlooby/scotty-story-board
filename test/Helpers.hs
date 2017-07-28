{-# LANGUAGE OverloadedStrings #-}

-- | Adapted from Test.Hspec.Wai
module Helpers where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Vault.Lazy as Vault
import           Network.CGI.Protocol (formEncode)
import           Network.HTTP.Types (Header, Method, hContentType, methodDelete, methodGet, methodOptions, methodPatch, methodPost, methodPut)
import           Network.Wai (Request(..), vault)
import           Network.Wai.Test (SRequest(..), SResponse, defaultRequest, runSession, setPath, srequest)
import           Test.Hspec.Wai (WaiSession)
import           Test.Hspec.Wai.Internal (getApp)

import           Session (Session, vaultKey)

run :: SRequest -> WaiSession SResponse
run req = getApp >>= liftIO . runSession (srequest req)

get' :: ByteString -> SRequest
get' path = request' methodGet path [] ""

post' :: ByteString -> LB.ByteString -> SRequest
post' path = request' methodPost path []

put' :: ByteString -> LB.ByteString -> SRequest
put' path = request' methodPut path []

patch' :: ByteString -> LB.ByteString -> SRequest
patch' path = request' methodPatch path []

options' :: ByteString -> SRequest
options' path = request' methodOptions path [] ""

delete' :: ByteString -> SRequest
delete' path = request' methodDelete path [] ""

request' :: Method -> ByteString -> [Header] -> LB.ByteString -> SRequest
request' method path headers body = SRequest req body
  where
    req = setPath defaultRequest {requestMethod = method, requestHeaders = headers} path

postHtmlForm' :: ByteString -> [(String, String)] -> SRequest
postHtmlForm' path = request' methodPost path [(hContentType, "application/x-www-form-urlencoded")] . LB8.pack . formEncode

putHtmlForm' :: ByteString -> [(String, String)] -> SRequest
putHtmlForm' path = request' methodPut path [(hContentType, "application/x-www-form-urlencoded")] . LB8.pack . formEncode

withSession :: Session -> SRequest -> SRequest
withSession session request = request {simpleRequest = withSessionSet}
  where
      req = simpleRequest request
      withSessionSet = req { vault = Vault.insert vaultKey session (vault req) }
