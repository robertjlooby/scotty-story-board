{-# LANGUAGE OverloadedStrings #-}

-- | Adapted from Test.Hspec.Wai
module Helpers where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.State.Lazy (evalStateT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.List (intersperse)
import           Data.Text.Lazy (Text)
import qualified Data.Vault.Lazy as Vault
import           Network.HTTP.Types (Header, Method, hContentType, methodDelete, methodGet, methodOptions, methodPatch, methodPost, methodPut)
import           Network.URI (escapeURIString, isUnescapedInURI)
import           Network.Wai (Request(..), vault)
import           Network.Wai.Test (SRequest(..), SResponse, defaultRequest, runSession, setPath, srequest)
import           Test.Hspec.Wai (WaiSession)
import           Test.Hspec.Wai.Internal (getApp)
import qualified Web.Scotty as S
import           Web.Scotty.Internal.Types (ActionEnv(..), ActionError, ScottyResponse, runAM)

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

runActionM :: ActionEnv -> ScottyResponse -> S.ActionM a -> IO (Either (ActionError Text) a)
runActionM env response action =
    evalStateT state response
  where
    reader = runExceptT $ runAM action
    state = runReaderT reader env

makeEnv :: Request -> ActionEnv
makeEnv req =
    Env req [] emptyBS emptyBS' []
  where
    emptyBS = return ""
    emptyBS' = return ""

-- From cgi lib
-- | Formats name-value pairs as application\/x-www-form-urlencoded.
formEncode :: [(String,String)] -> String
formEncode xs =
    concat $ intersperse "&" [urlEncode n ++ "=" ++ urlEncode v | (n,v) <- xs]

-- | Converts a single value to the application\/x-www-form-urlencoded encoding.
urlEncode :: String -> String
urlEncode = replace ' ' '+' . escapeURIString okChar
  where okChar c = c == ' ' ||
                   (isUnescapedInURI c && c `notElem` ("&=+" :: String))

-- | Replaces all instances of a value in a list by another value.
replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)
