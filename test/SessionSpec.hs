{-# LANGUAGE OverloadedStrings #-}

module SessionSpec where

import           Data.Vault.Lazy (empty, insert)
import           Network.Wai (defaultRequest, vault)
import           Test.Hspec (Spec, describe, it, shouldBe)

import           Helpers (makeEnv, runActionM)
import           Session
import qualified User as U

spec :: Spec
spec = describe "Session" $ do
    describe "getSession" $ do
        it "is Nothing if no session" $ do
            let env = makeEnv defaultRequest
            Right found <- runActionM env undefined getSession
            found `shouldBe` Nothing

        it "gets the session" $ do
            let session = Session (U.UserId 42)
                req = defaultRequest {vault = insert vaultKey session empty}
                env = makeEnv req
            Right found <- runActionM env undefined getSession
            found `shouldBe` Just session
