{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Network.Wai (Application, responseLBS)
import Network.Wai.Test
import qualified Network.HTTP.Types as H

import Network.Wai.Middleware.Cache

main :: IO ()
main = defaultMain [
        testCase "App without ETag" caseSimple,
        testCase "App with Generated ETag" caseETag,
        testCase "Generated ETag" caseEmbeddedEtag
    ]

-- | Just test simple app
caseSimple :: Assertion
caseSimple = flip runSession appSimple $ do
    r <- defRequest
    assertResponseBase r
    assertNoHeader "etag" r

caseETag :: Assertion
caseETag = flip runSession (headerETag appSimple) $ do
    r <- defRequest
    assertResponseBase r
    assertHeader "etag" hashedData r

caseEmbeddedEtag :: Assertion
caseEmbeddedEtag = flip runSession appWithETag $ do
    r <- defRequest
    assertResponseBase r
    assertHeader "etag" "no-match" r

defRequest :: Session SResponse
defRequest = request defaultRequest

----------------------------------------------------------------------------
-- Assertions
----------------------------------------------------------------------------

assertResponseBase :: SResponse -> Session ()
assertResponseBase r = do
    assertStatus 200 r
    assertBody sourceData r

----------------------------------------------------------------------------
-- Applications
----------------------------------------------------------------------------
    
appSimple :: Application
appSimple _ = return $ responseLBS H.ok200 [] sourceData

appWithETag :: Application
appWithETag _ = return $ responseLBS H.ok200 
        [("ETag", "no-match")] sourceData

----------------------------------------------------------------------------
-- Data
----------------------------------------------------------------------------

sourceData :: BL.ByteString
sourceData = BL.fromChunks $ replicate 100000 "Hash response mock"

hashedData :: B.ByteString
hashedData = "385765267c8bd0154b5303b894dc34e7"

