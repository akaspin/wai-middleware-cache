{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))


import Network.Wai (Application, responseLBS)
import Network.Wai.Test
import qualified Network.HTTP.Types as H

import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = defaultMain [
        testCase "Just test" caseUncached
    ]
    
caseUncached :: Assertion
caseUncached = flip runSession testApp $ do
    r <- request defaultRequest
    liftIO $ print r
    
testApp :: Application
testApp _ = do
    liftIO $ print "test" 
    return $ responseLBS H.ok200 [] "short"
