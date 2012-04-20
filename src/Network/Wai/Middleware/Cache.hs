{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE OverloadedStrings #-} 

-- | Transparent front cache middleware for 'Network.Wai'.
--   
--   Instead caching internal data, this middleware caches entire responses.
--   Of course, this creates additional costs. However, the simplification of 
--   the internal structure and concentration of caching in the immediate 
--   vicinity of the request is more than redeem them.
--
--  > cache (debugBackend True)
--  >       ourFrivolousApplication
 
module Network.Wai.Middleware.Cache (
    -- * Middleware
    CacheBackend,
    CacheBackendError(..),
    cache,
    headerETag,
    
    -- * Request helpers
    lookupETag
) where

import Prelude hiding (concatMap)

import Control.Exception (Exception)

import Numeric (showHex)

import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy (empty)

import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Serialize (encode)

import Data.Conduit (ResourceT, ($=), ($$), Flush(..))
import qualified Data.Conduit.List as CL
import Data.Conduit.Blaze (builderToByteStringFlush)
import Crypto.Conduit (sinkHash)

import Network.Wai (Application, Middleware, Request(..), Response(..),
        responseLBS, responseSource)
import Network.HTTP.Types (status304)

-- | Abstract cache backend. Result may be 'Nothing' you need to respond  
--   with status @304 - Not Modified@. 
type CacheBackend =
       Application      -- ^ Application
    -> Request          -- ^ Request
    -> ResourceT IO (Maybe Response)

-- | Cache backend can throw errors. For handle this, use, for example,
--   "Network.Wai.Middleware.Catch".
data CacheBackendError = CacheBackendError B.ByteString
    deriving (Show, Eq, Typeable)
instance Exception CacheBackendError

-- | Cache middleware. Use it with conjuction with 'CacheBackend' and 
--   'headerETag'.  
--  
--  > -- Simplest backend. Suggests @304 - Not Modified@ with site root.
--  > rootBackend app req = do 
--  >     case rawPathInfo req of
--  >         "/" -> return Nothing
--  >         _ -> do
--  >             res <- app req 
--  >             return $ Just res
--  > app = responseLBS ok200 [] "someresponse"
--  > 
--  > cachedApp = cache rootBackend $ headerETag $ app
cache ::
       CacheBackend     -- ^ Cache backend.
    -> Middleware
cache cacheBackend app req = do
    res <- cacheBackend app req
    return $ fromMaybe (responseLBS status304 [] empty) res 

-- | Add \"ETag\" header to response if it not present. Value of header is 
--   @MD5@ hash of response body.
headerETag :: Middleware
headerETag app req = do
    res <- app req
    let (rs, rh, rsrc) = responseSource res
    case lookup "etag" rh of
        (Just _) -> return res
        Nothing -> do
            digest <- rsrc $= builderToByteStringFlush $= 
                    CL.map fromChunk $$ sinkHash
            let hash = toHex . encode $ (digest :: MD5Digest)
            return $ ResponseSource rs (("ETag", hash):rh) rsrc
  where
    fromChunk (Chunk a) = a
    fromChunk Flush = ""
    
    -- | Convert to hex
    toHex :: B.ByteString -> B.ByteString
    toHex =
        B.concatMap word8ToHex
      where
        word8ToHex :: Word8 -> B.ByteString
        word8ToHex w = B8.pack $ pad $ showHex w []
        -- We know that the input will always be 1 or 2 characters long.
        pad :: String -> String
        pad [x] = ['0', x]
        pad s   = s
    
----------------------------------------------------------------------------
-- Request Helpers
----------------------------------------------------------------------------

-- | Helper for extract @If-None-Match@ header from 'Request'. Use this with 
--   backends.
lookupETag :: Request -> Maybe B.ByteString
lookupETag = lookup "If-None-Match" . requestHeaders




