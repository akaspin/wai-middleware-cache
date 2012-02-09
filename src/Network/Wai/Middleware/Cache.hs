-- | Transparent front cache middleware for 'Network.Wai'.
--   
--   Instead caching internal data, this middleware caches entire responses.
--   Of course, this creates additional costs. However, the simplification of 
--   the internal structure and concentration of caching in the immediate 
--   vicinity of the request is more than redeem them.
--
--  > cache (hedisBackend defaultConnectInfo)
--  >       rawPathInfo 
--  >       (const ["cdn", "templates"])
--  >       lookupEtag
--  >
--  >
--  >
 
module Network.Wai.Middleware.Cache where

import Data.ByteString (ByteString)

import Network.Wai (Request(..))

cache :: 
       (Request -> ByteString)         -- ^ Cache key function
    -> (Request -> [ByteString])       -- ^ Cache tags function 
    -> (Request -> ByteString)         -- ^ 
cache = undefined
