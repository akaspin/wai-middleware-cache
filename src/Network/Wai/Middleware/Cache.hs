
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
    -- * Backend
    CacheBackend,
    -- * Middleware
    cache
) where

import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (empty)
import Data.Conduit (ResourceT)

import Network.Wai (Application, Middleware, Request(..), Response(..),
        responseLBS)
import Network.HTTP.Types (status304)

-- | Abstract cache backend. Result may be 'Nothing' you need to respond  
--   wirh status @304 - Not Modified@.
type CacheBackend =
       Application      -- ^ Application
    -> Request          -- ^ Request
    -> ResourceT IO (Maybe Response)

-- | Cache middleware. Use it with conjuction with 'CacheBackend'.  
--  
--  > -- Simplest backend. Suggests @304 - Not Modified@ with site root.
--  > rootBackend app req = do 
--  >     case rawPathInfo req of
--  >         "/" -> return Nothing
--  >         _ -> do
--  >             res <- app req 
--  >             return $ Just res
cache ::
       CacheBackend     -- ^ Cache backend.
    -> Middleware
cache cacheBackend app req = do
    res <- cacheBackend app req
    return $ fromMaybe (responseLBS status304 [] empty) res 

