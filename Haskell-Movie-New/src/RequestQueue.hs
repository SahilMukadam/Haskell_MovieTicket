{-|
Module      : RequestQueue
Description : This module defines the request queue and operations to enqueue and dequeue requests.
Maintainer  : Your Name <your.email@example.com>
Stability   : experimental
Portability : portable

The `RequestQueue` module provides a basic implementation of a queue for managing requests. 
It allows clients to enqueue requests into a shared queue and dequeue them for processing.

This module contains:
- `RequestQueue` type, which represents the queue of requests.
- `newRequestQueue` function to create a new empty queue.
- `enqueue` function to add a request to the queue.
- `dequeue` function to remove and return the first request from the queue.

-}

module RequestQueue (RequestQueue, newRequestQueue, enqueue, dequeue) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Monad (void)
import Data.Maybe (isJust)

-- | The `RequestQueue` type represents a queue of requests. It is backed by an `MVar` containing a list of requests.
type RequestQueue a = MVar [a]

-- | Creates a new empty request queue.
newRequestQueue :: IO (RequestQueue a)
newRequestQueue = newMVar []

-- | Adds a request to the queue.
enqueue :: RequestQueue a -> a -> IO ()
enqueue queue request = do
    -- Take the current queue from the MVar, modify it by adding the request, and put it back in the MVar
    requests <- takeMVar queue
    putMVar queue (requests ++ [request])

-- | Removes and returns the first request from the queue.
dequeue :: RequestQueue a -> IO (Maybe a)
dequeue queue = do
    -- Take the current queue from the MVar and modify it
    requests <- takeMVar queue
    case requests of
        -- If the queue is empty, return Nothing and reset the queue
        [] -> do
            putMVar queue []
            return Nothing
        -- Otherwise, return the first request and modify the queue
        (x:xs) -> do
            putMVar queue xs
            return (Just x)
