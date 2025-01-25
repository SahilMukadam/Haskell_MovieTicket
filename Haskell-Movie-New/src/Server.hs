{-|
Module      : Server
Description : This module defines the server that processes movie booking requests from clients.
Maintainer  : Your Name <your.email@example.com>
Stability   : experimental
Portability : portable

The `Server` module defines the server's behavior in the movie ticket booking system. 
It processes incoming booking requests from clients by retrieving them from the request queue, 
creating appropriate responses, and adding them to the response queue for the clients to receive.

This module contains:
- `startServer` function to process incoming requests from the queue and generate responses.

-}

module Server (startServer) where

import Control.Concurrent.MVar (MVar, putMVar)
import Control.Concurrent (threadDelay)
import Booking (Request(..), Response(..), createResponse)
import RequestQueue (RequestQueue, dequeue)
import Logging (logRequestResponse)  

-- | Starts the server that processes movie booking requests from the request queue.
startServer :: RequestQueue Request -> MVar Response -> IO ()
startServer requestQueue responseQueue = do
    let processRequests = do
            -- Try to dequeue a request from the request queue
            maybeRequest <- dequeue requestQueue
            case maybeRequest of
                -- If a request is retrieved, process it
                Just request -> do
                    -- Generate a response for the request
                    response <- createResponse request
                    
                    -- Enqueue the response in the response queue
                    putMVar responseQueue response
                    
                    -- Log the details of the processed request and response
                    putStrLn $ "Request processed for client " ++ show (reqClientId request)
                              ++ " | Movie: " ++ reqMovieName request
                              ++ " | Request Time: " ++ show (reqBookingDateTime request)
                              ++ " | Response Time: " ++ show (resResponseDateTime response)
                              
                    -- Continue processing further requests
                    processRequests

                -- If no request is available, wait and then continue
                Nothing -> threadDelay 100000 >> processRequests

    -- Start processing requests
    processRequests
