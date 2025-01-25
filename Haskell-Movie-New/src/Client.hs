{-|
Module      : Client
Description : This module simulates a client that sends movie booking requests to the server.
Maintainer  : Your Name <your.email@example.com>
Stability   : experimental
Portability : portable

The `Client` module simulates the behavior of a client in a movie ticket booking system. 
It continuously generates booking requests with random data, including a client ID, movie selection, 
number of seats, and booking time. The client then sends these requests to a shared request queue 
for processing by the server.

This module contains:
- `startClient` function to simulate a client that repeatedly sends booking requests to a shared queue.

-}

module Client (startClient) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar)
import System.Random (randomRIO)
import Data.Time (getCurrentTime)
import Booking (Request(..))
import RequestQueue (RequestQueue, enqueue)
import Logging (logRequestResponse)

startClient :: Int -> RequestQueue Request -> IO ()
startClient clientId requestQueue = do
    let makeRequest = do
            -- Generate a random delay (between 100,000 and 500,000 microseconds)
            delay <- randomRIO (100000, 500000)
            threadDelay delay

            -- Get the current system time (for the booking time)
            currentTime <- getCurrentTime

            -- Generate a random number of seats (between 2 and 10)
            seats <- randomRIO (2, 10)

            -- Create a movie name based on the client ID (this will create a different movie name for each client)
            let movie = "Random Movie " ++ show (clientId `mod` 5 + 1)

            -- Create a booking request
            let request = Request clientId movie currentTime seats

            -- Enqueue the request to the shared request queue
            enqueue requestQueue request

            -- Log the details of the request
            let logMessage = "Client " ++ show clientId ++ " sent a request for movie '" ++ movie ++ "'."
            let orderConfirmDateTime = show currentTime  -- Simulate the order confirmation time
            logRequestResponse (show clientId) movie (show currentTime) orderConfirmDateTime

            -- Recursively continue the process of making requests
            makeRequest

    -- Start the recursive process to generate requests
    makeRequest
