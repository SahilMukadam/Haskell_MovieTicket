{-|
Module      : Main
Description : Main module to simulate a movie booking system with multiple clients and a server.

This module simulates a movie booking system where multiple clients send booking requests, 
and a server processes those requests. The system demonstrates concurrent programming by 
utilizing threads for clients and a server. Each client sends a set of requests for movie bookings, 
and the server processes these requests while logging relevant details. At the end of the simulation, 
an HTML report is generated to summarize the booking details.

The core components of this module include:
- Clients: They send booking requests with random movies and booking times.
- Server: It processes the requests, generates responses, and logs the details.
- Logging: All client requests and server responses are logged.
- Reporting: An interactive HTML report is generated to show the results of the simulation.

-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent (forkIO, threadDelay)
import System.Random (randomRIO)
import RequestQueue (RequestQueue, newRequestQueue, enqueue, dequeue)
import Booking (Request(..), Response(..), createResponse)
import Data.Time (getCurrentTime)
import Logging (logRequestResponse, generateHTMLReport)

movies :: [String]
movies = ["The Matrix", "Inception", "The Dark Knight", "Forrest Gump", "Interstellar", 
          "Moana 2", "Iron Man (2008)", "The Amazing Spiderman", "TENET", "The Lion King"]

-- This value defines how many clients will be sending requests to the server in parallel. 
numClients :: Int
numClients = 10

-- | The total number of requests that will be processed (between 100 and 150).
-- This variable defines the total number of booking requests that will be sent in the simulation. 
totalRequests :: Int
totalRequests = 120  -- You can adjust this as needed (between 100 and 150)

-- | Simulates the behavior of a client in the booking system.
startClient :: Int -> RequestQueue Request -> MVar Response -> IO ()
startClient clientId requestQueue responseQueue = do
    let numRequests = totalRequests `div` numClients  -- Each client makes an equal number of requests
    mapM_ (makeRequest clientId requestQueue responseQueue) [1..numRequests]

  where
    makeRequest :: Int -> RequestQueue Request -> MVar Response -> Int -> IO ()
    makeRequest clientId requestQueue responseQueue _ = do
        -- Introduce a random delay between 100ms and 1 second for each request.
        threadDelay =<< randomRIO (100000, 1000000) 
        
        currentTime <- getCurrentTime
        
        -- Select a random movie from the list.
        randomMovieIndex <- randomRIO (0, length movies - 1)
        let movie = movies !! randomMovieIndex  -- Pick a random movie
        
        -- Create a request for the selected movie.
        let request = Request clientId movie currentTime 2 -- Example data with the selected movie
        enqueue requestQueue request
        
        -- Log the request data (clientId, movie, booking time).
        putStrLn $ "Client " ++ show clientId ++ " sent a request for movie: " ++ movie
        let orderConfirmDateTime = show currentTime  -- Use current time for confirmation
        logRequestResponse (show clientId) movie (show currentTime) orderConfirmDateTime

-- | Simulates the server processing requests from clients.
startServer :: RequestQueue Request -> MVar Response -> IO ()
startServer requestQueue responseQueue = do
    let processRequests = do
            maybeRequest <- dequeue requestQueue
            case maybeRequest of
                Just request -> do
                    -- Generate a response for the request.
                    response <- createResponse request
                    putMVar responseQueue response
                    
                    -- Log the processed response.
                    putStrLn $ "Server processed request from client " ++ show (reqClientId request)
                    let logMessage = "Server processed request for client " ++ show (reqClientId request) ++ " with response: " ++ show response
                    let orderConfirmDateTime = "2025-01-01 12:00:00"  -- Example order confirmation time
                    logRequestResponse (show (reqClientId request)) (reqMovieName request) (show (reqBookingDateTime request)) orderConfirmDateTime
                    
                    processRequests
                Nothing -> threadDelay 100000 >> processRequests
    processRequests

-- | The main entry point of the application.
main :: IO ()
main = do
    -- Create request queue and response MVar
    requestQueue <- newRequestQueue :: IO (RequestQueue Request)
    responseQueue <- newMVar undefined :: IO (MVar Response)  -- Initialize the MVar correctly
    
    -- Start the server in a separate thread
    _ <- forkIO $ startServer requestQueue responseQueue
    
    -- Start 10 client threads (each making multiple requests)
    mapM_ (\clientId -> forkIO (startClient clientId requestQueue responseQueue)) [1..numClients]
    
    -- Wait for all responses
    mapM_ (const $ takeMVar responseQueue) [1..totalRequests]  -- Wait for all responses (totalRequests)

    -- Generate the HTML report
    generateHTMLReport

    putStrLn "All requests processed. Exiting."
