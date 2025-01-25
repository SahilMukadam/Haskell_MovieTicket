{-|
Module      : Booking
Description : This module defines the data types and functions for managing a movie booking system.
Maintainer  : Your Name <your.email@example.com>
Stability   : experimental
Portability : portable

The `Booking` module provides the essential data types and a function for handling booking requests and generating responses in a movie ticket booking system.

This module contains:
- `Request` data type to represent a booking request.
- `Response` data type to represent a server's response after processing a booking request.
- `createResponse` function to generate a `Response` based on a given `Request`.

-}

module Booking (Request(..), Response(..), createResponse) where

import Data.Time (UTCTime, getCurrentTime)

data Request = Request
    { reqClientId     :: Int       -- ^ The unique ID for the client making the request
    , reqMovieName    :: String    -- ^ The name of the movie the client wants to book tickets for
    , reqBookingDateTime :: UTCTime -- ^ The time when the booking request is made
    , reqSeatsBooked  :: Int       -- ^ The number of seats the client wants to book
    } deriving (Show)

data Response = Response
    { resClientId        :: Int    -- ^ The client ID from the request
    , resConfirmationMsg :: String -- ^ A confirmation message about the booking
    , resResponseDateTime :: UTCTime -- ^ The time when the server responded to the request
    } deriving (Show)

createResponse :: Request -> IO Response
createResponse request = do
    -- Get the current time
    currentTime <- getCurrentTime
    -- Create a confirmation message
    let message = "Booking confirmed for " ++ show (reqSeatsBooked request)
                  ++ " seats to watch \"" ++ reqMovieName request ++ "\""
    -- Return the response with the relevant details
    return Response
        { resClientId = reqClientId request
        , resConfirmationMsg = message
        , resResponseDateTime = currentTime
        }
