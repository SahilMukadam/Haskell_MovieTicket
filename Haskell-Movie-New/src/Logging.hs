{-|
Module      : Logging
Description : This module provides logging functionality for a movie ticket booking system.
Maintainer  : Your Name <your.email@example.com>
Stability   : experimental
Portability : portable

The `Logging` module is designed to manage the logging of booking requests and responses in the movie ticket booking system. It stores logs of client requests and generates an HTML report that includes both a table and a graphical representation of booking data.

This module contains the following functions:
- `logRequestResponse`: Logs the details of each booking request and its response.
- `getRequestData`: Retrieves all collected log data.
- `generateHTMLReport`: Generates an HTML report that includes a table and a bar chart of booking statistics.

-}

module Logging (logRequestResponse, getRequestData, generateHTMLReport) where

import System.IO
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import System.Directory (getCurrentDirectory)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (forkIO, threadDelay)

type LogData = [(String, String, String, String)]  -- (ClientID, Movie Name, Date & Time, Order Confirmation Date & Time)

-- | A mutable global variable used to store the log data. 
logStore :: IORef LogData
logStore = unsafePerformIO (newIORef [])

-- | Logs the details of a movie booking request into a log file.
logRequestResponse :: String -> String -> String -> String -> IO ()
logRequestResponse clientID movieName dateTime orderConfirmDateTime = do
    -- Get the current system time and convert it to local time
    currentTime <- getCurrentTime
    timezone <- getCurrentTimeZone
    let localTime = utcToLocalTime timezone currentTime
        timeString = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime

    -- Construct the log message
    let logMessage = timeString ++ " | ClientID: " ++ clientID
                    ++ " | Movie: " ++ movieName
                    ++ " | Date & Time: " ++ dateTime
                    ++ " | Order Confirmation Date & Time: " ++ orderConfirmDateTime
                    ++ "\n"
    
    -- Get the current directory to save the log file
    currentDir <- getCurrentDirectory

    -- Update the log store with the new log entry
    modifyIORef' logStore ((clientID, movieName, dateTime, orderConfirmDateTime):)

    -- Asynchronously write the log message to the log file in the background
    _ <- forkIO $ do
        let logFilePath = currentDir ++ "/request.log"
        threadDelay 100000  -- Delay to allow for async processing
        withFile logFilePath AppendMode $ \handle -> do
            hPutStr handle logMessage
    return ()

-- | Retrieves the collected log data stored in the `logStore` variable.
getRequestData :: IO LogData
getRequestData = readIORef logStore

-- | Generates an HTML report that includes a table of all booking requests and a bar chart visualizing the number of bookings per movie.
generateHTMLReport :: IO ()
generateHTMLReport = do
    -- Get the current directory to save the HTML report
    currentDir <- getCurrentDirectory
    let htmlFileName = currentDir ++ "/BookingDashboardWithGraph.html"
    
    -- Retrieve the logged request data
    logData <- getRequestData

    -- Process the log data to count the number of bookings per movie
    let movieCounts = foldr (\(_, movieName, _, _) acc -> 
                                case lookup movieName acc of
                                    Just count -> (movieName, count + 1) : filter ((/= movieName) . fst) acc
                                    Nothing -> (movieName, 1) : acc) [] logData
    let movieLabels = map fst movieCounts
    let movieValues = map snd movieCounts

    -- Construct the HTML content for the report
    let htmlHeader = "<html><head><title>Movie Ticket Booking Dashboard</title>" ++
                     "<style>body { font-family: Arial, sans-serif; margin: 20px; }" ++
                     "table { border-collapse: collapse; width: 100%; margin-bottom: 30px; }" ++
                     "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }" ++
                     "th { background-color: #f4f4f4; cursor: pointer; }</style>" ++
                     "<script src='https://cdn.jsdelivr.net/npm/chart.js'></script>" ++
                     "<p></p>" ++
                     "<script>" ++
                     "function sortTable(n) {" ++
                     "  var table, rows, switching, i, x, y, shouldSwitch, dir, switchcount = 0;" ++
                     "  table = document.getElementById('bookingTable');" ++
                     "  switching = true; dir = 'asc';" ++
                     "  while (switching) {" ++
                     "    switching = false; rows = table.rows;" ++
                     "    for (i = 1; i < (rows.length - 1); i++) {" ++
                     "      shouldSwitch = false;" ++
                     "      x = rows[i].getElementsByTagName('TD')[n];" ++
                     "      y = rows[i + 1].getElementsByTagName('TD')[n];" ++
                     "      if (dir == 'asc') {" ++
                     "        if (x.innerHTML.toLowerCase() > y.innerHTML.toLowerCase()) {" ++
                     "          shouldSwitch = true; break;" ++
                     "        }" ++
                     "      } else if (dir == 'desc') {" ++
                     "        if (x.innerHTML.toLowerCase() < y.innerHTML.toLowerCase()) {" ++
                     "          shouldSwitch = true; break;" ++
                     "        }" ++
                     "      }" ++
                     "    }" ++
                     "    if (shouldSwitch) {" ++
                     "      rows[i].parentNode.insertBefore(rows[i + 1], rows[i]);" ++
                     "      switching = true; switchcount++;" ++
                     "    } else {" ++
                     "      if (switchcount == 0 && dir == 'asc') {" ++
                     "        dir = 'desc'; switching = true;" ++
                     "      }" ++
                     "    }" ++
                     "  }" ++
                     "}</script>" ++
                     "</head><body>" ++
                     "<h1>Movie Ticket Booking Dashboard</h1>"

    let htmlTable = "<p>Table Representation</p>" ++
                    "<table id='bookingTable'>" ++
                    "<tr><th onclick='sortTable(0)'>ClientID</th>" ++
                    "<th onclick='sortTable(1)'>Movie Name</th>" ++
                    "<th onclick='sortTable(2)'>Date & Time</th>" ++
                    "<th onclick='sortTable(3)'>Order Confirmation Date & Time</th></tr>" ++
                    concatMap (\(clientID, movieName, dateTime, orderConfirmDateTime) -> 
                                "<tr><td>" ++ clientID ++ "</td><td>" ++ movieName
                                ++ "</td><td>" ++ dateTime ++ "</td><td>" ++ orderConfirmDateTime ++ "</td></tr>") logData ++
                    "</table>"

    let htmlGraph = "<canvas id='movieChart' width='400' height='200'></canvas>" ++
                    "<script>" ++
                    "const ctx = document.getElementById('movieChart').getContext('2d');" ++
                    "const chart = new Chart(ctx, {" ++
                    "  type: 'bar'," ++
                    "  data: {" ++
                    "    labels: " ++ show movieLabels ++ "," ++
                    "    datasets: [{" ++
                    "      label: 'Number of Bookings'," ++
                    "      data: " ++ show movieValues ++ "," ++
                    "      backgroundColor: 'rgba(75, 89, 192, 0.2)'," ++
                    "      borderColor: 'rgba(75, 89, 192, 1)'," ++
                    "      borderWidth: 1" ++
                    "    }]" ++
                    "  }" ++
                    "});" ++
                    "</script></body></html>"

    -- Write the generated HTML report to a file
    writeFile htmlFileName (htmlHeader ++ htmlGraph ++ htmlTable)
