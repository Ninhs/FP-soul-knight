module Main where

import Room (newRooms, createRoom)
import Network (runServer)
import DB (connectDB, initSchema)

main :: IO ()
main = do
  conn <- connectDB
  initSchema conn
  rooms <- newRooms
  code <- createRoom rooms
  putStrLn ("Phong test da tao, ma phong: " ++ code)
  runServer rooms