module Main where

import Room (newRooms)
import Network (runServer)
import DB (connectDB, initSchema)

main :: IO ()
main = do
  conn <- connectDB
  initSchema conn
  rooms <- newRooms
  putStrLn "Server san sang: client gui JoinRoom voi ma phong rong de tao phong moi"
  runServer rooms
