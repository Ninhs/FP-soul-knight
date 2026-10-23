module Main where

import Room (newRooms, createRoom)
import Network (runServer)

main :: IO ()
main = do
  rooms <- newRooms
  code <- createRoom rooms
  putStrLn ("Phong test da tao, ma phong: " ++ code)
  runServer rooms