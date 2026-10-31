module Main where

import System.IO (BufferMode(LineBuffering), hSetBuffering, stderr, stdout)

import Room (newRooms)
import Network (runServer)
import DB (initSchema)

main :: IO ()
main = do
  -- Render ghi log qua pipe: không đặt LineBuffering thì chữ nằm trong bộ đệm, không hiện ra
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  initSchema
  rooms <- newRooms
  putStrLn "Server san sang: client gui JoinRoom voi ma phong rong de tao phong moi"
  runServer rooms
