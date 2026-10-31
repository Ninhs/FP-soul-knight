{-# LANGUAGE DeriveGeneric #-}
module Protocol where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Types.GameState (GameState)

-- Những gì CLIENT gửi lên server
data ClientMsg
  = JoinRoom { roomCode :: String, playerName :: String }
    -- ^ roomCode rỗng ("") = tạo phòng mới rồi vào luôn (người tạo là chủ phòng)
  | StartGame
    -- ^ chủ phòng (id nhỏ nhất) bấm bắt đầu ván, chỉ hợp lệ khi phòng đang ở Lobby
  | InputMsg { moveDir :: (Float, Float), aimDir :: (Float, Float), attack :: Bool }
  | LeaveRoom
  | AdjustGold Int
    -- ^ cộng/trừ vàng sau khi mở rương hoặc mua đồ ở shop (server kiểm tra đủ vàng khi trừ)
  | HealSelf Int
    -- ^ hồi máu cho mình (uống bình thuốc, mở rương)
  | SetReady Bool
    -- ^ bật / tắt trạng thái sẵn sàng ở lobby (chủ phòng chỉ được bắt đầu khi mọi người đã sẵn sàng)
  | BoostStats Int Float Int
    -- ^ tăng chỉ số mình sau khi mua ở shop: (sát thương, tốc chạy theo đơn vị thế giới, máu tối đa)
  deriving (Show, Generic)
instance ToJSON ClientMsg
instance FromJSON ClientMsg

-- Những gì SERVER gửi về client
data ServerMsg
  = RoomJoined { assignedId :: Int, joinedCode :: String, initialState :: GameState }
  | StateUpdate GameState
  | RoomError String
  deriving (Show, Generic)
instance ToJSON ServerMsg
instance FromJSON ServerMsg
