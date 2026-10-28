module Assets where

import Graphics.Gloss

-- Kiểu dữ liệu lưu trữ toàn bộ tài nguyên game sau khi load lên bộ nhớ
data GameAssets = GameAssets
  { heroIdleFrames :: [Picture]
  , heroWalkFrames :: [Picture]
  , orcIdleFrames  :: [Picture]
  }

-- Hàm load toàn bộ asset vào bộ nhớ
loadAssets :: IO GameAssets
loadAssets = do
  -- Load các frame idle của soldier (hero) từ thư mục assets/soldier/idle/
  -- Ví dụ load 10 frame từ hero_idle00.bmp đến hero_idle09.bmp
  hIdle <- sequence [ loadBMP ("assets/soldier/idle/hero_idle0" ++ show i ++ ".bmp") | i <- [0..9] ]
  
  -- Load các frame walk của soldier từ thư mục assets/soldier/walk/ (12 frame: 0 đến 11)
  hWalk <- sequence [ loadBMP path | i <- [0..11], let path = "assets/soldier/walk/hero_idle" ++ (if i < 10 then "0" ++ show i else show i) ++ ".bmp" ] -- (hoặc thay đường dẫn chuẩn theo tên file walk của bạn)

  -- Load idle của orc từ assets/orc/idle/ (11 frame: từ 00 đến 10)
  oIdle <- sequence [ loadBMP ("assets/orc/idle/orc_idle" ++ (if i < 10 then "0" ++ show i else show i) ++ ".bmp") | i <- [0..10] ]

  return GameAssets
    { heroIdleFrames = hIdle
    , heroWalkFrames = hIdle -- Tạm thời gán trỏ tạm nếu chưa đủ, bạn có thể thay bằng danh sách walk thực tế
    , orcIdleFrames  = oIdle
    }