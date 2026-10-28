module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event)
import Assets
import System.IO (stdout, hSetEncoding, utf8)
type TestState = Float
main :: IO ()
main = do
    hSetEncoding stdout utf8
    putStrLn "Đang load assets..."
    assets <- loadAssets
    putStrLn "Load assets thành công! Khởi động game..."
    
    let window = InWindow "Soul Knight Haskell - Test" (800, 600) (100, 100)
        bgColor = black
        fps = 60
        initialState = 0.0

    -- Hàm play của Gloss tạo vòng lặp game cơ bản
    play window bgColor fps initialState (render assets) handleInput update

-- Hàm Render: Vẽ nhân vật ra màn hình dựa theo thời gian
render :: GameAssets -> TestState -> Picture
render assets time = 
    let frames = soldierIdle assets -- Lấy bộ ảnh Idle của Soldier làm test
        numFrames = length frames
        -- Tính toán index của frame: tốc độ 10 hình/giây
        frameIndex = floor (time * 10) `mod` numFrames
        currentFrame = frames !! frameIndex
    -- Phóng to nhân vật lên 3 lần để nhìn cho rõ
    in scale 3.0 3.0 currentFrame

-- Hàm bắt sự kiện bàn phím (Tạm thời bỏ qua)
handleInput :: Event -> TestState -> TestState
handleInput _ state = state

-- Hàm cập nhật logic game mỗi khung hình (Cộng dồn thời gian)
update :: Float -> TestState -> TestState
update dt time = time + dt