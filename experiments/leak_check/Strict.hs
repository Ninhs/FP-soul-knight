import qualified Data.Map.Strict as M
import Data.List (foldl')
import Control.Monad (foldM)
type CD = M.Map Int Float
-- mô phỏng applyInputs: foldl' trên (state, cooldowns) như Control.hs, 1 người chơi KHÔNG tấn công
applyInputs :: Float -> Bool -> CD -> Int -> (Int, CD)
applyInputs dt attacking cds0 s0 = foldl' step (s0, cds0) [1]
  where
    step (s, cds) pid =
      let cd = max 0 (M.findWithDefault 0 pid cds - dt)
          (s', cd') = if not attacking || cd > 0 then (s, cd) else (s + 1, 0.5)
      in (s', M.insert pid cd' cds)
main :: IO ()
main = do
  let go :: Int -> CD -> IO CD
      go 0 c = pure c
      go n c = do
        let (_, c') = applyInputs 0.016 False c 0
        go (n - 1) c'
  r <- go 400000 M.empty
  print (M.toList r)
