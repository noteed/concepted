module Concepted.Misc where

-- | Givent a string, break it into a list of strings
-- - at each '\n',
-- - so that the total lenght of the words of each string is <= n.
chop :: Int -> String -> [String]
chop n str = concatMap (f n [] [] . words) ls
  where ls = lines str
        f _ xs ys [] = ys ++ [unwords xs]
        f n' xs ys (w:ws) = let m = max n' (length w) in
          if sum (map length xs) + length w > m
          then f m [] (ys ++ [unwords xs]) (w:ws)
          else f m (xs++[w]) ys ws

-- | Set n to its nearest multiple of t.
snap :: Int -> Double -> Double
snap t n = fromIntegral $
  let n' = floor n
      m =  n' `mod` t
  in if m > 5 then n' + t - m else n' - m

snapXY :: Int -> (Double, Double) -> (Double, Double)
snapXY t (a, b) = (snap t a, snap t b)
