module List where

import           Control.Arrow (first)


partitionI ∷ Int → Int → [(Int, Int)]
partitionI 1 l = [(0, l)]
partitionI n l = (0, eaten) : (map (first (+eaten)) $ partitionI (n-1) (l-eaten))
  where eaten = l `div` n

slice ∷ [a] → Int → Int → [a]
slice xs at count = take count $ drop at xs

partitionN ∷ Int → [a] → [[a]]
partitionN n xs = (uncurry (slice xs)) <$> partitionI n (length xs)
