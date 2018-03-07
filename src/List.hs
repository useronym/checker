module List where

import Data.List (partition)


partitionN ∷ Int → [a] → [[a]]
partitionN n xs = let xs' = zip [0..] xs in
                    map (\i → map snd $ filter (\(i', _) → i' `mod` n == i) xs')
                        [0..(n-1)]
