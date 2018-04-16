module List where


partitionN ∷ Int → [a] → [[a]]
partitionN n xs =
  let xs' = zip [0..] xs in
    map (\i → [p | (i', p) ← xs'
                 , i' `mod` n == i])
      [0..(n-1)]
