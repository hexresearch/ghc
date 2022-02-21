{-# LANGUAGE BangPatterns #-}

module T16040 where

-- A type to take the place of state
data X a = X { runX :: !a }

test1 :: Int -> Int
test1 = \(!i) -> go i where
    go = \(!i) -> if i > 0
        then go $! i - 1
        else i
{-# NOINLINE test1 #-}

-- | Like 'test1', this function's result should have the CPR property and be
-- unboxed. At the moment it isn't, because we are conservative and avoid
-- reboxing in case of `test2 (I# 0)`, which means boxing up `i-1` in the
-- recursive case of `go`.
test2 :: Int -> Int
test2 = \(!i) -> runX (go i) where
    go = \(!i) -> if i > 0
        then go $! i - 1
        else X i
{-# NOINLINE test2 #-}
