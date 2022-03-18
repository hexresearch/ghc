module T21257 (g, g2) where

f :: (Bool, Bool) -> (Bool, Bool)
f pr = (pr `seq` True, case pr of (a,b) -> a && b)
{-# NOINLINE f #-}

-- The demand signature of `g` should say `LP(ML,ML)`, e.g., that the pair
-- components are used at most once.
g :: (Bool, Bool) -> ()
g pr = f pr `seq` ()

-- | Key point: Unlike for `seq`, repeated apps of `seq'` can't be simplified
-- away.
seq' :: a -> b -> b
seq' = seq
{-# NOINLINE seq' #-}

f2 :: (Bool, Bool) -> ()
f2 pr = pr `seq'` fst pr `seq'` ()
{-# NOINLINE f2 #-}

-- The demand signature of `g2` should say `SP(1L,A)` (prior to worker/wrapper),
-- e.g., that the pair components are used at most once.
g2 :: (Bool, Bool) -> ()
g2 pr = f2 pr `seq` ()
