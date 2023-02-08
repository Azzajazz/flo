module Utils (
    liftFst,
    mapFst,
    flist
) where

liftFst :: (a -> b) -> (a, c) -> (b, c)
liftFst f (x, y) = (f x, y)

mapFst :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFst f = map (liftFst f)

flist :: [(a -> b)] -> a -> [b]
flist fs x = map (\f -> f x) fs