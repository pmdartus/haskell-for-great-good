-- Modules
-- http://learnyouahaskell.com/modules

-- Loading
-- =======

-- Can be imported in the ghci using :m + <module> syntax
-- Use () to import specific functions
-- Use hiding to avoid import some functions
-- Use qualified to access the function using the module name as prefix
-- Use qualified as to rename the module name

-- Data.Map
-- ========

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
-- findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
-- findKey key [] = Nothing
-- findKey key ((k,v):xs) = if key == k
--   then Just v
--   else findKey key xs
findKey key = foldr (\(k, v) acc -> if key == k then Just v else acc ) Nothing