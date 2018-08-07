module TLP.Example03 where 

-- | 1. Introduction and Extended Infix Operators

-- Data.Map library is deprecated.
-- import Data.Map (Map, union)
-- Why?

-- Purescript effects are completely removed
-- purescript --run is a replacement for that.

import Data.Array (union)

-- is show defined on the arrays by default?
-- What things have a default show instance defined on them?

-- There is Data.List and Data.Sequence which is better than Data.Array.
  -- In what cases.
  -- What is generally good to use?

-- infix of writing things makes them more readable.
-- Is the world infix? Prefix is same as calling function with parameters.
-- Is there a need for postfix?

allGifts :: Array Int
allGifts = list1 `union` list2
  where
    list1 = [1,2,3,4,5]
    list2 = [9,8,7,6,5]

