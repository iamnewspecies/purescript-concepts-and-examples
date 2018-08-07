module Example03 where 

-- Data.Map library is deprecated.
-- import Data.Map (Map, union)
-- Why?

-- Purescript effects are completely removed
-- purescript --run is a replacement for that.

import Data.Array (union)

allGifts :: Array Int
allGifts = list1 `union` list2
  where
    list1 = [1,2,3,4,5]
    list2 = [9,8,7,6,5]

