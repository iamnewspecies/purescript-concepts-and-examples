module TLP.Example07 where

-- | 7. Anonymous Function Arguments
-- Up until 7 is about sytactic sugar which reduce you code.

import Prelude
import Data.Array (filter)
import Data.Maybe


-- | f and g are the same 
f = filter (\x -> x < 10)
g = filter (_ < 10)

-- | record accessor
h = map (\x -> x.name)
i = map _.name

-- NOTE ::  we can use the parameter only once
-- here it automatically understands that name is the first parameter and age is the second parameter
j = (\name age -> { name: name, age: age })
k = {name : _, age : _}

-- updating a record
l = map (\o -> o { enabled = true })
m = map _ {enabled = true}

-- need to understand how this works
-- how do lenses work?
-- enabledLens = lens _.enabled (_ { enabled = _ })

-- if statement anonymous
n = if _ then "yes" else "no"

-- case statement anonymous
o default = case _ of
        Just x -> x
        Nothing -> default

-- What next?