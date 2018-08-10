module TLP.Example09 where

import Prelude ((<>), (+))

-- | 10. Functional Dependencies

-- Implementing the type class with single parameter.
data Arithmatic a = Arithmatic (a -> a -> a)  (a -> a -> a) (a -> a -> a)

addAnything (Arithmatic a l r) = a
getLeftAnything (Arithmatic a l r) = l
getRightAnything (Arithmatic a l r) = r

-- Implementing the type class.
-- These are the things that you want 
getLeftString :: String -> String -> String
getLeftString left right = left

getRightString :: String -> String -> String
getRightString left right = right

appendString :: String -> String -> String
appendString left right = left <> right

arithmaticString :: Arithmatic String
arithmaticString = Arithmatic appendString getLeftString getRightString

getLeftInt :: Int -> Int -> Int
getLeftInt left right = left

getRightInt :: Int -> Int -> Int
getRightInt left right = right

addInt :: Int -> Int -> Int
addInt left right = left + right

arithmaticInt :: Arithmatic Int
arithmaticInt = Arithmatic addInt getLeftInt getRightInt

getLeftArray :: forall a. Array a -> Array a -> Array a
getLeftArray left right = left

getRightArray :: forall a. Array a -> Array a -> Array a
getRightArray left right = right

appendArray :: forall a. Array a -> Array a -> Array a
appendArray left right = right

arithmaticArray :: forall a. Arithmatic (Array a)
arithmaticArray = Arithmatic appendArray getLeftArray getRightArray

-- Note in the following functions addAnything and the parameter part of the code is the same
-- What is changing is arithmaticString, arithmaticInt, arithmaticArray.
-- One thing is that during compile time you can always decide what arithmatic* should be
-- Another thing is during runtime if are more than one functction calling this function and you just have put some contraint. 
-- In that case the compiler will go find the defintions of all of them.

-- What happens after this?
    -- Somehow during the runtime this map of functions has to pass and the right "Arithmatic" has to be used when the type of the function is generic.
        -- How will the passing work?

testStringAdd x y = addAnything arithmaticString x y
testIntAdd x y = addAnything arithmaticInt x y
testArrayAdd x y = addAnything arithmaticArray x y

testStringLeft x y = getLeftAnything arithmaticString x y
testIntLeft x y = getLeftAnything arithmaticInt x y
testArrayLeft x y = getLeftAnything arithmaticArray x y

testStringRight x y = getRightAnything arithmaticString x y
testIntRight x y = getRightAnything arithmaticInt x y
testArrayRight x y = getRightAnything arithmaticArray x y

-- typeclasses with no argument
    -- Prim Partial
-- typeclasses with one argument
    -- Eq, Ord etc
-- typeclasses with multiple argument
    -- functional dependecies came into picture

-- When do you create a typeClass
-- Does type class bring together all the "things" which needs to have 
-- the same functionality or does it just enforce some restriction.

-- | MultiParameter functional dependecy (Its implementation).
-- How different will it be from the above implementation?


-- what is the meaning of this sentence?
    -- The Parallel class represents a relationship between a monad and an applicative functor, 
    -- such that we can use the applicative functor to execute commands in parallel.

-- Can we do things without functional dependency? Functional dependency is saying you cannot have one more of the same a->b/a->c simultaneously.

-- Functional dependency.
-- Examples of its use.
-- What next?



