module TLP.Example02 where

import Prelude

data SomeType = A String | B String

main :: SomeType -> String
main (A s) = identity s
main (B s) = identity s


main' sometype = case sometype of 
        (A s) -> identity s
        (B s) -> identity s

someFunction = main (A "5")

foo x s = show x <> s

bar y = y <> foo

-- expr = caseS | functionApplcation | expr | literal
-- functionApplcation = functionName expr 
-- literal = string | number | 
-- string = "[char]*"
-- number = /d+(./d+)?


-- main 
-- -- case expression type evaluation

-- someFunction



-- foo

-- bar

-- class C a b | a -> b, b -> a where
--    transform :: a -> b 

-- instance  aIntString :: C Int String where
--         transform = show

-- else instance aIntToNumber :: C Int Number where
--         transform x = 1.0

-- instance aStringString :: C String String where
--         transform x = x

-- else instance aaString :: Show a => C a String where
--         -- transform :: a -> String
--         transform = show



-- -- -- callTransform :: forall a b. C a b => b
-- -- callTransform = transform 


-- -- -- callcallTransform :: forall b. b
-- -- callcallTransform = callTransform

-- -- -- callingFun :: forall a b. C a b => Show b => String
-- -- -- callingFun = let x = callTransform 
-- -- --         in show x

