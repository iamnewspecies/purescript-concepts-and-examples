module CreateYourOwnMonad where

-- it needs an instance of functor
-- it needs in instance of applicative
-- after this it needs bind and pure

-- > :k Array
-- Type -> Type
-- > :k Fail
-- Symbol -> Type
-- > :k (->)
-- Type -> Type -> Type