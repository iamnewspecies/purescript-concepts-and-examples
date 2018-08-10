module TLP.Example06 where

-- | Three very imporatant goals
-- It seems that there is a need for auto geenration of code.
-- A smaller representation of code.
-- A more readable code.

import Data.Maybe
import Data.Newtype

import Data.Foldable (find)
-- | 5. Data.Newtype Deriving

-- Not very clear!!!
-- https://pursuit.purescript.org/packages/purescript-newtype/3.0.0/docs/Data.Newtype#v:overF

-- There is one more generic deriving instance.

-- class Newtype new old | new -> old where
--   wrap :: old -> new
--   unwrap :: new -> old

newtype EmailAddress = EmailAddress String

-- instance showEmailAddress :: Show EmailAddress where
--   show (EmailAddress s) = "(EmailAddress " <> show s <> ")"
  
-- Why is the underscore required?
-- Is it to indicate that code has to be auto generated here?
derive instance newtypeEmailAddress :: Newtype EmailAddress _

toUpper :: String -> String
toUpper s = s


-- | over
-- type signature of over is as follows
-- over :: forall t a s b. Newtype t a => Newtype s b => (a -> t) -> (a -> b) -> t -> s
upperEmail :: EmailAddress -> EmailAddress
upperEmail = over EmailAddress toUpper

-- | overF 
-- what is the meaning of the follwing function?
-- what is byDomain doing?
-- type signature of overF is as follows
-- overF :: forall f g t a s b. Functor f => Functor g => Newtype t a => Newtype s b => (a -> t) -> (f a -> g b) -> f t -> g s
-- not clear yet!
-- byDomain :: String -> Array EmailAddress -> Maybe EmailAddress
-- byDomain domain = overF EmailAddress (find (contains (wrap domain)))

-- What next?