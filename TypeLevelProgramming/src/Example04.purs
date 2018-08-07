module TLP.Example04 where

-- | 3. Deriving Eq and Ord

-- import Prelude

import Data.Eq
import Data.Ord


type EmailAddress = String

type PhoneNumber = String

data PersonKey
  = ByEmail EmailAddress
  | ByPhone PhoneNumber

data PersonValue = Naughty | Nice

derive instance eqPersonKey :: Eq PersonKey
derive instance ordPersonKey :: Ord PersonKey

-- How is compiler able to auto generate code?
-- Can we add more such instances to our compiler?
-- Where all does the code generation in purescript? Other than the core JS code!