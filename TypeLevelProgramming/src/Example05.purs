module TLP.Example05 where

-- | 4. Newtype Deriving
    -- If you've used Haskell before, you might be familiar with its GeneralizedNewtypeDeriving language extension. 
        -- Well, now that feature is available in PureScript too!
        -- What is this haskell feature?

-- What is the naming convetion purescript imports?
import Data.Eq
import Data.Ord
import Data.Functor

-- What is control here? What is the convention here?
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.State.Trans (StateT)
import Control.Apply
import Control.Applicative
import Control.Bind
import Control.Monad

-- What is the need of class here? Convetion here?
import Control.Monad.Error.Class
import Control.Monad.State.Class

-- Why is not part of the control?
import Effect
import Effect.Class

newtype EmailAddress = EmailAddress String

newtype PhoneNumber = PhoneNumber String

newtype SSN = SSN String


-- There are three different ways of defining instances for Eq of newtype.
-- | 1 . First verbose method
-- instance eqEmailAddress :: Eq EmailAddress where
--   eq (EmailAddress s1) (EmailAddress s2) = eq s1 s2
  
-- instance eqPhoneNumber :: Eq PhoneNumber where
--   eq (PhoneNumber s1) (PhoneNumber s2) = eq s1 s2
  
-- instance eqSSN :: Eq SSN where
--   eq (SSN s1) (SSN s2) = eq s1 s2

-- | 2. This seems to be there only for Eq and Ord class
derive instance eqEmailAddress :: Eq EmailAddress
derive instance eqPhoneNumber :: Eq PhoneNumber
derive instance eqSSN :: Eq SSN

-- | 3. This seems to there only for newtype. But you can use to declare any newtype instnace.
-- derive newtype instance eqEmailAddress :: Eq EmailAddress
-- derive newtype instance eqPhoneNumber :: Eq PhoneNumber
-- derive newtype instance eqSSN :: Eq SSN

newtype App a = App (StateT Int (ExceptT String (Effect)) a)


-- No need to actually derive the entire instance. What is the code generated for this?
derive newtype instance functorApp :: Functor App
derive newtype instance applyApp :: Apply App
derive newtype instance applicativeApp :: Applicative App
derive newtype instance bindApp :: Bind App
derive newtype instance monadApp :: Monad App

derive newtype instance monadEffApp :: MonadEffect (App)
derive newtype instance monadStateApp :: MonadState Int (App)
derive newtype instance monadErrorApp :: MonadError String (App)

-- What next?
