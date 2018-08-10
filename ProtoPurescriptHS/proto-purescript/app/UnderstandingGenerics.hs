module UnderstandingGenerics where

-- | Example of sum type
data Pastry
    = Turnover
    | Macaroon
    | Brownie
    | Cookie

-- | Example of product type
data Person = Person
  { firstName       :: String
  , lastName        :: String
  , age             :: Int
  , height          :: Float
  , phoneNumber     :: String
  , flavor          :: String
  }

-- | All the sum type cannot exist simultaneously
-- | Product exist together to form one higher level type

-- | All data types can be presented as SUMS of PRODUCTS

-- | Evaluation of expressions
data Expr
    = Add { l :: Expr, r :: Expr }
    | Mul { l :: Expr, r :: Expr }
    | Sub { l :: Expr, r :: Expr }
    | Div { l :: Expr, r :: Expr }
    | Number { val :: Int }

-- | Elaboration expands out all record selectors into toplevel functions which extract the named fields of a product.
data Point a b = Point { x :: a, y :: b }

-- Above code is converted to this for lookups
-- x :: Point a b -> a
-- x (Point a _) = a

-- y :: Point a b -> b
-- y (Point _ b) = b



-- | Compiler hooks.
-- | Here you can also add type level constraint
class Generic a where
    type Rep a :: * -> *
    from :: a -> (Rep a) x
    to :: (Rep a) x -> a

-- | All the core representaion needed
data    V1        p                       -- Empty
data    U1        p = U1                  -- ()
data    (:+:) f g p = L1 (f p) | R1 (g p) -- Sum
data    (:*:) f g p = (f p) :*: (g p)     -- Product
-- Why are all the p required?

-- ** ? What is so diferrent about these ? and where are these used ?
newtype K1    i c p = K1 { unK1 :: c }    -- a container for a c 
-- What is the meaining of this ? And what gets converted to this?
-- To represent the structure of our datatype we need to set up several datatypes to encode, sums, products, empty branches and various metadata about the names of fields, constructors and their types

newtype M1  i t f p = M1 { unM1 :: f p }  -- metadata wrapper 
-- What is the meaining of this ? And what gets converted to this?
-- To represent the structure of our datatype we need to set up several datatypes to encode, sums, products, empty branches and various metadata about the names of fields, constructors and their types


-- | An example
data Ingredient
    = Flour
    | Sugar

-- | We have to define a generic instance for this.

-- | 1 -- start
data T_Ingredient
data C_Flour
data C_Sugar

-- | 2
instance Generic Ingredient where
    type Rep Ingredient = M1 D (T_Ingredient ((M1 C (C_Flour U1)) :+: (M1 C (C_Sugar U1))))

-- Structural polymorphims in generic Haskell

-- Static typing