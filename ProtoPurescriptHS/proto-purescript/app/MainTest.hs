{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MainTest where

import Lib

main :: IO ()
main = someFunc

data Nat = Zero | Succ Nat

three = Succ (Succ (Succ Zero))

evenN :: Nat -> Bool
evenN Zero = True
evenN (Succ n) = oddN n

oddN :: Nat -> Bool
oddN Zero = False
oddN (Succ n) = evenN n

-- -------------------------- ----==========================-------------------------------------------------\ \---

data Zero
data Succ e

type Three = Succ (Succ (Succ Zero))
type Four = Succ (Succ (Succ (Succ Zero)))

-- V1 -- how its internal implemtation looks like?
-- class Even n where isEven :: n
-- class Odd n where isOdd :: n


-- instance Even Zero
-- instance Odd n => Even (Succ n) 
-- instance Even n => Odd (Succ n)

data True
data False

-- V2 -- how its internal implemtation looks like?
-- class Even n b where evenT :: n -> b
-- class Odd n b where oddT :: n -> b

-- instance            Even Zero True
-- instance Odd n b => Even (Succ n) b
-- instance            Odd Zero False
-- instance Even n b => Odd (Succ n) b

-- V3
class Even n b | n -> b where evenT :: n -> b
class Odd n b | n -> b where oddT :: n -> b

instance            Even Zero True
instance Odd n b => Even (Succ n) b
instance            Odd Zero False
instance Even n b => Odd (Succ n) b


data V1 p
data U1 p = U1

-- What is the right way of declaring this variables
-- data (f :+: g) p = L1 (f p) | R1 (g p)
-- data (f :*: g) p = (f p) :*: (g p)




-- ^ Flow of our compiler code execution 
-- modl :: FilePath -> L.Text -> CompilerM ()
-- modl fname
--     = parseP fname
--   >=> dataP
--   >=> groupP
--   >=> renameP
--   >=> desugarP
--   >=> inferP
--   >=> evalP

-- parseP   :: FilePath -> L.Text -> CompilerM Syn.Module
-- dataP    :: Syn.Module -> CompilerM Syn.Module
-- groupP   :: Syn.Module -> CompilerM Syn.Module
-- renameP  :: Syn.Module -> CompilerM Syn.Module
-- desugarP :: Syn.Module -> CompilerM Syn.Module
-- inferP   :: Syn.Module -> CompilerM Core.Module
-- evalP    :: Core.Module -> CompilerM ()

-- ^ We also need a debugging system as we need to figure out where the error is. This need not be very robust.
-- ** The implementation of the interactive shell will use a custom library called repline , which is a higher-level wrapper on top of haskeline made to be more pleasant when writing interactive shells


-- Parser 
    -- parsec library
    -- support for adding infix operator later

-- Renamer 
    -- rename variables to ensure there is no name shadowing

-- Datatypes
    -- User defined data declarations need to be handled and added to the typing context so that their use throughout the program logic can be typechecked. This will also lead us into the construction of a simple kind inference system, and the support of higher-kinded types.
    -- data Bool = False | True
    -- data Maybe a = Nothing | Just a
    -- data T1 f a = T1 (f a)
    -- Each constructor definition will also introduce several constructor functions into the Core representation of the module. Record types will also be supported and will expand out into selectors for each of the various fields.

-- Desugaring
    -- Pattern matching
    -- Multiple Equations
    -- Constructor Patterns
    -- Nested Patterns

{-

There are many edge cases of pattern matching that we will have to consider. The confluence of all them gives rise to a rather complex set of AST rewrites:

Multiple arguments
Overlapping patterns
Literal patterns
Nested patterns
Non-exhaustive equations
Conditional equations
Non-linear patterns

On top of pattern matching we will implement the following more trivial syntactic sugar translations:

Expand if/then statements into case expressions.
Expand pattern guards into case expressions.
Expand out do-notation for monads.
Expand list syntactic sugar.
Expand tuple syntactic sugar.
Expand out operator sections.
Expand out string literals.
Expand out numeric literals.
We will however punt on an important part of the Haskell specification, namely overloaded literals. In Haskell numeric literals are replaced by specific functions from the Num or Fractional typeclasses.

-- Frontend
42 :: Num a => a
3.14 :: Fractional a => a

-- Desugared
fromInteger (42 :: Integer)
fromRational (3.14 :: Rational)
We will not implement this, as it drastically expands the desugarer scope.

We will however follow GHC's example in manifesting unboxed types as first class values in the language so literals that appear in the AST are rewritten in terms of the wired-in constructors (Int#, Char#, Addr#, etc).

I# : Int# -> Int
C# : Char# -> Char
> :core 1
I# 1#
> :core 1 + 2
plus (I# 1#) (I# 2#)
> :core "snazzleberry"
unpackCString# "snazzleberry"#


-}
-- Core
-- Type Classes
-- Type Checker
-- Interpretes
-- Error Reporitng