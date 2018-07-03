module ProtoPurescript.Sheleved.Main where

-- THIS MODULE WILL NO MORE BE IMPLEMENTED IN THIS FASHOIN AS THERE ARE TOO MANY "Unknowns" FOR ME TO BUILD IT IN THIS FASHION.

import Data.Map
import Control.Monad.Aff (Aff)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.State.Trans (StateT)
import Data.Maybe (Maybe)

{-

The goal of this small (not so small) project is to understand how the language works as lot of it we take for granted. 
We can do more if understand the typle level and runtime very well.

## Design of ProtoPurescript

# Intro 


# Scope
We will not implement all the functionalities of purescript. 
As our goal is to understand what features of purescript and Haskell are built at what level.
We will try to restrict ourseleves to items listed below.
The path we will be taking will be very much similar to how stephan delphi has taken in his series which is called "Write you a haskell".

Things we will implement:

-- Indentation sensitive grammar
-- Pattern matching
-- Algebraic datatypes
-- Where statements
-- Recursive functions/datatypes
-- Operator sections
-- Implicit let-rec
-- List and tuple sugar
-- Records
-- Custom operators
-- Do-notation
-- Type annotations
-- Monadic IO
-- Typeclasses
-- Arithmetic primops
-- Type synonyms
-- List comprehensions

Things we will not implement are:

-- Overloaded literals
-- GADTs
-- Polymorphic recursion
-- Any GHC-specific language extensions.
-- Newtypes
-- Module namespaces
-- Operator parameters
-- Defaulting rules
-- Exceptions
-- Parallelism
-- Software Transactional Memory
-- Foreign Function Interface

Now if one feels so inclined one could of course implement these features on top of our final language, 
but they are left as an exercise to the reader! We will surely discuss at the end where is it idle to build these features.

# Intermediate Forms

Compilation pipeline = Parse >>= Rename >>= Typecheck >>= Desugar >>= ToCore >>= Evaluate

Code stages = Source >>= Frontend >>= Core >>= PHOAS

** Need to add explanantion for what these things mean.

-}

data FrontendAST = FNode String
            | FChild (Array FrontendAST)

data CoreAST = CNode String
            | CChild (Array CoreAST)

newtype TypingENV = TypingENV {
                                env :: String
                              }

newtype CoreEnv = CoreEnv {
                            env :: String
                          }

newtype DataEnv = DataEnv {
                            env :: String
                          }

data Kind = Kind

newtype ClassEnv = ClassEnv {
                                classEnv :: String
                            }
newtype Flags = Flags {
                        flagA :: String
                      }

newtype ClassHier = ClassHier {
                                classHier :: String
                              }

data CompilerState = CompilerState
  {
    _fname    :: Maybe String                 -- ^ File path
  , _imports  :: Array String                 -- ^ Loaded modules
  , _src      :: Maybe String                 -- ^ Why is this L.Text and not String haskell ? -- ^ File source
  , _ast      :: Maybe FrontendAST            -- ^ What is pattern synonym ? https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms   -- ^ Frontend AST
  , _tenv     :: TypingENV                    -- ^ Typing environment
  , _kenv     :: Map String Kind              -- ^ Kind environment
  , _cenv     :: ClassEnv                     -- ^ Typeclass environment
  , _cast     :: Maybe CoreAST                -- ^ Core AST
  , _flags    :: Flags                        -- ^ Compiler flags
  , _venv     :: CoreEnv                      -- ^ Core interpreter environment
  , _denv     :: DataEnv                      -- ^ Entity dictionary
  , _clenv    :: ClassHier                    -- ^ Typeclass hierarchy
  }

-- have to check what are the equivalent in purescript
-- CompilerState
--   { 
--   , _src      :: Maybe L.Text              -- ^ File source
--   , _ast      :: Maybe Syn.Module          -- ^ Frontend AST
--   , _tenv     :: Env.Env                   -- ^ Typing environment
--   , _kenv     :: Map.Map Name Kind         -- ^ Kind environment
--   , _cenv     :: ClassEnv.ClassEnv         -- ^ Typeclass environment
--   , _cast     :: Maybe Core.Module         -- ^ Core AST
--   , _flags    :: Flags.Flags               -- ^ Compiler flags
--   , _venv     :: CoreEval.ValEnv Core.Expr -- ^ Core interpreter environment
--   , _denv     :: DataEnv.DataEnv           -- ^ Entity dictionary
--   , _clenv    :: ClassEnv.ClassHier        -- ^ Typeclass hierarchy
--   }

-- We have to change the Aff to Eff
type CompilerMonad a e = ExceptT String (StateT CompilerState (Aff e)) a

-- parseP   :: FilePath -> L.Text -> CompilerM Syn.Module
-- dataP    :: Syn.Module -> CompilerM Syn.Module
-- groupP   :: Syn.Module -> CompilerM Syn.Module
-- renameP  :: Syn.Module -> CompilerM Syn.Module
-- desugarP :: Syn.Module -> CompilerM Syn.Module
-- inferP   :: Syn.Module -> CompilerM Core.Module
-- evalP    :: Core.Module -> CompilerM ()






