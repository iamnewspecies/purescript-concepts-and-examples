Equivalent of https://github.com/sdiehl/write-you-a-haskell for purescript.

- Why do we go functional?
  -- The world is about 
  -- All machines are about some action on some object. 
  -- If you are still not convinced then read this following book "Wholeness and implicate order" by David Bohm. (You need to read the first three chapters to understand the philosophy of our domain)[0][https://www.amazon.in/Wholeness-Implicate-Order-Routledge-Classics/dp/0415289793/ref=sr_1_2?ie=UTF8&qid=1530621732&sr=8-2&keywords=david+bohm] The idea that this book is trying to talk about is... at least my interpretation in short... There are infinite objects that exist in this world and we need to somehow look at these things in a very unified way otherwise there is an explosion of concepts which is very difficult to keep track of. 
  You can basically start asking yourself why a screen "sitting" on a table has to be seen differntly as compared to you sitting on a chair. Sure, you would say that doing this reduces the beauty and history of language but I hope that is not what we are trying to acomplish in when we write code. Fundamentally you are tring to transform something into something else with the help of some "process". "_ -> _" -- Anything can come in place of "_". This automatically pushes you to think that almost everything has some similarity. You try to find patterns. So basically funcational way of thinking pushes you to think in this fashion "_ -> _" and all other languages push you into thinking this " very_important __ very_important". You can always argue that other forms of writing code can also accomplish what functional programming langauges accomplish. But it is that one simple way of doing things which generates the most ideal patterns.

--  What is the natural outcome of this philosophy?
  -- If you agree with the philosophy then the obvious outcome is we should focus on functions. We push this philosophy so far that we see the most fundametal thing as some function. This brings into picture lamda calculus.
  -- "lamda calculus" is the assembly language of all functional programming language.
  -- Basically you can transform every programming language into some very basic "simple" primitives. Lamda calculus is theory about these very basic "simple" primitives.

- We enter this functional programming world at the very leaf level (assuming the concepts are in some tree structure). This makes it overwhelming for us to get a grip on what we are building. We will end up building these things in a very hacky way. Functional programming on the other hand is all about very few "simple" concepts. Everything is built over it for convinience. You can say that it is the same case with every programming language. The difference is in what path was taken at the core of every programming language.
  
Enough philosophy now the concepts.

- You can find the disgn discussion of GHC here [1][http://aosabook.org/en/ghc.html]

- What is lamda calculus?
  -- what makes lamda calculus?
    Valid λ-calculus expressions can be defined inductively as follows:
    -- A variable x is a valid λ-term.
    -- If t is a valid λ-term and x is a variable, then λx. t is a valid λ-term.
    -- If t and s are both valid λ-terms, then t s is a valid λ-term.
  
  -- What are the operations that are defined in lamda calculus.
    -- application (that is all). Which is the tird statement of the above mentioned laws. This operation is left associtive.
    -- Every lamda function has just one paramter.
    -- (TODO :: Need to add few examples here)

  -- After this we need to understand "Combinators".
    What are combinators?
    Why do we need combinators?
    What are the basic combinator we need in our programming languages?
    What are the advanced combinators that we need for our programming language?
    -- need to complete this.

- In functional programming language there are two families of programming language.
    - ML - Typed lamda calculus (Haskell, Purescript)
    - Lisp - Untyped lamda calculus

-- Lamda calculus is used in optimising code. Dead code optmization happens here. Basic idea is every program can undergo certiain beta reduction and other replacemts and can be reduced to few SKI combinators of lamda calculus and certain part of the code is just eleminated if during simplification. It is very similar to :: 100 * 4 * 8 * 0 = 0 no matter what. So calculation will never happen.

We move one step above lamda calculus which is SYSTEM F programming language which has all the basic constructs required for a programming language. Every program after desugaring expanda into this. Basically all the special costructs that you see in your code which makes your code short and concise are some magic that is done here. Note :: SYSTEM F is a typed language. Types exist even at this stage of this code. The AST at this level just needs a compiler which understands the following things.

(TODO :: Need to expalain what are these and why they are used.)
data Expr
  = App Expr Expr
  | Var Var
  | Lam Name Type Expr
  | Case Expr [Alt]
  | Let Bind Expr
  | Lit Literal
  | Placeholder Name Type

data Var
  = Id Name Type
  | TyVar Name Kind

data Type
  = TVar TVar
  | TCon TyCon
  | TApp Type Type
  | TArr Type Type
  | TForall [Pred] [TVar] Type

data Kind
  = KStar
  | KArr Kind Kind
  | KPrim
  | KVar Name

The types are checked twice. Once just after the parsing the source code and once after the source code is desugared and converted into SYSTEM F. The explanation for why types exist even at this level is discussed in the link provided above [1].

- What are the steps in compile time    
    -- GHC [3][http://aosabook.org/en/ghc.html#fig.ghc.pipeline]
    -- need to give more details here.

Following connects all the important things needed to figure out how GHC was built.
    [4][https://ghc.haskell.org/trac/ghc/wiki/Commentary]

Following is an incomplete tutorial on buidling a functional programming compiler from first principles.
    [5][http://dev.stephendiehl.com/fun/index.html]

Following is a link to to understand functional programming language better
    [6][http://dev.stephendiehl.com/hask/#cabal]

Folloing is very good discussion on what is needed to build a basic functional programming language.
    [7][http://dev.stephendiehl.com/fun/007_path.html]

Up until now we discussed how Haskell was designed. After this we will try to figure out things with purescript as the center.

- Now let us write a simple code and see what happens to it in a purescript compiler and how does it get converted to Javascript code. We will also create the intermidiate ASTs after each compiler iteration and reference to what part of the code is responsible for doing this.


# Please refer to the Sorce-JS-Purescript for the rest of the tutorial