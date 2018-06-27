module NanoParsec where

-- import Data.Char
-- import Control.Monad
-- import Control.Applicative
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
-- import Control.Monad.Rec.Class (forever)
-- import Data.Array (concat, concatMap, head, tail)
-- import Data.Either (Either(..))
-- import Data.Eq ((==))
-- import Data.Int (fromString)
-- import Data.Maybe (fromMaybe)
-- import Data.String (toCharArray)
import Data.Tuple (Tuple(..))
import Node.ReadLine (READLINE, close, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)
import Prelude (class Monad, Unit(..), unit, show, ($), (*), (+), (-), (==), bind, pure)

type FString = Array Char

newtype Parser a = Parser { parse :: FString -> Array (Tuple a FString) }

-- parse :: forall a. Parser a -> FString -> Array (Tuple a FString)
-- parse (Parser { parse : p }) s = p s

-- runParser :: forall a. Parser a -> FString -> Either String a
-- runParser m s =
--   case parse m s of
--     [Tuple res []] -> Right res
--     [Tuple _ _]   -> Left "Parser did not consume entire stream."
--     _           -> Left "Parser error."

-- item :: Parser Char
-- item = Parser { parse : (\s -> case s of
--                 [] -> []
--                 arr -> [Tuple (fromMaybe (fromCharCode 0) (head arr)) (fromMaybe [] (tail arr))]) }

-- bind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
-- bind p f = Parser { parse : (\s -> concatMap (\(Tuple a s') -> 
--                                             parse (f a) s') (parse p s)) }

-- -- before pure it was unit
-- unit :: forall a. a -> Parser a
-- unit a = Parser { parse : (\s -> [Tuple a s]) }

-- -- instance functorParser :: Functor Parser where
-- --   fmap f (Parser cs) = Parser { parse : (\s -> [(f a, b) | (a, b) <- cs s])}

-- -- instance Applicative Parser where
-- --   pure = return
-- --   (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

-- -- instance monadParser :: Monad Parser where
-- --   pure = pureParser
-- --   bind  = bind

-- -- instance MonadPlus Parser where
-- --   mzero = failure
-- --   mplus = combine

-- -- instance alternativeParser :: Alternative Parser where
-- --   empty = mzero
-- --   (<|>) = option

-- combine :: forall a. Parser a -> Parser a -> Parser a
-- combine p q = Parser { parse : (\s -> concat [parse p s, parse q s])}

-- failure :: forall a. Parser a
-- failure = Parser { parse : (\cs -> [])}

-- option :: forall a. Parser a -> Parser a -> Parser a
-- option  p q = Parser { parse : (\s -> case parse p s of 
--                                         []     -> parse q s
--                                         res    -> res) }

-- satisfy :: (Char -> Boolean) -> Parser Char
-- satisfy p = item `bind` \c ->
--   if p c
--   then unit c
--   else failure

-- -- -------------------------------------------------------------------------------
-- -- -- Combinators
-- -- -------------------------------------------------------------------------------

-- -- oneOf :: Array Char -> Parser Char
-- -- oneOf s = satisfy (flip elem s)

-- -- chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
-- -- chainl p op a = (p `chainl1` op) <|> return a

-- -- chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- -- p `chainl1` op = do {a <- p; rest a}
-- --   where rest a = (do f <- op
-- --                      b <- p
-- --                      rest (f a b))
-- --                  <|> return a

-- -- char :: Char -> Parser Char
-- -- char c = satisfy (c ==)

-- -- natural :: Parser Integer
-- -- natural = read <$> some (satisfy isDigit)

-- -- string :: String -> Parser String
-- -- string [] = return []
-- -- string (c:cs) = do { char c; string cs; return (c:cs)}

-- -- token :: Parser a -> Parser a
-- -- token p = do { a <- p; spaces ; return a}

-- -- reserved :: String -> Parser String
-- -- reserved s = token (string s)

-- -- spaces :: Parser String
-- -- spaces = many $ oneOf " \n\r"

-- -- digit :: Parser Char
-- -- digit = satisfy isDigit

-- -- number :: Parser Int
-- -- number = do
-- --   s <- string "-" <|> return []
-- --   cs <- some digit
-- --   return $ read (s ++ cs)

-- -- parens :: Parser a -> Parser a
-- -- parens m = do
-- --   reserved "("
-- --   n <- m
-- --   reserved ")"
-- --   return n

-- -- -------------------------------------------------------------------------------
-- -- -- Calulator parser
-- -- -------------------------------------------------------------------------------

-- -- -- number = [ "-" ] digit { digit }.
-- -- -- digit = "0" | "1" | ... | "8" | "9".
-- -- -- expr = term { addop term }.
-- -- -- term = factor { mulop factor }.
-- -- -- factor = "(" expr ")" | number.
-- -- -- addop = "+" | "-".
-- -- -- mulop = "*".

-- data Expr
--   = Add Expr Expr
--   | Mul Expr Expr
--   | Sub Expr Expr
--   | Lit Int

-- eval :: Expr -> Int
-- eval ex = case ex of
--   Add a b -> eval a + eval b
--   Mul a b -> eval a * eval b
--   Sub a b -> eval a - eval b
--   Lit n   -> n

-- -- int :: String -> Parser Expr
-- -- int i = do
-- --   n <- fromString i
-- --   return (Lit n)

-- -- expr :: Parser Expr
-- -- expr = term `chainl1` addop

-- -- term :: Parser Expr
-- -- term = factor `chainl1` mulop

-- -- factor :: Parser Expr
-- -- factor =
-- --       int
-- --   <|> parens expr

-- -- infixOp :: FString -> (a -> a -> a) -> Parser (a -> a -> a)
-- -- infixOp x f = reserved x >> return f

-- -- addop :: Parser (Expr -> Expr -> Expr)
-- -- addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

-- -- mulop :: Parser (Expr -> Expr -> Expr)
-- -- mulop = infixOp "*" Mul

-- -- run :: String -> Expr
-- -- run s = runParser  $ toCharArray s

main :: forall e. Eff (exception :: EXCEPTION, readline :: READLINE, console :: CONSOLE| e) Unit
main = do
  interface <- createConsoleInterface noCompletion
  _ <- setPrompt ">  " 2 interface
  _ <- prompt interface
  setLineHandler interface $ \s ->
    if s == "quit"
        then close interface
      else do
        _ <- pure $ show s
        -- _ <- log $ show $ eval $ run s
        prompt interface


