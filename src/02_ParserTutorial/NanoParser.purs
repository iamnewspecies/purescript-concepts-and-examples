module NanoParsec where

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Node.ReadLine (READLINE, close, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)
import Prelude (class Eq, class Monad, Unit(..), add, bind, div, mul, negate, pure, show, sub, unit, ($), ($>), (*), (+), (-), (==), (>>=))
import Text.Parsing.StringParser (ParseError(..), Parser, runParser, try)
import Text.Parsing.StringParser.Combinators (many1, endBy1, sepBy1, optionMaybe, many, manyTill, many1Till, chainl, fix, between)
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.StringParser.String (anyDigit, eof, string, anyChar, regex)

digit :: Parser Int
digit = string "0" $> 0
    <|> string "1" $> 1
    <|> string "2" $> 2
    <|> string "3" $> 3
    <|> string "4" $> 4
    <|> string "5" $> 5
    <|> string "6" $> 6
    <|> string "7" $> 7
    <|> string "8" $> 8
    <|> string "9" $> 9

exprTest :: Parser Int
exprTest = buildExprParser [ [Infix (string "/" >>= \_ -> pure div) AssocRight]
                           , [Infix (string "*" >>= \_ -> pure mul) AssocRight]
                           , [Infix (string "-" >>= \_ -> pure sub) AssocRight]
                           , [Infix (string "+" >>= \_ -> pure add) AssocRight]
                           ] digit

expectResult :: Parser Int -> String -> Either ParseError Int
expectResult p input = runParser p input

main :: forall e. Eff (exception :: EXCEPTION, readline :: READLINE, console :: CONSOLE| e) Unit
main = do
  interface <- createConsoleInterface noCompletion
  _ <- setPrompt ">  " 2 interface
  _ <- prompt interface
  setLineHandler interface $ \s ->
    if s == "quit"
        then close interface
      else do
        _ <- log $ show $ expectResult exprTest "1*2+3/4-5"
        prompt interface


