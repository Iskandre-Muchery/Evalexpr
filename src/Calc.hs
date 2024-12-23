module Calc
(runEvalExpr,
runEvalExprTestSuite,
) where

import Control.Applicative
import System.Environment ( getArgs )
import Text.Read
import Numeric
import Text.Printf
import System.Exit
import System.IO
import Data.Maybe
import Data.Char
import Data.String
import Data.List
import Parser

data Operators = Add Operators Operators
            | Sub Operators Operators
            | Mul Operators Operators
            | Div Operators Operators
            | Pow Operators Operators
            | Lit Float

whichh :: String -> String
whichh str = foldl (\tab str' -> tab++str') [] (words str)

selector :: Operators -> Float
selector e = case e of
  Add a b -> selector a + selector b
  Sub a b -> selector a - selector b
  Mul a b -> selector a * selector b
  Pow a b -> selector a ** selector b
  Div a b -> selector a / selector b
  Lit n -> n

definePriorities :: Expound Operators
definePriorities = entry
    where
    entry = binOp Sub '-' entry1 <|> binOp Add '+' entry1 <|> entry1
    entry1 = binOp Sub '-' entry2 <|> binOp Add '+' entry2 <|> entry2
    entry2 = binOp Sub '-' term <|> binOp Add '+' term <|> term
    term = binOp Add '+' muldiv <|> binOp Sub '-' muldiv <|> muldiv
    muldiv = binOp Mul '*' muldiv1 <|> binOp Div '/' muldiv1 <|> muldiv1
    muldiv1 = binOp Mul '*' muldiv2 <|> binOp Div '/' muldiv2 <|> muldiv2
    muldiv2 = binOp Mul '*' power <|> binOp Div '/' power <|> power
    power = binOp Pow '^' factor <|> factor
    factor = parens <|> lit
    lit = Lit <$> number
    parens = (word '(' *> definePriorities <* word ')')
    binOp c o p = fmap c p <*> (word o *> p)

evalExpr :: String -> Either String Float
evalExpr s = (fmap selector) $ runParser definePriorities (whichh s)

runEvalExpr :: IO()
runEvalExpr = do
    args <- getArgs
    if (False `elem` (map (`elem` "0123456789 \t\n.-+/*^()") (head args)))
        then do
            exitWith (ExitFailure 84)
        else case evalExpr (head args) of
                Left err -> exitWith (ExitFailure 84)
                Right result -> printf "%.2f\n" $ result

runEvalExprTestSuite :: String -> IO()
runEvalExprTestSuite args = do
        case evalExpr args of
            Left err -> putStrLn "Test Failed"
            Right result -> printf "%.2f\n" $ result