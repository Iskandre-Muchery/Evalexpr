module Parser
(runParser,
word,
number,
Expound
) where

import Control.Applicative
import System.Environment ( getArgs )
import Text.Read
import Numeric
import System.Exit
import System.IO
import Data.Maybe
import Data.Char
import Data.String
import Data.List


newtype Expound a  = Expound {
  parse :: String -> Either String (a, String)
}

instance Functor Expound where
  fmap f pa = Expound p
      where
      p str = case (parse pa $ str) of
          Left err -> Left err
          Right (parsed, str') -> Right (f parsed, str')

instance Applicative Expound where
  pure a = Expound $ \str -> Right (a, str)
  liftA2 fabc pa pb = Expound p
      where
          p str = case (parse pa $ str) of
              Left err -> Left err
              Right (parsed, str') -> case (parse pb $ str') of
                  Left err' -> Left err'
                  Right (parsed', str'') -> Right (fabc parsed parsed', str'')

instance Monad Expound where
  return = pure
  pa >>= fapb = Expound b
      where
          b str = case (parse pa $ str) of
              Left e -> Left e
              Right (parsed, str') -> parse (fapb parsed) str'

instance Alternative Expound where
  empty = Expound $ \_ -> Left "There was an error"
  pa <|> pa' = Expound p where
      p str = case (parse pa $ str) of
          Left _  -> (parse pa' $ str)
          Right x -> Right x

word :: Char -> Expound Char
word c = validate (c ==)

oneOf :: [Char] -> Expound Char
oneOf s = validate (`elem` s)

validate :: (Char -> Bool) -> Expound Char
validate pre = Expound p
  where
      p [] = Left "There was an error"
      p (c:cs) = case (pre c) of
          False -> Left "There was an error"
          True -> Right (c, cs)

defineDigits :: Expound Char
defineDigits = oneOf ".0123456789"

number :: Expound Float
number = read <$> some defineDigits

runParser :: Expound a -> String -> Either String a
runParser (Expound p) s = fst <$> p s
