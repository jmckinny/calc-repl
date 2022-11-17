module Main where
import Data.Char
import Control.Applicative
import Control.Monad
import System.IO

data Expr = Value Integer 
            | Addition (Expr,Expr) 
            | Subtraction (Expr,Expr)
            | Multiplication (Expr,Expr)
            | Division (Expr,Expr)
            | Modulus (Expr,Expr)
            deriving (Show,Eq)


newtype Parser x = Parser { runParser :: String -> Maybe (x, String)}

instance Functor Parser where 
    fmap f (Parser x) = Parser $ \input -> do
        (x',s') <- x input
        return (f x', s')

instance Applicative Parser where
    pure x = Parser $ \s -> Just(x,s)

    (Parser f) <*> (Parser x) = Parser $ \s -> do
        (f',s1) <- f s
        (x',s2) <- x s1 
        return (f' x', s2)

instance Monad Parser where
    (Parser x) >>= f = Parser $ \s -> do 
        (x',s') <- x s
        runParser (f x') s'

instance MonadFail Parser where 
    fail _ = Parser $ \s -> Nothing 

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
      Parser $ \input -> p1 input <|> p2 input


charParse :: Char -> Parser Char
charParse c = Parser p 
    where p [] = Nothing 
          p (x:xs) | x == c = Just (c,xs)
                   | otherwise = Nothing


spanParser :: (Char -> Bool) -> Parser String
spanParser f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (token, rest)


notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (xs, input') <- p input
    if null xs
      then Nothing
      else Just (xs, input')

parseNumber :: Parser Expr
parseNumber = f <$> notNull (spanParser isDigit)
    where f ds = Value $ read ds

parseSpace :: Parser Char
parseSpace = charParse ' '

parseWhiteSpace :: Parser String 
parseWhiteSpace = spanParser isSpace


parseOp :: Char -> Parser (Expr, Expr)
parseOp op = pair
    where pair = (,) <$> (charParse '(' *> parseExpr <* charParse op) <*> (parseExpr <* charParse ')')

parseAdd :: Parser Expr
parseAdd = Addition <$> parseOp '+'

parseSub :: Parser Expr
parseSub = Subtraction <$> parseOp '-'

parseMul :: Parser Expr
parseMul = Multiplication <$> parseOp '*'

parseDiv :: Parser Expr
parseDiv = Division <$> parseOp '/'

parseMod :: Parser Expr
parseMod = Modulus <$> parseOp '%'

parseExpr :: Parser Expr
parseExpr = parseNumber <|> parseAdd <|> parseSub <|> parseMul <|> parseDiv <|> parseMod


evalExpr :: Expr -> Integer
evalExpr e = 
    case e of 
        Subtraction (x,y) -> (evalExpr x) - (evalExpr y)
        Addition (x,y) -> (evalExpr x) + (evalExpr y)
        Multiplication (x,y) -> (evalExpr x) * (evalExpr y)
        Division (x,y) -> (evalExpr x) `div` (evalExpr y)
        Modulus (x,y) -> (evalExpr x) `mod` (evalExpr y)
        Value x -> x

parseEval :: String -> Maybe Integer
parseEval s = case runParser parseExpr (filter (not . isSpace) s) of 
            Nothing -> Nothing 
            Just (x,s) -> Just $ evalExpr x


main :: IO  ()
main = do 
    putStr "> "
    hFlush stdout
    input <- getLine
    unless (input == "quit" || input == "q")
        $ (putStrLn $ show $ parseEval input) >> main