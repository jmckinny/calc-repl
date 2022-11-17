module Braces (validBraces) where
import Data.List (delete)

data Data = Brace Brace | Char Char 
    deriving (Show, Eq)
data Brace = Opening Opening | Closing Closing
    deriving (Show, Eq)
data Opening = OpenParen | OpenSqr | OpenCurly
    deriving (Show, Eq)
data Closing = CloseParen | CloseSqr | CloseCurly
    deriving (Show, Eq)

validBraces :: String -> Bool
validBraces xs = helper (map convert xs) []

helper :: [Data] -> [Opening] -> Bool
helper [] stack = null stack
helper (h:t) stack = 
        case h of
        Brace b -> 
            case b of
            Opening x -> helper t (x:stack)
            Closing x -> (matching x `elem` stack) && helper t (delete (matching x) stack)
        Char _ -> helper t stack

convert :: Char -> Data
convert x
    | x == '(' = Brace $ Opening OpenParen
    | x == '[' = Brace $ Opening OpenSqr
    | x == '{' = Brace $ Opening OpenCurly
    | x == ')' = Brace $ Closing CloseParen
    | x == ']' = Brace $ Closing CloseSqr
    | x == '}' = Brace $ Closing CloseCurly
    | otherwise = Char x

matching :: Closing -> Opening
matching x = 
    case x of
    CloseParen -> OpenParen
    CloseSqr -> OpenSqr
    CloseCurly -> OpenCurly