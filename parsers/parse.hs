{-# LANGUAGE GADTs, FlexibleContexts #-}
module Parser where
import Control.Monad
import Lexer

data Transition where
    None :: Transition
    Accept :: Transition
    Shift :: String -> Transition
    Reduce :: String -> Int -> Transition
    deriving (Show, Eq)

data SemanticVal where
    S :: String -> SemanticVal
    I :: Int -> SemanticVal
    Error :: String -> SemanticVal
    --V :: [a] -> SemanticVal
    deriving (Show, Eq)

pos :: Lexer.Token -> (Int, Int)
pos (_, _, p) = p

tokenName :: Lexer.Token -> String
tokenName (Lexer.Name n, _, _) = n
tokenName (Lexer.Skip, _, _) = "(SKIP)"
tokenName (Lexer.Err e, _, _) = "(ERR) " ++ e

parser :: (String -> (String -> Transition)) -> String -> [Lexer.Automata] -> String -> SemanticVal
parser table start automata input =
    case Lexer.lexer automata input of
        Nothing -> Error "Bad lexing"
        Just tokens ->
            case tokenName (last tokens) of
                "\x18" -> actualParser table tokens [start] []
                _ -> actualParser table (tokens ++ [(Lexer.Name "\x18", "\x18", (Lexer.tRow (last tokens), Lexer.tCol (last tokens)))]) [start] []

actualParser :: (String -> (String -> Transition)) -> [Lexer.Token] -> [String] -> [SemanticVal] -> SemanticVal
actualParser _ [] _ _ = Error "Unexpected EOF"
actualParser _ _ [] _ = Error "Empty stack"
actualParser table tokens (top:stack) semstack = do {
    case table top (tokenName (head tokens)) of
        None -> Error ("None " ++ show (pos (head tokens)) ++ ": " ++ top ++ ", " ++ tokenName (head tokens))
        Accept -> if null semstack then S "success" else head semstack
        Shift s -> actualParser table (drop 1 tokens) (s:top:stack) semstack
        Reduce s n -> case table (stack !! max 0 (n - 1)) s of
            Shift str -> actualParser table tokens (str : drop (n - 1) stack) semstack
            _ -> Error ("Bad reduce " ++ show (pos (head tokens)) ++ ": " ++ top ++ ", " ++ tokenName (head tokens))
}
-- actualParser _ tokens _ _ = S (show tokens)
