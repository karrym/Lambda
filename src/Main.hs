
module Main where

import Control.Monad (unless)
import Control.Monad.State
import System.IO (hFlush, stdout)
import System.Environment (getArgs, getProgName)
import Eval
import Type
import Term
import Parser
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Char
import Data.Maybe (isJust)
import Debug.Trace

getInput :: IO String
getInput = putStr "λ> " >> hFlush stdout >> getLine

repl :: IO ()
repl = do
        input <- getInput
        unless (input == "quit" || input == "q") $ do
            case eval input of
                Left err -> putStrLn err
                Right (t, m) -> do
                    e <- m
                    putStrLn $ "type: " ++ show t
                    putStrLn $ "value: " ++ show e
            repl

toNumber :: IxLam -> Maybe Int
toNumber (IxLam (IxLam l)) = go l where
    go (IxApp (Index 1) x) = (+ 1) <$> go x
    go (Index 0)           = return 0
    go _                   = Nothing
toNumber (Closure _ l)     = toNumber l
toNumber _                 = Nothing

fromNumber :: Int -> IxLam
fromNumber 0 = IxLam (IxLam (Index 0))
fromNumber n = case fromNumber (n-1) of
                   IxLam (IxLam l) -> IxLam (IxLam (IxApp (Index 1) l))
                   _               -> undefined

-- church encoding
toString :: IxLam -> String
toString (IxLam (IxLam l)) = go l where
    go (IxApp (IxApp (Index 1) a) b) = case toNumber a of
                                       Just c -> chr c : go b
                                       Nothing -> "Wrong integer format."
    go (Index 0) = []
    go _ = "Wrong string format."
toString (Closure _ l) = toString (IxLam l)
toString _ = "Wrong string format."

fromString :: String -> IxLam
fromString = IxLam . IxLam . go where
    go [] = Index 0
    go (x:xs) = IxApp (IxApp (Index 1) (fromNumber $ ord x)) $ go xs

fromNumber' :: Int -> Lambda
fromNumber' 0 = Lam "s" (Lam "z" (Var "z"))
fromNumber' n = case fromNumber' (n-1) of
                   Lam s (Lam z l) -> Lam s (Lam z (App (Var s) l))
                   _               -> undefined

fromString' :: String -> Lambda
fromString' = Lam "c" . Lam "n" . go where
    go [] = Var "n"
    go (x:xs) = App (App (Var "c") (fromNumber' $ ord x)) $ go xs

correct :: Type -> Bool
correct t = isJust $ unify [(t, c)] where
    Just c = parse (evalStateT tfun (maxVar t + 1, M.empty)) "((((a->a)->a->a)->b->b)->b->b)->(((a->a)->a->a)->b->b)->b->b"

main :: IO ()
main = do
        args <- getArgs
        case args of
            [] -> repl
            [x] -> do
                code <- readFile x
                input <- getContents
                case eval code of
                    Left s -> putStrLn s
                    Right (Forall _ t, m) -> do
                        print t
                        m >>= reduce IM.empty >>= print
                        if not $ correct t
                            then do
                                putStrLn "Type incorrect"
                                putStrLn "correct type: ((((a->a)->a->a)->b->b)->b)->(((a->a)->a->a)->b->b)->b"
                            else do
                                l <- m
                                r <- reduce IM.empty . IxApp l $ fromString input
                                putStrLn $ toString r
            _ -> do
                prog <- getProgName
                putStrLn $ "Usage: " ++ prog ++ " [filename]"
