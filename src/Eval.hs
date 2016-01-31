
module Eval where

import Term
import Parser
import Type
import qualified Data.IntMap as IM
import Control.Monad ((>=>))
import Data.List (elemIndex)
import Data.IORef

toIndexed :: Lambda -> Maybe IxLam
toIndexed = go [] where
    go xs (Var x)   = Index <$> x `elemIndex` xs
    go xs (App a b) = IxApp <$> go xs a <*> go xs b
    go xs (Lam x l) = IxLam <$> go (x:xs) l
    go xs (Let s a b) = IxLet <$> go xs a <*> go (s:xs) b
    go xs (Fix f e) = IxFix <$> go (f:xs) e

addExpr :: IORef Memory -> Env -> Env
addExpr l = IM.insert 0 l . IM.mapKeys succ

reduce :: Env -> IxLam -> IO IxLam
reduce env (Index i) = do
        case IM.lookup i env of
            Nothing -> return $ Index i
            Just ref -> do
                v <- readIORef ref
                case v of
                    Value e -> return e
                    Thunk e f -> do
                        r <- reduce e f
                        writeIORef ref (Value r)
                        return r
reduce env (IxLam l) = return $ Clojure env l
reduce env (IxLet a b) = do
        ref <- newIORef (Thunk env a)
        reduce (addExpr ref env) b
reduce env (IxApp a b) = do
        l <- reduce env a
        case l of
            Clojure e c -> do
                ref <- newIORef (Thunk env b)
                reduce (addExpr ref e) c
            c -> IxApp c <$> reduce env b
reduce env f@(IxFix e) = do
        ref <- newIORef (Thunk env f)
        reduce (addExpr ref env) e
reduce _ a = return a

eval :: String -> Either String (Type, IO IxLam)
eval str = do
        v <- maybe (Left "Could not parse.") Right $ parse assign str
        e <- maybe (Left "This term includes free variable.") Right $ toIndexed v
        t <- maybe (Left "This term has no type.") Right $ typeinfer e
        return (t, reduce IM.empty e)
