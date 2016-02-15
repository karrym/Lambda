
module Eval where

import Term
import Parser
import Type
import qualified Data.IntMap as IM
import Control.Monad ((>=>), when)
import Control.Monad.ST
import Data.List (elemIndex)
import Data.STRef

toIndexed :: Lambda -> Maybe IxLam
toIndexed = go [] where
    go xs (Var x)   = Index <$> x `elemIndex` xs
    go xs (App a b) = IxApp <$> go xs a <*> go xs b
    go xs (Lam x l) = IxLam <$> go (x:xs) l
    go xs (Let s a b) = IxLet <$> go xs a <*> go (s:xs) b
    go xs (Fix f e) = IxFix <$> go (f:xs) e

reduce :: IxLam -> IxLam
reduce (IxApp a b) = case reduce a of
                         IxLam l -> reduce $ runST (newSTRef (True, b) >>= replace 0 l)
                         c       -> IxApp c (reduce b)
reduce (IxLam l)   = IxLam (reduce l)
reduce (IxLet a b) = reduce (IxApp (IxLam b) a)
reduce f@(IxFix e) = reduce $ runST (newSTRef (False, f) >>= replace 0 e)
reduce l           = l

replace :: Int -> IxLam -> STRef s (Bool, IxLam) -> ST s IxLam
replace n (Index i) l  = case compare n i of
                            LT -> return $ Index (i - 1)
                            EQ -> do
                                (p, f) <- readSTRef l
                                when p $ writeSTRef l (False, reduce f)
                                shift n . snd <$> readSTRef l
                            GT -> return $ Index i
replace n (IxApp a b) l = IxApp <$> replace n a l <*> replace n b l
replace n (IxLam a) b   = IxLam <$> replace (n+1) a b
replace n (IxLet a b) l = IxLet <$> replace n a l <*> replace (n+1) b l
replace n (IxFix e) l   = IxFix <$> replace (n+1) e l

shift :: Int -> IxLam -> IxLam
shift m = go 0 where
    go n (Index i)   = if n <= i
                           then Index (i + m)
                           else Index i
    go n (IxApp a b) = IxApp (go n a) (go n b)
    go n (IxLam l)   = IxLam $ go (n+1) l
    go n (IxLet a b) = IxLet (go n a) (go (n+1) b)
    go n (IxFix e)   = IxFix $ go (n+1) e

eval :: String -> Either String (Type, IxLam)
eval str = do
        v <- maybe (Left "Could not parse.") Right $ parse assign str
        e <- maybe (Left "This term includes free variable.") Right $ toIndexed v
        t <- maybe (Left "This term has no type.") Right $ typeinfer e
        return (t, reduce e)
