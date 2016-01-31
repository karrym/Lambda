
module Type where

import Term
import Parser
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.List ((\\))
import Data.Foldable (foldlM)
import Control.Monad.State
import Control.Monad (guard, replicateM)
import Control.Applicative (Alternative(..))
import Control.Monad.Trans.Class (lift)
import Control.Arrow ((***))
import Debug.Trace

data Type = TVar Int
          | TFun Type Type
          deriving (Eq)

data Scheme = Forall IS.IntSet Type
            deriving (Eq)

maxVar :: Type -> Int
maxVar (TVar i) = i
maxVar (TFun a b) = max (maxVar a) (maxVar b)

minVar :: Type -> Int
minVar (TVar i) = i
minVar (TFun a b) = min (minVar a) (minVar b)

instance Show Type where
        show t = evalState (showExpr 0 t) (IM.singleton (minVar t) 'a') where
            open  a b = if a > b then "(" else ""
            close a b = if a > b then ")" else ""
            showExpr :: Int -> Type -> State (IM.IntMap Char) String
            showExpr _ (TVar i)   = do
                e <- get
                case IM.lookup i e of
                    Just c -> return [c]
                    Nothing -> do
                        let (_, c) = IM.findMax e
                        modify $ IM.insert i (succ c)
                        return [succ c]
            showExpr n (TFun a b) = do
                s1 <- showExpr 1 a
                s2 <- showExpr 0 b
                return $ open n 0 ++ s1 ++ " -> " ++ s2 ++ close n 0
{-
instance Show Type where
        show = showExpr 0 where
            open  a b = if a > b then "(" else ""
            close a b = if a > b then ")" else ""
            showExpr :: Int -> Type -> String
            showExpr _ (TVar i)   = show i
            showExpr n (TFun a b) = open n 0 ++ showExpr 1 a ++ " -> " ++ showExpr 0 b ++ close n 0
-}

instance Show Scheme where
        show (Forall a t) = show a ++ '.':show t

tfun :: StateT (Int, M.Map String Int) Parser Type
tfun = TFun <$> tvar <*> (lift (string "->") *> tfun)
    <|> tvar

tvar :: StateT (Int, M.Map String Int) Parser Type
tvar = lift (char '(') *> tfun <* lift (char ')')
    <|> do
        v <- lift token
        (i, m) <- get
        case M.lookup v m of
            Nothing -> put (i+1, M.insert v i m) >> return (TVar i)
            Just i -> return (TVar i)

type TypeEnv = IM.IntMap Scheme

type Subst = IM.IntMap Type

type Rest = (Type, Type)

addType :: Scheme -> TypeEnv -> TypeEnv
addType t = IM.insert 0 t . IM.mapKeys succ

freshType :: Monad m => StateT Int m Type
freshType = TVar <$> (get <* modify succ)

subst :: Subst -> Type -> Type
subst s (TVar i) = maybe (TVar i) id $ IM.lookup i s
subst s (TFun a b) = TFun (subst s a) (subst s b)

isFree :: Int -> Type -> Bool
isFree n (TVar m) = n /= m
isFree n (TFun a b) = isFree n a && isFree n b

unifycate :: Subst -> [Rest] -> Maybe Subst
unifycate s [] = return s
unifycate s ((TVar n, TVar m):xs) | n == m = unifycate s xs
unifycate s ((TVar n, t):xs) = do
      guard $ isFree n t
      let f = subst $ IM.singleton n t
      unifycate (IM.insert n t $ IM.map f s) $ map (f *** f) xs
unifycate s ((t, TVar n):xs) = unifycate s ((TVar n, t):xs)
unifycate s ((TFun a b, TFun c d):xs) = unifycate s ((a,c):(b,d):xs)

unify :: [Rest] -> Maybe Subst
unify = unifycate IM.empty

freeType :: Type -> IS.IntSet
freeType (TVar i) = IS.singleton i
freeType (TFun a b) = IS.union (freeType a) (freeType b)

toRest :: Subst -> [Rest]
toRest = IM.foldWithKey (\a t r -> (TVar a, t) : r) []

closure :: Type -> TypeEnv -> Scheme
closure t e = flip Forall t $ freeType t IS.\\ vs where
    vs = IM.foldl (\b (Forall _ a) -> IS.union (freeType a) b) IS.empty e

instant :: Monad m => Scheme -> StateT Int m Type
instant (Forall v t) = do
        s <- foldlM (\m i -> do
                    n <- freshType
                    return $ IM.insert i n m) IM.empty (IS.elems v)
        return $ subst s t

polytype :: TypeEnv -> IxLam -> StateT Int Maybe (Subst, Type)
polytype e (Index i) = do
        t <- instant =<< (lift $ IM.lookup i e)
        return (IM.empty, t)
polytype e (IxLam l) = do
        a <- freshType
        (s, t) <- polytype (addType (Forall IS.empty a) e) l
        --trace (show s) $ return ()
        --trace (show t) $ return ()
        return (s, TFun (subst s a) t)
polytype e (IxApp l r) = do
        (s1, t1) <- polytype e l
        (s2, t2) <- polytype (IM.map (\(Forall s t) -> Forall s $ subst s1 t) e) r
        a <- freshType
        --trace (show (subst s2 t1, TFun t2 a)) $ return ()
        s3 <- lift $ unify [(subst s2 t1, TFun t2 a)]
        --trace (show s3) $ return ()
        --trace (show a) $ return ()
        s <- lift . unify $ toRest =<< [s1, s2, s3]
        return (s, subst s3 a)
polytype e (IxLet e1 e2) = do
        (s1, t1) <- polytype e e1
        let s = closure t1 (IM.map (\(Forall x y) -> Forall x $ subst s1 y) e)
        (s2, t2) <- polytype (addType s e) e2
        s3 <- lift . unify $ toRest s1 ++ toRest s2
        return (s3, subst s3 t2)
polytype e (IxFix l) = do
        a <- freshType
        (s1, t) <- polytype (addType (Forall IS.empty a) e) l
        s2 <- lift . unify $ (a, t) : toRest s1
        return (s2, subst s2 t)
polytype _ _ = empty

typeinfer :: IxLam -> Maybe Type
typeinfer l = snd <$> evalStateT (polytype IM.empty l) 0
