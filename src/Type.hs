
module Type where

import Term
import Parser
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.List ((\\))
import Control.Monad.State
import Control.Monad (guard, replicateM)
import Control.Applicative (Alternative(..))
import Control.Monad.Trans.Class (lift)
import Control.Arrow ((***))

data Type = TVar Int
          | TFun Type Type
          deriving (Eq)

data Scheme = Forall [Int] Type
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
                        let (i, c) = IM.findMax e
                        modify $ IM.insert (i+1) (succ c)
                        return [succ c]
            showExpr n (TFun a b) = do
                s1 <- showExpr 1 a
                s2 <- showExpr 0 b
                return $ open n 0 ++ s1 ++ " -> " ++ s2 ++ close n 0

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


unify :: [Rest] -> Maybe Subst
unify [] = return IM.empty
unify ((TVar n, TVar m):xs) | n == m = unify xs
unify ((TVar n, t):xs) = do
      guard $ isFree n t
      let sub = IM.singleton n t
          f = subst sub
      flip IM.union sub <$> (unify $ map (f *** f) xs)
unify ((t, TVar n):xs) = unify ((TVar n, t):xs)
unify ((TFun a b, TFun c d):xs) = unify ((a,c):(b,d):xs)

freeType :: Scheme -> [Int]
freeType (Forall ts t) = vars t \\ ts where
    vars (TVar i) = [i]
    vars (TFun t1 t2) = vars t1 ++ vars t2

toRest :: Subst -> [Rest]
toRest = IM.foldWithKey (\a t r -> (TVar a, t) : r) []

closure :: Scheme -> TypeEnv -> Scheme
closure s@(Forall _ t) e = flip Forall t $ freeType s \\ (IM.elems e >>= freeType)

polytype :: TypeEnv -> IxLam -> StateT Int Maybe (Subst, Scheme)
polytype e (Index i) = do
        x <- lift $ IM.lookup i e  
        case x of
            Forall v t -> do
                v1 <- replicateM (length v) freshType
                let s = foldr (uncurry IM.insert) IM.empty $ zip v v1
                return (IM.empty, Forall [] $ subst s t)
polytype e (IxLam l) = do
        a <- freshType
        (s, Forall ts t) <- polytype (addType (Forall [] a) e) l
        return (s, Forall ts $ TFun (subst s a) t)
polytype e (IxApp l r) = do
        (s1, Forall ts1 t1) <- polytype e l
        (s2, Forall ts2 t2) <- polytype e r
        a <- freshType
        s3 <- lift . unify $ (t1, TFun t2 a) : toRest (IM.union s1 s2)
        return (s3, Forall (ts1 ++ ts2) $ subst s3 a)
polytype e (IxLet e1 e2) = do
        (s1, t1) <- polytype e e1
        let s = closure t1 (IM.map (\(Forall x y) -> Forall x $ subst s1 y) e)
        (s2, Forall ts t2) <- polytype (addType s e) e2
        s3 <- lift . unify . toRest $ IM.union s1 s2
        return (s3, Forall ts $ subst s3 t2)
polytype e (IxFix l) = do
        a <- freshType
        (s1, Forall ts t) <- polytype (addType (Forall [] a) e) l
        s2 <- lift . unify $ (a, t) : toRest s1
        return (s2, Forall ts $ subst s2 t)
polytype _ _ = empty

typeinfer :: IxLam -> Maybe Scheme
typeinfer l = snd <$> evalStateT (polytype IM.empty l) 0
