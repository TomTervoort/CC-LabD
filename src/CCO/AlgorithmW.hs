
-- | Implements type inference of a Hindley-Miller term using algorithm W.
module CCO.AlgorithmW where

import CCO.SystemF
import qualified CCO.HM as HM

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow
import Control.Monad

-- | Generator of fresh (type) variables.
newtype VarFactory = VarFactory Int

-- | Initialise a new fresh variable factory.
initVarFactory :: VarFactory
initVarFactory = VarFactory 0

-- | Generate a fresh variable.
freshVar :: VarFactory -> (Var, VarFactory)
freshVar (VarFactory n) = ("_" ++ show n, VarFactory $ n + 1)

-- | Generate a fresh type variable.
freshTyVar :: VarFactory -> (TyVar, VarFactory)
freshTyVar (VarFactory n) = ("__" ++ show n, VarFactory $ n + 1)

-- | Type generalisation.
gen :: TyEnv -> Ty -> Ty
gen env ty = S.foldr Forall ty $ freeVars ty `S.difference` freeEnvVars env

-- | Type instantiation.
inst :: VarFactory -> Ty -> (Ty, VarFactory)
inst fac (Forall a t) = let (fresh, fac') = freshTyVar fac
                         in first (subTyVar a $ TyVar fresh) $ inst fac' t
inst fac ty = (ty, fac)

-- | Substitute one type variable with a type.
subTyVar :: TyVar -> Ty -> TySubst
subTyVar from to = sub
 where sub ty = case ty of
                 TyVar v    | v == from -> to
                            | otherwise -> TyVar v
                 Forall a t | from == a -> Forall a t
                            | otherwise -> Forall a $ sub t
                 Arr t1 t2  -> Arr (sub t1) (sub t2)

-- | Robinson's unification algorithm. Uses Monad 'fail' in case of a type error.
unify :: Monad m => Ty -> Ty -> m TySubst
unify a b =
 case (a,b) of
  (TyVar a, TyVar b) | a == b -> return id
  (TyVar a1, t2)     | not $ a1 `S.member` freeVars t2 -> return $ subTyVar a1 t2
  (t1, TyVar a2)     | not $ a2 `S.member` freeVars t1 -> return $ subTyVar a2 t1
  (Arr t11 t12, Arr t21 t22) -> do s1 <- unify t11 t21
                                   s2 <- unify (s1 t12) (s1 t22)
                                   return $ s2 . s1
  _ -> fail $ concat ["Type error.\n\tExpected: '", show a, "'.\n\tActual: '", show b, "'."]


-- | Implements algorithm W for type inference.
algorithmW :: Monad m => VarFactory -> TyEnv -> HM.Tm -> m (Ty, TySubst, VarFactory)
algorithmW fac env (HM.Tm spos term) =
 case term of
  HM.Var x       -> case lookupEnv x env of 
                     Nothing -> fail $ "Can not infer type of '" ++ x ++ "'." -- TODO: use spos
                     Just ty -> return (ty, id, fac)
  HM.Lam x t1    -> do let (a1, fac') = first TyVar $ freshTyVar fac
                       (ty2, s, fac') <- algorithmW fac' (insertEnv x a1 env) t1
                       return (Arr (s a1) ty2, s, fac')
  HM.App t1 t2   -> do let (a, fac') = first TyVar $ freshTyVar fac
                       (ty1, s1, fac') <- algorithmW fac' env t1
                       (ty2, s2, fac') <- algorithmW fac' (mapEnv s1 env) t2
                       s3 <- unify (s2 ty1) (Arr ty2 a)
                       return (s3 a, s3 . s2 . s1, fac')
  HM.Let x t1 t2 -> do (ty1, s1, fac') <- algorithmW fac env t1
                       (ty,  s2, fac') <- algorithmW fac' 
                                            (mapEnv s1 
                                            $ insertEnv x (gen (mapEnv s1 env) ty1) env
                                            ) t2
                       return (ty, s2 . s1, fac')

-- | Uses algorithmW to find a principal type: the most polymorphic type that can be assigned to a 
--   given term. An environment should be provided and will be updated. Monadic 'fail' is used in 
--   case of a type error. 
inferPrincipalType :: Monad m => HM.Tm -> TyEnv -> m (Ty, TyEnv)
inferPrincipalType term env = 
  do (ty, s, _) <- algorithmW initVarFactory env term
     let newEnv = mapEnv s env
     return (gen newEnv ty, newEnv)
                             
