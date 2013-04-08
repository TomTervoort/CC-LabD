

-- UUAGC 0.9.38.1 (CCO/HM2SystemF.ag)
module CCO.HM2SystemF where
{-# LINE 5 "CCO/HM2SystemF.ag" #-}


import qualified CCO.HM as HM

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow
import Control.Monad

{-# LINE 18 "CCO/HM2SystemF.hs" #-}

{-# LINE 2 "./CCO/SystemF/AG/Base.ag" #-}

import CCO.SourcePos
{-# LINE 23 "CCO/HM2SystemF.hs" #-}
{-# LINE 41 "CCO/HM2SystemF.ag" #-}


instance Show Ty where
 show ty = stringRep_Syn_Ty $ wrap_Ty (sem_Ty ty) Inh_Ty

instance Show TyEnv where
 show env = stringRep_Syn_TyEnv $ wrap_TyEnv (sem_TyEnv env) Inh_TyEnv


-- | Look up a variable in an environment.
lookupEnv :: Var -> TyEnv -> Maybe Ty
lookupEnv _ EmptyTyEnv = Nothing
lookupEnv x (ConsTyEnv y t rest) | x == y    = Just t
                                 | otherwise = lookupEnv x rest

-- | Insert a binding into an environment.
insertEnv :: Var -> Ty -> TyEnv -> TyEnv
insertEnv = ConsTyEnv

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

gen :: TyEnv -> Ty -> Ty
gen env ty = S.foldr Forall ty $ freeVars ty `S.difference` freeEnvVars env

freeVars :: Ty -> Set TyVar
freeVars ty = ftv_Syn_Ty $ wrap_Ty (sem_Ty ty) Inh_Ty

freeEnvVars :: TyEnv -> Set TyVar
freeEnvVars env = ftv_Syn_TyEnv $ wrap_TyEnv (sem_TyEnv env) Inh_TyEnv

inst :: VarFactory -> Ty -> (Ty, VarFactory)
inst fac (Forall a t) = let (fresh, fac') = freshTyVar fac
                         in first (subTyVar a $ TyVar fresh) $ inst fac' t
inst fac ty = (ty, fac)

-- | A type substitution.
type TySubst = Ty -> Ty

-- | Map a substitution over all the types within an environment.
mapEnv :: TySubst -> TyEnv -> TyEnv
mapEnv _ EmptyTyEnv = EmptyTyEnv
mapEnv f (ConsTyEnv x t rest) = ConsTyEnv x (f t) $ mapEnv f rest

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


-- | TODO ....
algorithmW :: Monad m => VarFactory -> TyEnv -> HM.Tm -> m (Ty, TySubst, VarFactory)
algorithmW fac env (HM.Tm spos term) =
 case term of
  HM.Var x       -> case lookupEnv x env of 
                     Nothing -> fail $ "Can not infer type of '" ++ x ++ "'." -- TODO: use spos
                     Just ty -> return (ty, id, fac)
  HM.Lam x t1    -> do let (a1, fac') = first TyVar $ freshTyVar fac
                       (ty2, s, fac') <- algorithmW fac' (insertEnv x a1 env) t1
                       return (Arr (s a1) ty2, s, fac') -- TODO: of 's $ Arr a1 ty2' ???
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
{-# LINE 125 "CCO/HM2SystemF.hs" #-}

{-# LINE 10 "./CCO/SystemF/AG/Base.ag" #-}

type TyVar = String    -- ^ Type of type variables. 
type Var   = String    -- ^ Type of variables.
{-# LINE 131 "CCO/HM2SystemF.hs" #-}
-- Tm ----------------------------------------------------------
data Tm  = App (Tm ) (Tm ) 
         | Lam (Var) (Ty ) (Tm ) 
         | TyApp (Tm ) (Ty ) 
         | TyLam (TyVar) (Tm ) 
         | Var (Var) 
-- cata
sem_Tm :: Tm  ->
          T_Tm 
sem_Tm (App _t1 _t2 )  =
    (sem_Tm_App (sem_Tm _t1 ) (sem_Tm _t2 ) )
sem_Tm (Lam _x _ty _t1 )  =
    (sem_Tm_Lam _x (sem_Ty _ty ) (sem_Tm _t1 ) )
sem_Tm (TyApp _t1 _ty )  =
    (sem_Tm_TyApp (sem_Tm _t1 ) (sem_Ty _ty ) )
sem_Tm (TyLam _a _t1 )  =
    (sem_Tm_TyLam _a (sem_Tm _t1 ) )
sem_Tm (Var _x )  =
    (sem_Tm_Var _x )
-- semantic domain
type T_Tm  = ( )
data Inh_Tm  = Inh_Tm {}
data Syn_Tm  = Syn_Tm {}
wrap_Tm :: T_Tm  ->
           Inh_Tm  ->
           Syn_Tm 
wrap_Tm sem (Inh_Tm )  =
    (let ( ) = sem 
     in  (Syn_Tm ))
sem_Tm_App :: T_Tm  ->
              T_Tm  ->
              T_Tm 
sem_Tm_App t1_ t2_  =
    (let 
     in  ( ))
sem_Tm_Lam :: Var ->
              T_Ty  ->
              T_Tm  ->
              T_Tm 
sem_Tm_Lam x_ ty_ t1_  =
    (let _tyIftv :: (Set TyVar)
         _tyIstringRep :: String
         ( _tyIftv,_tyIstringRep) =
             ty_ 
     in  ( ))
sem_Tm_TyApp :: T_Tm  ->
                T_Ty  ->
                T_Tm 
sem_Tm_TyApp t1_ ty_  =
    (let _tyIftv :: (Set TyVar)
         _tyIstringRep :: String
         ( _tyIftv,_tyIstringRep) =
             ty_ 
     in  ( ))
sem_Tm_TyLam :: TyVar ->
                T_Tm  ->
                T_Tm 
sem_Tm_TyLam a_ t1_  =
    (let 
     in  ( ))
sem_Tm_Var :: Var ->
              T_Tm 
sem_Tm_Var x_  =
    (let 
     in  ( ))
-- Ty ----------------------------------------------------------
data Ty  = Arr (Ty ) (Ty ) 
         | Forall (TyVar) (Ty ) 
         | TyVar (TyVar) 
-- cata
sem_Ty :: Ty  ->
          T_Ty 
sem_Ty (Arr _ty1 _ty2 )  =
    (sem_Ty_Arr (sem_Ty _ty1 ) (sem_Ty _ty2 ) )
sem_Ty (Forall _a _ty1 )  =
    (sem_Ty_Forall _a (sem_Ty _ty1 ) )
sem_Ty (TyVar _a )  =
    (sem_Ty_TyVar _a )
-- semantic domain
type T_Ty  = ( (Set TyVar),String)
data Inh_Ty  = Inh_Ty {}
data Syn_Ty  = Syn_Ty {ftv_Syn_Ty :: (Set TyVar),stringRep_Syn_Ty :: String}
wrap_Ty :: T_Ty  ->
           Inh_Ty  ->
           Syn_Ty 
wrap_Ty sem (Inh_Ty )  =
    (let ( _lhsOftv,_lhsOstringRep) = sem 
     in  (Syn_Ty _lhsOftv _lhsOstringRep ))
sem_Ty_Arr :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Arr ty1_ ty2_  =
    (let _lhsOftv :: (Set TyVar)
         _lhsOstringRep :: String
         _ty1Iftv :: (Set TyVar)
         _ty1IstringRep :: String
         _ty2Iftv :: (Set TyVar)
         _ty2IstringRep :: String
         _lhsOftv =
             ({-# LINE 29 "CCO/HM2SystemF.ag" #-}
              _ty1Iftv `S.union` _ty2Iftv
              {-# LINE 233 "CCO/HM2SystemF.hs" #-}
              )
         _lhsOstringRep =
             ({-# LINE 30 "CCO/HM2SystemF.ag" #-}
              "(" ++ _ty1IstringRep ++ " -> " ++ _ty2IstringRep ++ ")"
              {-# LINE 238 "CCO/HM2SystemF.hs" #-}
              )
         ( _ty1Iftv,_ty1IstringRep) =
             ty1_ 
         ( _ty2Iftv,_ty2IstringRep) =
             ty2_ 
     in  ( _lhsOftv,_lhsOstringRep))
sem_Ty_Forall :: TyVar ->
                 T_Ty  ->
                 T_Ty 
sem_Ty_Forall a_ ty1_  =
    (let _lhsOftv :: (Set TyVar)
         _lhsOstringRep :: String
         _ty1Iftv :: (Set TyVar)
         _ty1IstringRep :: String
         _lhsOftv =
             ({-# LINE 31 "CCO/HM2SystemF.ag" #-}
              S.delete a_ _ty1Iftv
              {-# LINE 256 "CCO/HM2SystemF.hs" #-}
              )
         _lhsOstringRep =
             ({-# LINE 32 "CCO/HM2SystemF.ag" #-}
              "(forall " ++ a_ ++ ". " ++ _ty1IstringRep ++ ")"
              {-# LINE 261 "CCO/HM2SystemF.hs" #-}
              )
         ( _ty1Iftv,_ty1IstringRep) =
             ty1_ 
     in  ( _lhsOftv,_lhsOstringRep))
sem_Ty_TyVar :: TyVar ->
                T_Ty 
sem_Ty_TyVar a_  =
    (let _lhsOftv :: (Set TyVar)
         _lhsOstringRep :: String
         _lhsOftv =
             ({-# LINE 27 "CCO/HM2SystemF.ag" #-}
              S.singleton a_
              {-# LINE 274 "CCO/HM2SystemF.hs" #-}
              )
         _lhsOstringRep =
             ({-# LINE 28 "CCO/HM2SystemF.ag" #-}
              a_
              {-# LINE 279 "CCO/HM2SystemF.hs" #-}
              )
     in  ( _lhsOftv,_lhsOstringRep))
-- TyEnv -------------------------------------------------------
data TyEnv  = ConsTyEnv (Var) (Ty ) (TyEnv ) 
            | EmptyTyEnv 
-- cata
sem_TyEnv :: TyEnv  ->
             T_TyEnv 
sem_TyEnv (ConsTyEnv _var _binding _envTail )  =
    (sem_TyEnv_ConsTyEnv _var (sem_Ty _binding ) (sem_TyEnv _envTail ) )
sem_TyEnv (EmptyTyEnv )  =
    (sem_TyEnv_EmptyTyEnv )
-- semantic domain
type T_TyEnv  = ( (Set TyVar),String)
data Inh_TyEnv  = Inh_TyEnv {}
data Syn_TyEnv  = Syn_TyEnv {ftv_Syn_TyEnv :: (Set TyVar),stringRep_Syn_TyEnv :: String}
wrap_TyEnv :: T_TyEnv  ->
              Inh_TyEnv  ->
              Syn_TyEnv 
wrap_TyEnv sem (Inh_TyEnv )  =
    (let ( _lhsOftv,_lhsOstringRep) = sem 
     in  (Syn_TyEnv _lhsOftv _lhsOstringRep ))
sem_TyEnv_ConsTyEnv :: Var ->
                       T_Ty  ->
                       T_TyEnv  ->
                       T_TyEnv 
sem_TyEnv_ConsTyEnv var_ binding_ envTail_  =
    (let _lhsOftv :: (Set TyVar)
         _lhsOstringRep :: String
         _bindingIftv :: (Set TyVar)
         _bindingIstringRep :: String
         _envTailIftv :: (Set TyVar)
         _envTailIstringRep :: String
         _lhsOftv =
             ({-# LINE 37 "CCO/HM2SystemF.ag" #-}
              _bindingIftv `S.union` _envTailIftv
              {-# LINE 316 "CCO/HM2SystemF.hs" #-}
              )
         _lhsOstringRep =
             ({-# LINE 38 "CCO/HM2SystemF.ag" #-}
              "[" ++ var_ ++ " -> " ++ _bindingIstringRep ++ "]"
                          ++ _envTailIstringRep
              {-# LINE 322 "CCO/HM2SystemF.hs" #-}
              )
         ( _bindingIftv,_bindingIstringRep) =
             binding_ 
         ( _envTailIftv,_envTailIstringRep) =
             envTail_ 
     in  ( _lhsOftv,_lhsOstringRep))
sem_TyEnv_EmptyTyEnv :: T_TyEnv 
sem_TyEnv_EmptyTyEnv  =
    (let _lhsOftv :: (Set TyVar)
         _lhsOstringRep :: String
         _lhsOftv =
             ({-# LINE 35 "CCO/HM2SystemF.ag" #-}
              S.empty
              {-# LINE 336 "CCO/HM2SystemF.hs" #-}
              )
         _lhsOstringRep =
             ({-# LINE 36 "CCO/HM2SystemF.ag" #-}
              "[]"
              {-# LINE 341 "CCO/HM2SystemF.hs" #-}
              )
     in  ( _lhsOftv,_lhsOstringRep))