

-- UUAGC 0.9.38.1 (CCO/HM2SystemF.ag)

{-# LINE 5 "CCO/HM2SystemF.ag" #-}


import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow

{-# LINE 13 "CCO/HM2SystemF.hs" #-}

{-# LINE 2 "./CCO/SystemF/AG/Base.ag" #-}

import CCO.SourcePos
{-# LINE 18 "CCO/HM2SystemF.hs" #-}
{-# LINE 29 "CCO/HM2SystemF.ag" #-}


-- | Represents types that only have existential quantification at the top-level.
type TyScheme = Ty

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

gen :: TyEnv -> Ty -> TyScheme
gen env ty = S.foldr Forall ty $ ftvTy `S.difference` ftvEnv
 where ftvTy = ftv_Syn_Ty $ wrap_Ty (sem_Ty ty) Inh_Ty
       ftvEnv = ftv_Syn_TyEnv $ wrap_TyEnv (sem_TyEnv env) Inh_TyEnv

inst :: VarFactory -> TyScheme -> (Ty, VarFactory)
inst gen (Forall a t) = let (fresh, gen') = freshTyVar gen
                         in first (subTyVar a fresh) $ inst gen' t
inst gen ty = (ty, gen)

-- | Substitute one type variable with another inside a type.
subTyVar :: TyVar -> TyVar -> Ty -> Ty
subTyVar from to = sub
 where sub ty = case ty of
                 TyVar v    | v == from -> TyVar to
                            | otherwise -> TyVar v
                 Arr t1 t2  -> Arr (sub t1) (sub t2)
                 Forall a t -> Forall a $ sub t

{-# LINE 59 "CCO/HM2SystemF.hs" #-}

{-# LINE 10 "./CCO/SystemF/AG/Base.ag" #-}

type TyVar = String    -- ^ Type of type variables. 
type Var   = String    -- ^ Type of variables.
{-# LINE 65 "CCO/HM2SystemF.hs" #-}
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
         ( _tyIftv) =
             ty_ 
     in  ( ))
sem_Tm_TyApp :: T_Tm  ->
                T_Ty  ->
                T_Tm 
sem_Tm_TyApp t1_ ty_  =
    (let _tyIftv :: (Set TyVar)
         ( _tyIftv) =
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
type T_Ty  = ( (Set TyVar))
data Inh_Ty  = Inh_Ty {}
data Syn_Ty  = Syn_Ty {ftv_Syn_Ty :: (Set TyVar)}
wrap_Ty :: T_Ty  ->
           Inh_Ty  ->
           Syn_Ty 
wrap_Ty sem (Inh_Ty )  =
    (let ( _lhsOftv) = sem 
     in  (Syn_Ty _lhsOftv ))
sem_Ty_Arr :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Arr ty1_ ty2_  =
    (let _lhsOftv :: (Set TyVar)
         _ty1Iftv :: (Set TyVar)
         _ty2Iftv :: (Set TyVar)
         _lhsOftv =
             ({-# LINE 22 "CCO/HM2SystemF.ag" #-}
              _ty1Iftv `S.union` _ty2Iftv
              {-# LINE 162 "CCO/HM2SystemF.hs" #-}
              )
         ( _ty1Iftv) =
             ty1_ 
         ( _ty2Iftv) =
             ty2_ 
     in  ( _lhsOftv))
sem_Ty_Forall :: TyVar ->
                 T_Ty  ->
                 T_Ty 
sem_Ty_Forall a_ ty1_  =
    (let _lhsOftv :: (Set TyVar)
         _ty1Iftv :: (Set TyVar)
         _lhsOftv =
             ({-# LINE 23 "CCO/HM2SystemF.ag" #-}
              S.delete a_ _ty1Iftv
              {-# LINE 178 "CCO/HM2SystemF.hs" #-}
              )
         ( _ty1Iftv) =
             ty1_ 
     in  ( _lhsOftv))
sem_Ty_TyVar :: TyVar ->
                T_Ty 
sem_Ty_TyVar a_  =
    (let _lhsOftv :: (Set TyVar)
         _lhsOftv =
             ({-# LINE 21 "CCO/HM2SystemF.ag" #-}
              S.singleton a_
              {-# LINE 190 "CCO/HM2SystemF.hs" #-}
              )
     in  ( _lhsOftv))
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
type T_TyEnv  = ( (Set TyVar))
data Inh_TyEnv  = Inh_TyEnv {}
data Syn_TyEnv  = Syn_TyEnv {ftv_Syn_TyEnv :: (Set TyVar)}
wrap_TyEnv :: T_TyEnv  ->
              Inh_TyEnv  ->
              Syn_TyEnv 
wrap_TyEnv sem (Inh_TyEnv )  =
    (let ( _lhsOftv) = sem 
     in  (Syn_TyEnv _lhsOftv ))
sem_TyEnv_ConsTyEnv :: Var ->
                       T_Ty  ->
                       T_TyEnv  ->
                       T_TyEnv 
sem_TyEnv_ConsTyEnv var_ binding_ envTail_  =
    (let _lhsOftv :: (Set TyVar)
         _bindingIftv :: (Set TyVar)
         _envTailIftv :: (Set TyVar)
         _lhsOftv =
             ({-# LINE 27 "CCO/HM2SystemF.ag" #-}
              _bindingIftv `S.union` _envTailIftv
              {-# LINE 224 "CCO/HM2SystemF.hs" #-}
              )
         ( _bindingIftv) =
             binding_ 
         ( _envTailIftv) =
             envTail_ 
     in  ( _lhsOftv))
sem_TyEnv_EmptyTyEnv :: T_TyEnv 
sem_TyEnv_EmptyTyEnv  =
    (let _lhsOftv :: (Set TyVar)
         _lhsOftv =
             ({-# LINE 26 "CCO/HM2SystemF.ag" #-}
              S.empty
              {-# LINE 237 "CCO/HM2SystemF.hs" #-}
              )
     in  ( _lhsOftv))