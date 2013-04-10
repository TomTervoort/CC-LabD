

-- UUAGC 0.9.42.3 (AG.ag)
module CCO.SystemF.AG where

{-# LINE 2 ".\\AG/Base.ag" #-}

import CCO.SourcePos

import Data.Set (Set)
import qualified Data.Set as S
{-# LINE 13 "AG.hs" #-}
{-# LINE 13 ".\\AG/Base.ag" #-}

type TyVar = String    -- ^ Type of type variables. 
type Var   = String    -- ^ Type of variables.
{-# LINE 18 "AG.hs" #-}

{-# LINE 10 ".\\AG/TyEnv.ag" #-}

-- | The empty type environment.
emptyEnv :: TyEnv
emptyEnv = EmptyTyEnv

-- | Look up a variable in an environment.
lookupEnv :: Var -> TyEnv -> Maybe Ty
lookupEnv _ EmptyTyEnv = Nothing
lookupEnv x (ConsTyEnv y t rest) | x == y    = Just t
								 | otherwise = lookupEnv x rest

-- | Insert a binding into an environment.
insertEnv :: Var -> Ty -> TyEnv -> TyEnv
insertEnv = ConsTyEnv
{-# LINE 35 "AG.hs" #-}
-- Tm ----------------------------------------------------------
data Tm = Var (Var)
        | Lam (Var) (Ty) (Tm)
        | App (Tm) (Tm)
        | TyLam (TyVar) (Tm)
        | TyApp (Tm) (Ty)
-- cata
sem_Tm :: Tm ->
          T_Tm
sem_Tm (Var _x) =
    (sem_Tm_Var _x)
sem_Tm (Lam _x _ty _t1) =
    (sem_Tm_Lam _x (sem_Ty _ty) (sem_Tm _t1))
sem_Tm (App _t1 _t2) =
    (sem_Tm_App (sem_Tm _t1) (sem_Tm _t2))
sem_Tm (TyLam _a _t1) =
    (sem_Tm_TyLam _a (sem_Tm _t1))
sem_Tm (TyApp _t1 _ty) =
    (sem_Tm_TyApp (sem_Tm _t1) (sem_Ty _ty))
-- semantic domain
type T_Tm = ( )
data Inh_Tm = Inh_Tm {}
data Syn_Tm = Syn_Tm {}
wrap_Tm :: T_Tm ->
           Inh_Tm ->
           Syn_Tm
wrap_Tm sem (Inh_Tm) =
    (let ( ) = sem
     in  (Syn_Tm))
sem_Tm_Var :: Var ->
              T_Tm
sem_Tm_Var x_ =
    (let
     in  ( ))
sem_Tm_Lam :: Var ->
              T_Ty ->
              T_Tm ->
              T_Tm
sem_Tm_Lam x_ ty_ t1_ =
    (let _tyIftv :: (Set TyVar)
         _tyIstringRep :: String
         ( _tyIftv,_tyIstringRep) =
             ty_
     in  ( ))
sem_Tm_App :: T_Tm ->
              T_Tm ->
              T_Tm
sem_Tm_App t1_ t2_ =
    (let
     in  ( ))
sem_Tm_TyLam :: TyVar ->
                T_Tm ->
                T_Tm
sem_Tm_TyLam a_ t1_ =
    (let
     in  ( ))
sem_Tm_TyApp :: T_Tm ->
                T_Ty ->
                T_Tm
sem_Tm_TyApp t1_ ty_ =
    (let _tyIftv :: (Set TyVar)
         _tyIstringRep :: String
         ( _tyIftv,_tyIstringRep) =
             ty_
     in  ( ))
-- Ty ----------------------------------------------------------
data Ty = TyVar (TyVar)
        | Arr (Ty) (Ty)
        | Forall (TyVar) (Ty)
-- cata
sem_Ty :: Ty ->
          T_Ty
sem_Ty (TyVar _a) =
    (sem_Ty_TyVar _a)
sem_Ty (Arr _ty1 _ty2) =
    (sem_Ty_Arr (sem_Ty _ty1) (sem_Ty _ty2))
sem_Ty (Forall _a _ty1) =
    (sem_Ty_Forall _a (sem_Ty _ty1))
-- semantic domain
type T_Ty = ( (Set TyVar),String)
data Inh_Ty = Inh_Ty {}
data Syn_Ty = Syn_Ty {ftv_Syn_Ty :: (Set TyVar),stringRep_Syn_Ty :: String}
wrap_Ty :: T_Ty ->
           Inh_Ty ->
           Syn_Ty
wrap_Ty sem (Inh_Ty) =
    (let ( _lhsOftv,_lhsOstringRep) = sem
     in  (Syn_Ty _lhsOftv _lhsOstringRep))
sem_Ty_TyVar :: TyVar ->
                T_Ty
sem_Ty_TyVar a_ =
    (let _lhsOftv :: (Set TyVar)
         _lhsOstringRep :: String
         _lhsOftv =
             ({-# LINE 10 ".\\AG/Semantics.ag" #-}
              S.singleton a_
              {-# LINE 132 "AG.hs" #-}
              )
         _lhsOstringRep =
             ({-# LINE 11 ".\\AG/Semantics.ag" #-}
              a_
              {-# LINE 137 "AG.hs" #-}
              )
     in  ( _lhsOftv,_lhsOstringRep))
sem_Ty_Arr :: T_Ty ->
              T_Ty ->
              T_Ty
sem_Ty_Arr ty1_ ty2_ =
    (let _lhsOftv :: (Set TyVar)
         _lhsOstringRep :: String
         _ty1Iftv :: (Set TyVar)
         _ty1IstringRep :: String
         _ty2Iftv :: (Set TyVar)
         _ty2IstringRep :: String
         _lhsOftv =
             ({-# LINE 12 ".\\AG/Semantics.ag" #-}
              _ty1Iftv `S.union` _ty2Iftv
              {-# LINE 153 "AG.hs" #-}
              )
         _lhsOstringRep =
             ({-# LINE 13 ".\\AG/Semantics.ag" #-}
              "(" ++ _ty1IstringRep ++ " -> " ++ _ty2IstringRep ++ ")"
              {-# LINE 158 "AG.hs" #-}
              )
         ( _ty1Iftv,_ty1IstringRep) =
             ty1_
         ( _ty2Iftv,_ty2IstringRep) =
             ty2_
     in  ( _lhsOftv,_lhsOstringRep))
sem_Ty_Forall :: TyVar ->
                 T_Ty ->
                 T_Ty
sem_Ty_Forall a_ ty1_ =
    (let _lhsOftv :: (Set TyVar)
         _lhsOstringRep :: String
         _ty1Iftv :: (Set TyVar)
         _ty1IstringRep :: String
         _lhsOftv =
             ({-# LINE 14 ".\\AG/Semantics.ag" #-}
              S.delete a_ _ty1Iftv
              {-# LINE 176 "AG.hs" #-}
              )
         _lhsOstringRep =
             ({-# LINE 15 ".\\AG/Semantics.ag" #-}
              "(forall " ++ a_ ++ ". " ++ _ty1IstringRep ++ ")"
              {-# LINE 181 "AG.hs" #-}
              )
         ( _ty1Iftv,_ty1IstringRep) =
             ty1_
     in  ( _lhsOftv,_lhsOstringRep))
-- TyEnv -------------------------------------------------------
data TyEnv = EmptyTyEnv
           | ConsTyEnv (Var) (Ty) (TyEnv)
-- cata
sem_TyEnv :: TyEnv ->
             T_TyEnv
sem_TyEnv (EmptyTyEnv) =
    (sem_TyEnv_EmptyTyEnv)
sem_TyEnv (ConsTyEnv _var _binding _envTail) =
    (sem_TyEnv_ConsTyEnv _var (sem_Ty _binding) (sem_TyEnv _envTail))
-- semantic domain
type T_TyEnv = ( (Set TyVar),String)
data Inh_TyEnv = Inh_TyEnv {}
data Syn_TyEnv = Syn_TyEnv {ftv_Syn_TyEnv :: (Set TyVar),stringRep_Syn_TyEnv :: String}
wrap_TyEnv :: T_TyEnv ->
              Inh_TyEnv ->
              Syn_TyEnv
wrap_TyEnv sem (Inh_TyEnv) =
    (let ( _lhsOftv,_lhsOstringRep) = sem
     in  (Syn_TyEnv _lhsOftv _lhsOstringRep))
sem_TyEnv_EmptyTyEnv :: T_TyEnv
sem_TyEnv_EmptyTyEnv =
    (let _lhsOftv :: (Set TyVar)
         _lhsOstringRep :: String
         _lhsOftv =
             ({-# LINE 18 ".\\AG/Semantics.ag" #-}
              S.empty
              {-# LINE 213 "AG.hs" #-}
              )
         _lhsOstringRep =
             ({-# LINE 19 ".\\AG/Semantics.ag" #-}
              "[]"
              {-# LINE 218 "AG.hs" #-}
              )
     in  ( _lhsOftv,_lhsOstringRep))
sem_TyEnv_ConsTyEnv :: Var ->
                       T_Ty ->
                       T_TyEnv ->
                       T_TyEnv
sem_TyEnv_ConsTyEnv var_ binding_ envTail_ =
    (let _lhsOftv :: (Set TyVar)
         _lhsOstringRep :: String
         _bindingIftv :: (Set TyVar)
         _bindingIstringRep :: String
         _envTailIftv :: (Set TyVar)
         _envTailIstringRep :: String
         _lhsOftv =
             ({-# LINE 20 ".\\AG/Semantics.ag" #-}
              _bindingIftv `S.union` _envTailIftv
              {-# LINE 235 "AG.hs" #-}
              )
         _lhsOstringRep =
             ({-# LINE 21 ".\\AG/Semantics.ag" #-}
              "[" ++ var_ ++ " -> " ++ _bindingIstringRep ++ "]"
                          ++ _envTailIstringRep
              {-# LINE 241 "AG.hs" #-}
              )
         ( _bindingIftv,_bindingIstringRep) =
             binding_
         ( _envTailIftv,_envTailIstringRep) =
             envTail_
     in  ( _lhsOftv,_lhsOstringRep))