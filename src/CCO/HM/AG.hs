

-- UUAGC 0.9.8 (src/CCO/HM/AG.ag)
module CCO.HM.AG where

{-# LINE 2 "src/CCO/HM/AG/Base.ag" #-}

import CCO.SourcePos
{-# LINE 10 "src/CCO/HM/AG.hs" #-}
{-# LINE 10 "src/CCO/HM/AG/Base.ag" #-}

type Var = String    -- ^ Type of variables.
{-# LINE 14 "src/CCO/HM/AG.hs" #-}
-- Tm ----------------------------------------------------------
data Tm  = Tm (SourcePos) (Tm_) 
-- cata
sem_Tm :: Tm  ->
          T_Tm 
sem_Tm (Tm _pos _t )  =
    (sem_Tm_Tm _pos (sem_Tm_ _t ) )
-- semantic domain
type T_Tm  = ( Tm)
data Inh_Tm  = Inh_Tm {}
data Syn_Tm  = Syn_Tm {self_Syn_Tm :: Tm}
wrap_Tm :: T_Tm  ->
           Inh_Tm  ->
           Syn_Tm 
wrap_Tm sem (Inh_Tm )  =
    (let ( _lhsOself) =
             (sem )
     in  (Syn_Tm _lhsOself ))
sem_Tm_Tm :: SourcePos ->
             T_Tm_  ->
             T_Tm 
sem_Tm_Tm pos_ t_  =
    (let _lhsOself :: Tm
         _tIself :: Tm_
         _self =
             Tm pos_ _tIself
         _lhsOself =
             _self
         ( _tIself) =
             (t_ )
     in  ( _lhsOself))
-- Tm_ ---------------------------------------------------------
data Tm_  = App (Tm) (Tm) 
          | Lam (Var) (Tm) 
          | Let (Var) (Tm) (Tm) 
          | Var (Var) 
-- cata
sem_Tm_ :: Tm_  ->
           T_Tm_ 
sem_Tm_ (App _t1 _t2 )  =
    (sem_Tm__App (sem_Tm _t1 ) (sem_Tm _t2 ) )
sem_Tm_ (Lam _x _t1 )  =
    (sem_Tm__Lam _x (sem_Tm _t1 ) )
sem_Tm_ (Let _x _t1 _t2 )  =
    (sem_Tm__Let _x (sem_Tm _t1 ) (sem_Tm _t2 ) )
sem_Tm_ (Var _x )  =
    (sem_Tm__Var _x )
-- semantic domain
type T_Tm_  = ( Tm_)
data Inh_Tm_  = Inh_Tm_ {}
data Syn_Tm_  = Syn_Tm_ {self_Syn_Tm_ :: Tm_}
wrap_Tm_ :: T_Tm_  ->
            Inh_Tm_  ->
            Syn_Tm_ 
wrap_Tm_ sem (Inh_Tm_ )  =
    (let ( _lhsOself) =
             (sem )
     in  (Syn_Tm_ _lhsOself ))
sem_Tm__App :: T_Tm  ->
               T_Tm  ->
               T_Tm_ 
sem_Tm__App t1_ t2_  =
    (let _lhsOself :: Tm_
         _t1Iself :: Tm
         _t2Iself :: Tm
         _self =
             App _t1Iself _t2Iself
         _lhsOself =
             _self
         ( _t1Iself) =
             (t1_ )
         ( _t2Iself) =
             (t2_ )
     in  ( _lhsOself))
sem_Tm__Lam :: Var ->
               T_Tm  ->
               T_Tm_ 
sem_Tm__Lam x_ t1_  =
    (let _lhsOself :: Tm_
         _t1Iself :: Tm
         _self =
             Lam x_ _t1Iself
         _lhsOself =
             _self
         ( _t1Iself) =
             (t1_ )
     in  ( _lhsOself))
sem_Tm__Let :: Var ->
               T_Tm  ->
               T_Tm  ->
               T_Tm_ 
sem_Tm__Let x_ t1_ t2_  =
    (let _lhsOself :: Tm_
         _t1Iself :: Tm
         _t2Iself :: Tm
         _self =
             Let x_ _t1Iself _t2Iself
         _lhsOself =
             _self
         ( _t1Iself) =
             (t1_ )
         ( _t2Iself) =
             (t2_ )
     in  ( _lhsOself))
sem_Tm__Var :: Var ->
               T_Tm_ 
sem_Tm__Var x_  =
    (let _lhsOself :: Tm_
         _self =
             Var x_
         _lhsOself =
             _self
     in  ( _lhsOself))