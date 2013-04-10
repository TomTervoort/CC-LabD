

-- UUAGC 0.9.38.1 (AG.ag)
module CCO.SystemF.AG where

{-# LINE 2 "./AG/Base.ag" #-}

import CCO.SourcePos

import Data.Set (Set)
import qualified Data.Set as S
{-# LINE 13 "AG.hs" #-}

{-# LINE 2 "./AG/Printing.ag" #-}

import CCO.Printing
{-# LINE 18 "AG.hs" #-}
{-# LINE 13 "./AG/Base.ag" #-}

type TyVar = String    -- ^ Type of type variables. 
type Var   = String    -- ^ Type of variables.
{-# LINE 23 "AG.hs" #-}

{-# LINE 10 "./AG/TyEnv.ag" #-}


instance Show Ty where
 show ty = stringRep_Syn_Ty $ wrap_Ty (sem_Ty ty) (Inh_Ty 0)

instance Show TyEnv where
 show env = stringRep_Syn_TyEnv $ wrap_TyEnv (sem_TyEnv env) (Inh_TyEnv 0)

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

{-# LINE 48 "AG.hs" #-}

{-# LINE 25 "./AG/Semantics.ag" #-}


-- | Provides the free type variables within a type.
freeVars :: Ty -> Set TyVar
freeVars ty = ftv_Syn_Ty $ wrap_Ty (sem_Ty ty) (Inh_Ty 0)

-- | Provides the free type variables within a type environment.
freeEnvVars :: TyEnv -> Set TyVar
freeEnvVars env = ftv_Syn_TyEnv $ wrap_TyEnv (sem_TyEnv env) (Inh_TyEnv 0)

-- | A type substitution.
type TySubst = Ty -> Ty

-- | Map a substitution over all the types within an environment.
mapEnv :: TySubst -> TyEnv -> TyEnv
mapEnv _ EmptyTyEnv = EmptyTyEnv
mapEnv f (ConsTyEnv x t rest) = ConsTyEnv x (f t) $ mapEnv f rest

{-# LINE 69 "AG.hs" #-}

{-# LINE 49 "./AG/Printing.ag" #-}

-- | Type of precedence levels.
type Prec = Int
{-# LINE 75 "AG.hs" #-}

{-# LINE 71 "./AG/Printing.ag" #-}

-- | Pretty prints in single-line mode, given the precedence level of its
-- immediate context, a term constructed from a binary operator of a specified
-- precedence level.
-- 
-- A term is enclosed in parentheses if the precedence level of its operator 
-- is less than the precedence level of the enclosing context.

ppInfixSL :: Prec -> (String, Prec) -> Doc -> Doc -> Doc
ppInfixSL ctx (op, prec) l r = modifier $ l >#< ppOp >#< r
  where
    modifier = if prec < ctx then parens else id
    ppOp     = text op

-- | Pretty prints in multiline mode, given the precedence level of its
-- immediate context, a term constructed from a binary operator of a specified
-- precedence level.
-- 
-- A term is enclosed in parentheses if the precedence level of its operator 
-- is less than the precedence level of the enclosing context.

ppInfixML :: Prec -> (String, Prec) -> Doc -> Doc -> Doc
ppInfixML ctx (op, prec) l r = modifier $ l >#< ppOp >-< r
  where
    modifier doc = if prec < ctx then (lparen >#< doc >-< rparen) else doc
    ppOp         = text op
{-# LINE 104 "AG.hs" #-}

{-# LINE 103 "./AG/Printing.ag" #-}

-- | Pretty prints in single-line mode, a construct involving a binder.
ppBinderSL :: String -> Doc -> Doc -> Doc
ppBinderSL binder arg body = text binder >|< arg >|< period >#< body

-- | Pretty prints in multiline mode, a construct involving a binder.
ppBinderML :: String -> Doc -> Doc -> Doc
ppBinderML binder arg body = text binder >|< arg >|< period >-< indent 2 body
{-# LINE 115 "AG.hs" #-}
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
type T_Tm  = Prec ->
             ( Doc,Doc)
data Inh_Tm  = Inh_Tm {prec_Inh_Tm :: Prec}
data Syn_Tm  = Syn_Tm {ppML_Syn_Tm :: Doc,ppSL_Syn_Tm :: Doc}
wrap_Tm :: T_Tm  ->
           Inh_Tm  ->
           Syn_Tm 
wrap_Tm sem (Inh_Tm _lhsIprec )  =
    (let ( _lhsOppML,_lhsOppSL) = sem _lhsIprec 
     in  (Syn_Tm _lhsOppML _lhsOppSL ))
sem_Tm_App :: T_Tm  ->
              T_Tm  ->
              T_Tm 
sem_Tm_App t1_ t2_  =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _t1Oprec :: Prec
              _t2Oprec :: Prec
              _lhsOppSL :: Doc
              _t1IppML :: Doc
              _t1IppSL :: Doc
              _t2IppML :: Doc
              _t2IppSL :: Doc
              _ppSL =
                  ({-# LINE 29 "./AG/Printing.ag" #-}
                   ppInfixSL _lhsIprec ("", 10) _t1IppSL _t2IppSL
                   {-# LINE 162 "AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 37 "./AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppInfixML _lhsIprec ("", 10) _t1IppML _t2IppML
                   {-# LINE 168 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 65 "./AG/Printing.ag" #-}
                   10
                   {-# LINE 173 "AG.hs" #-}
                   )
              _t2Oprec =
                  ({-# LINE 66 "./AG/Printing.ag" #-}
                   11
                   {-# LINE 178 "AG.hs" #-}
                   )
              _lhsOppSL =
                  ({-# LINE 11 "./AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 183 "AG.hs" #-}
                   )
              ( _t1IppML,_t1IppSL) =
                  t1_ _t1Oprec 
              ( _t2IppML,_t2IppSL) =
                  t2_ _t2Oprec 
          in  ( _lhsOppML,_lhsOppSL)))
sem_Tm_Lam :: Var ->
              T_Ty  ->
              T_Tm  ->
              T_Tm 
sem_Tm_Lam x_ ty_ t1_  =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _tyOprec :: Prec
              _t1Oprec :: Prec
              _lhsOppSL :: Doc
              _tyIftv :: (Set TyVar)
              _tyIppML :: Doc
              _tyIppSL :: Doc
              _tyIstringRep :: String
              _t1IppML :: Doc
              _t1IppSL :: Doc
              _ppSL =
                  ({-# LINE 27 "./AG/Printing.ag" #-}
                   ppBinderSL "\\" (text x_ >#< text ":" >#< _tyIppSL)
                     _t1IppSL
                   {-# LINE 210 "AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 34 "./AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppBinderML "\\" (text x_ >#< text ":" >#< _tyIppML)
                     _t1IppML
                   {-# LINE 217 "AG.hs" #-}
                   )
              _tyOprec =
                  ({-# LINE 63 "./AG/Printing.ag" #-}
                   0
                   {-# LINE 222 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 64 "./AG/Printing.ag" #-}
                   0
                   {-# LINE 227 "AG.hs" #-}
                   )
              _lhsOppSL =
                  ({-# LINE 11 "./AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 232 "AG.hs" #-}
                   )
              ( _tyIftv,_tyIppML,_tyIppSL,_tyIstringRep) =
                  ty_ _tyOprec 
              ( _t1IppML,_t1IppSL) =
                  t1_ _t1Oprec 
          in  ( _lhsOppML,_lhsOppSL)))
sem_Tm_TyApp :: T_Tm  ->
                T_Ty  ->
                T_Tm 
sem_Tm_TyApp t1_ ty_  =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _t1Oprec :: Prec
              _tyOprec :: Prec
              _lhsOppSL :: Doc
              _t1IppML :: Doc
              _t1IppSL :: Doc
              _tyIftv :: (Set TyVar)
              _tyIppML :: Doc
              _tyIppSL :: Doc
              _tyIstringRep :: String
              _ppSL =
                  ({-# LINE 31 "./AG/Printing.ag" #-}
                   ppInfixSL _lhsIprec ("", 10) _t1IppSL (brackets _tyIppSL)
                   {-# LINE 257 "AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 41 "./AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppInfixML _lhsIprec ("", 10) _t1IppML
                     (lbracket >#< _tyIppML >-< rbracket)
                   {-# LINE 264 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 68 "./AG/Printing.ag" #-}
                   9
                   {-# LINE 269 "AG.hs" #-}
                   )
              _tyOprec =
                  ({-# LINE 69 "./AG/Printing.ag" #-}
                   0
                   {-# LINE 274 "AG.hs" #-}
                   )
              _lhsOppSL =
                  ({-# LINE 11 "./AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 279 "AG.hs" #-}
                   )
              ( _t1IppML,_t1IppSL) =
                  t1_ _t1Oprec 
              ( _tyIftv,_tyIppML,_tyIppSL,_tyIstringRep) =
                  ty_ _tyOprec 
          in  ( _lhsOppML,_lhsOppSL)))
sem_Tm_TyLam :: TyVar ->
                T_Tm  ->
                T_Tm 
sem_Tm_TyLam a_ t1_  =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _t1Oprec :: Prec
              _lhsOppSL :: Doc
              _t1IppML :: Doc
              _t1IppSL :: Doc
              _ppSL =
                  ({-# LINE 30 "./AG/Printing.ag" #-}
                   ppBinderSL "/\\" (text a_) _t1IppSL
                   {-# LINE 299 "AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 39 "./AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppBinderML "/\\" (text a_) _t1IppML
                   {-# LINE 305 "AG.hs" #-}
                   )
              _t1Oprec =
                  ({-# LINE 67 "./AG/Printing.ag" #-}
                   0
                   {-# LINE 310 "AG.hs" #-}
                   )
              _lhsOppSL =
                  ({-# LINE 11 "./AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 315 "AG.hs" #-}
                   )
              ( _t1IppML,_t1IppSL) =
                  t1_ _t1Oprec 
          in  ( _lhsOppML,_lhsOppSL)))
sem_Tm_Var :: Var ->
              T_Tm 
sem_Tm_Var x_  =
    (\ _lhsIprec ->
         (let _lhsOppML :: Doc
              _lhsOppSL :: Doc
              _ppSL =
                  ({-# LINE 26 "./AG/Printing.ag" #-}
                   text x_
                   {-# LINE 329 "AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 33 "./AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 334 "AG.hs" #-}
                   )
              _lhsOppSL =
                  ({-# LINE 11 "./AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 339 "AG.hs" #-}
                   )
          in  ( _lhsOppML,_lhsOppSL)))
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
type T_Ty  = Prec ->
             ( (Set TyVar),Doc,Doc,String)
data Inh_Ty  = Inh_Ty {prec_Inh_Ty :: Prec}
data Syn_Ty  = Syn_Ty {ftv_Syn_Ty :: (Set TyVar),ppML_Syn_Ty :: Doc,ppSL_Syn_Ty :: Doc,stringRep_Syn_Ty :: String}
wrap_Ty :: T_Ty  ->
           Inh_Ty  ->
           Syn_Ty 
wrap_Ty sem (Inh_Ty _lhsIprec )  =
    (let ( _lhsOftv,_lhsOppML,_lhsOppSL,_lhsOstringRep) = sem _lhsIprec 
     in  (Syn_Ty _lhsOftv _lhsOppML _lhsOppSL _lhsOstringRep ))
sem_Ty_Arr :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_Arr ty1_ ty2_  =
    (\ _lhsIprec ->
         (let _lhsOftv :: (Set TyVar)
              _lhsOstringRep :: String
              _lhsOppML :: Doc
              _ty1Oprec :: Prec
              _ty2Oprec :: Prec
              _lhsOppSL :: Doc
              _ty1Iftv :: (Set TyVar)
              _ty1IppML :: Doc
              _ty1IppSL :: Doc
              _ty1IstringRep :: String
              _ty2Iftv :: (Set TyVar)
              _ty2IppML :: Doc
              _ty2IppSL :: Doc
              _ty2IstringRep :: String
              _lhsOftv =
                  ({-# LINE 12 "./AG/Semantics.ag" #-}
                   _ty1Iftv `S.union` _ty2Iftv
                   {-# LINE 388 "AG.hs" #-}
                   )
              _lhsOstringRep =
                  ({-# LINE 13 "./AG/Semantics.ag" #-}
                   "(" ++ _ty1IstringRep ++ " -> " ++ _ty2IstringRep ++ ")"
                   {-# LINE 393 "AG.hs" #-}
                   )
              _ppSL =
                  ({-# LINE 16 "./AG/Printing.ag" #-}
                   ppInfixSL _lhsIprec ("->", 0) _ty1IppSL _ty2IppSL
                   {-# LINE 398 "AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 20 "./AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppInfixML _lhsIprec ("->", 0) _ty1IppML _ty2IppML
                   {-# LINE 404 "AG.hs" #-}
                   )
              _ty1Oprec =
                  ({-# LINE 58 "./AG/Printing.ag" #-}
                   1
                   {-# LINE 409 "AG.hs" #-}
                   )
              _ty2Oprec =
                  ({-# LINE 59 "./AG/Printing.ag" #-}
                   0
                   {-# LINE 414 "AG.hs" #-}
                   )
              _lhsOppSL =
                  ({-# LINE 11 "./AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 419 "AG.hs" #-}
                   )
              ( _ty1Iftv,_ty1IppML,_ty1IppSL,_ty1IstringRep) =
                  ty1_ _ty1Oprec 
              ( _ty2Iftv,_ty2IppML,_ty2IppSL,_ty2IstringRep) =
                  ty2_ _ty2Oprec 
          in  ( _lhsOftv,_lhsOppML,_lhsOppSL,_lhsOstringRep)))
sem_Ty_Forall :: TyVar ->
                 T_Ty  ->
                 T_Ty 
sem_Ty_Forall a_ ty1_  =
    (\ _lhsIprec ->
         (let _lhsOftv :: (Set TyVar)
              _lhsOstringRep :: String
              _lhsOppML :: Doc
              _ty1Oprec :: Prec
              _lhsOppSL :: Doc
              _ty1Iftv :: (Set TyVar)
              _ty1IppML :: Doc
              _ty1IppSL :: Doc
              _ty1IstringRep :: String
              _lhsOftv =
                  ({-# LINE 14 "./AG/Semantics.ag" #-}
                   S.delete a_ _ty1Iftv
                   {-# LINE 443 "AG.hs" #-}
                   )
              _lhsOstringRep =
                  ({-# LINE 15 "./AG/Semantics.ag" #-}
                   "(forall " ++ a_ ++ ". " ++ _ty1IstringRep ++ ")"
                   {-# LINE 448 "AG.hs" #-}
                   )
              _ppSL =
                  ({-# LINE 17 "./AG/Printing.ag" #-}
                   ppBinderSL "forall " (text a_) _ty1IppSL
                   {-# LINE 453 "AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 22 "./AG/Printing.ag" #-}
                   _ppSL     >^<
                   ppBinderML "forall " (text a_) _ty1IppML
                   {-# LINE 459 "AG.hs" #-}
                   )
              _ty1Oprec =
                  ({-# LINE 60 "./AG/Printing.ag" #-}
                   0
                   {-# LINE 464 "AG.hs" #-}
                   )
              _lhsOppSL =
                  ({-# LINE 11 "./AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 469 "AG.hs" #-}
                   )
              ( _ty1Iftv,_ty1IppML,_ty1IppSL,_ty1IstringRep) =
                  ty1_ _ty1Oprec 
          in  ( _lhsOftv,_lhsOppML,_lhsOppSL,_lhsOstringRep)))
sem_Ty_TyVar :: TyVar ->
                T_Ty 
sem_Ty_TyVar a_  =
    (\ _lhsIprec ->
         (let _lhsOftv :: (Set TyVar)
              _lhsOstringRep :: String
              _lhsOppML :: Doc
              _lhsOppSL :: Doc
              _lhsOftv =
                  ({-# LINE 10 "./AG/Semantics.ag" #-}
                   S.singleton a_
                   {-# LINE 485 "AG.hs" #-}
                   )
              _lhsOstringRep =
                  ({-# LINE 11 "./AG/Semantics.ag" #-}
                   a_
                   {-# LINE 490 "AG.hs" #-}
                   )
              _ppSL =
                  ({-# LINE 15 "./AG/Printing.ag" #-}
                   text a_
                   {-# LINE 495 "AG.hs" #-}
                   )
              _lhsOppML =
                  ({-# LINE 19 "./AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 500 "AG.hs" #-}
                   )
              _lhsOppSL =
                  ({-# LINE 11 "./AG/Printing.ag" #-}
                   _ppSL
                   {-# LINE 505 "AG.hs" #-}
                   )
          in  ( _lhsOftv,_lhsOppML,_lhsOppSL,_lhsOstringRep)))
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
type T_TyEnv  = Prec ->
                ( (Set TyVar),String)
data Inh_TyEnv  = Inh_TyEnv {prec_Inh_TyEnv :: Prec}
data Syn_TyEnv  = Syn_TyEnv {ftv_Syn_TyEnv :: (Set TyVar),stringRep_Syn_TyEnv :: String}
wrap_TyEnv :: T_TyEnv  ->
              Inh_TyEnv  ->
              Syn_TyEnv 
wrap_TyEnv sem (Inh_TyEnv _lhsIprec )  =
    (let ( _lhsOftv,_lhsOstringRep) = sem _lhsIprec 
     in  (Syn_TyEnv _lhsOftv _lhsOstringRep ))
sem_TyEnv_ConsTyEnv :: Var ->
                       T_Ty  ->
                       T_TyEnv  ->
                       T_TyEnv 
sem_TyEnv_ConsTyEnv var_ binding_ envTail_  =
    (\ _lhsIprec ->
         (let _lhsOftv :: (Set TyVar)
              _lhsOstringRep :: String
              _bindingOprec :: Prec
              _envTailOprec :: Prec
              _bindingIftv :: (Set TyVar)
              _bindingIppML :: Doc
              _bindingIppSL :: Doc
              _bindingIstringRep :: String
              _envTailIftv :: (Set TyVar)
              _envTailIstringRep :: String
              _lhsOftv =
                  ({-# LINE 20 "./AG/Semantics.ag" #-}
                   _bindingIftv `S.union` _envTailIftv
                   {-# LINE 548 "AG.hs" #-}
                   )
              _lhsOstringRep =
                  ({-# LINE 21 "./AG/Semantics.ag" #-}
                   "[" ++ var_ ++ " -> " ++ _bindingIstringRep ++ "]"
                               ++ _envTailIstringRep
                   {-# LINE 554 "AG.hs" #-}
                   )
              _bindingOprec =
                  ({-# LINE 55 "./AG/Printing.ag" #-}
                   _lhsIprec
                   {-# LINE 559 "AG.hs" #-}
                   )
              _envTailOprec =
                  ({-# LINE 55 "./AG/Printing.ag" #-}
                   _lhsIprec
                   {-# LINE 564 "AG.hs" #-}
                   )
              ( _bindingIftv,_bindingIppML,_bindingIppSL,_bindingIstringRep) =
                  binding_ _bindingOprec 
              ( _envTailIftv,_envTailIstringRep) =
                  envTail_ _envTailOprec 
          in  ( _lhsOftv,_lhsOstringRep)))
sem_TyEnv_EmptyTyEnv :: T_TyEnv 
sem_TyEnv_EmptyTyEnv  =
    (\ _lhsIprec ->
         (let _lhsOftv :: (Set TyVar)
              _lhsOstringRep :: String
              _lhsOftv =
                  ({-# LINE 18 "./AG/Semantics.ag" #-}
                   S.empty
                   {-# LINE 579 "AG.hs" #-}
                   )
              _lhsOstringRep =
                  ({-# LINE 19 "./AG/Semantics.ag" #-}
                   "[]"
                   {-# LINE 584 "AG.hs" #-}
                   )
          in  ( _lhsOftv,_lhsOstringRep)))