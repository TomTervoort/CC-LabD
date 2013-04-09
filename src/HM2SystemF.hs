
-- | Contains the converter from a Hindley-Miller to a SystemF term.
module CCO.HM2SystemF where

import CCO.AlgorithmW (inferPrincipalType, TyEnv, emptyEnv, Ty)

import qualified CCO.HM as HM
import qualified CCO.SystemF as SF

import CCO.Feedback
import CCO.Component hiding (parser)
import CCO.Tree

import Control.Monad
import Control.Arrow

--TODO: CCO.SystemF correct importeren binnen AlgorithmW, zodat er niet twee versies van Ty zijn.
(???) :: CCO.AlgorithmW.Ty -> SF.Ty
(???) = undefined


-- TODO: Polymorfe type-lambda's en applicaties.
hm2SystemF :: HM.Tm -> Feedback SF.Tm
hm2SystemF = h2s emptyEnv
 where h2s env (HM.Tm _ term) = 
        case term of
         HM.Var x       -> return $ SF.Var x
         HM.App t1 t2   -> liftM2 SF.App (h2s env t1) (h2s env t2) -- TODO: TyApp toevoegen als t1 polymorf is
         HM.Lam x t1    -> do (ty, env') <- liftM (first (???)) $ inferPrincipalType t1 env
                              liftM (SF.Lam x ty) $ h2s env' t1
         HM.Let x t1 t2 -> do (ty1, env') <- liftM (first (???)) $ inferPrincipalType t1 env
                              (ty2, env') <- liftM (first (???)) $ inferPrincipalType t2 env'
                              liftM2 SF.App (liftM (SF.Lam x ty1) $ h2s env' t2) (h2s env' t1)


main :: IO ()
main = ioWrap $ parser >>> component (toTree >=> hm2SystemF) >>> arr fromTree >>> printer