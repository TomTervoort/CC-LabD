
-- | Contains the converter from a Hindley-Miller to a SystemF term.
module CCO.HM2SystemF where

import CCO.AlgorithmW (inferPrincipalType, TyEnv, emptyEnv, lookupEnv, insertEnv)

import qualified CCO.HM as HM
import qualified CCO.SystemF as SF

import CCO.Feedback


hm2SystemF :: HM.Tm -> Feedback SF.Tm
hm2SystemF = undefined