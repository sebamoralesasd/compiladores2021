{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadEval where

import MonadFD4
import Lang
import CEK
--import EvalCEK
import Global
import Common
import Errors

class MonadFD4 m => MonadEval m where
   evaluate :: Term -> m Term

newtype EvalCEK a = CEK (FD4 a) 
   deriving (MonadFD4, MonadError Error, MonadIO, MonadState GlEnv, Monad, Applicative, Functor)

instance MonadEval EvalCEK where
   evaluate = evalCEK

-- 'runCEK\'' corre una computación de la mónad 'EvalCEK' en el estado inicial 'Global.initialEnv' 
runCEK' :: EvalCEK a -> IO (Either Error (a, GlEnv))
runCEK' (CEK c) = runExceptT $ runStateT c initialEnv

runCEK :: EvalCEK a -> IO (Either Error a)
runCEK c = fmap fst <$> runCEK' c

evalCEK :: MonadFD4 m => Term -> m Term
evalCEK t = do v <- interactive $ TermEnviromentKontinuation t [] []
            --do v <- search t [] []
               return $ toTerm v
            where
              toTerm (Natural n) = Const NoPos (CNat n)
              toTerm (ClosureValue clos) =
                case clos of
                  Closure env term -> term