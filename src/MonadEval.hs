{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MonadEval where

import MonadFD4
import Lang
import EvalCEK

class MonadFD4 m => MonadEval m where
   evaluate :: Term -> m Term

newtype EvalCEK a = CEK (FD4 a)
newtype EvalFD4 a = Eval (FD4 a)

-- instance MonadFD4 EvalCEK where

-- instance MonadEval EvalCEK where
--    evaluate = evalCEK