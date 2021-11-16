{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module RunFD4 where

import MonadFD4
import MonadEval
import Global
import Errors
import Eval

instance MonadEval FD4 where
   evaluate = eval

-- 'runFD4\'' corre una computación de la mónad 'FD4' en el estado inicial 'Global.initialEnv' 
runFD4' :: FD4 a -> IO (Either Error (a, GlEnv))
runFD4' c =  runExceptT $ runStateT c initialEnv

runFD4:: FD4 a -> IO (Either Error a)
runFD4 c = fmap fst <$> runFD4' c