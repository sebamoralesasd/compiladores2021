module EvalCEK where

--import MonadEval
import MonadFD4
import Lang
import CEK
import Common

-- newtype FDCEK a = CEK (FD4 a)

-- instance MonadEval FDCEK where
--    evaluate = evalCEK

evalCEK :: MonadFD4 m => Term -> m Term
evalCEK t = do v <- interactive $ TermEnviromentKontinuation t [] []
            --do v <- search t [] []
               return $ toTerm v
            where
              toTerm (Natural n) = Const NoPos (CNat n)
              toTerm (ClosureValue clos) =
                case clos of
                  ClosureFun _ _ term -> term
                  ClosureFix _ _ _ term -> term