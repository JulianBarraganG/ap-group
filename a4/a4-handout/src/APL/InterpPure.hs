module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval evalm =
  let (ps, res) = runEval' envEmpty stateInitial evalm
   in (ps, fst <$> res)
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error (a, State))
    runEval' _ s (Pure x) = ([], pure (x, s))
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' r s (Free (TryCatchOp m1 m2 k)) =
      case runEval' r s m1 of
        (p1', Right (x, s1)) -> let (ps, res) = runEval' r s1 $ k x in (p1' ++ ps, res)
        (p1, Left _) -> case runEval' r s m2 of
          (p2', Right (x, s2)) -> let (ps, res) = runEval' r s2 $ k x in (p1 ++ p2' ++ ps, res)
          (p2, Left e) ->  ((p1 ++ p2), Left e)
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> ([], Left "Key not in state")
        Just x -> runEval' r s $ k x
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = ((key, val) : (filter ((key /=).fst) s))
        in runEval' r s' m
    runEval' r s (Free (TransactionOp m k)) =
      case runEval' r s m of
        (p1, Right (x, s')) -> let (ps, res) = runEval' r s' $ k x in (p1 ++ ps, res)
        (p1, Left e) -> (p1, Left e)
