module APL.InterpPure (runEval) where

import APL.Monad

data Outcome a
  = Done a State
  | Failed Error
  | Broke Val State

runEval :: EvalM a -> ([String], Either Error a)
runEval evalm = 
  let (ps, res) = runEval' envEmpty stateInitial evalm
    in case res of
      (Done x _) -> (ps, Right x)
      (Failed e) -> (ps, Left e)
      (Broke {}) -> (ps, Left "Break outside loop")
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Outcome a)
    runEval' _ s (Pure x) = ([], Done x s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Failed e)
    runEval' r s (Free (TryCatchOp m1 m2 k)) =
      case runEval' r s m1 of
        (p1', Done x s1) -> let (ps, res) = runEval' r s1 $ k x in (p1' ++ ps, res)
        (p1, Failed _) -> case runEval' r s m2 of
          (p2', Done x s2) -> let (ps, res) = runEval' r s2 $ k x in (p1 ++ p2' ++ ps, res)
          (pb, Broke v sb) -> let (ps, res) = runEval' r sb $ k v in (p1 ++ pb ++ ps, res)
          (p2, Failed e) ->  ((p1 ++ p2), Failed e)
        (pb, Broke v' sb') -> let (ps, res) = runEval' r sb' $ k v' in (pb ++ ps, res)
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> ([], Failed "Key not in state")
        Just x -> runEval' r s $ k x
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = ((key, val) : (filter ((key /=).fst) s))
        in runEval' r s' m
    runEval' r s (Free (TransactionOp m k)) =
      case runEval' r s m of
        (p1, Done x s') -> let (ps, res) = runEval' r s' $ k x in (p1 ++ ps, res)
        (pb, Broke v _) -> let (ps, res) = runEval' r s $ k v in (pb ++ ps, res)
        (p1, Failed e) -> (p1, Failed e)
    runEval' _ s (Free (BreakOp v)) = ([], Broke v s)
    runEval' r s (Free (LoopOp m k)) =
      case runEval' r s m of
        (p, Done x s') -> let (ps, res) = runEval' r s' $ k x in (p ++ ps, res)
        (pb, Broke v sb) -> let (ps, res) = runEval' r sb $ k v in (pb ++ ps, res)
        (p', Failed e) -> (p', Failed e)
