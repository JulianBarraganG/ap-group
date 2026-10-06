module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

data Outcome a
  = Done a
  | Failed Error
  | Broke Val

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  res <- runEvalIO' envEmpty dbFile evalm
  case res of
    Done x -> pure $ Right x
    Broke _ -> pure $ Left "Break outside loop"
    Failed e -> pure $ Left e
 
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Outcome a)
    runEvalIO' _ _ (Pure x) = pure $ Done x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Failed e
    runEvalIO' r db (Free (TryCatchOp m1 m2 k)) = do
      val1 <- runEvalIO' r db m1
      case val1 of
        Done x -> runEvalIO' r db $ k x
        Broke vc -> pure $ Broke vc
        Failed _ -> do
          val2 <- runEvalIO' r db m2
          case val2 of
            Done y -> runEvalIO' r db $ k y
            Broke vy -> pure $ Broke vy
            Failed e -> pure $ Failed e
    runEvalIO' r db (Free (KvGetOp key k)) = do
      readState <- readDB db
      case readState of
        Left e -> pure $ Failed e
        Right state ->
          case lookup key state of
            Nothing -> do
              newVal <- prompt $ "Invalid key: " ++ (show key) ++ " Enter a replacement: "
              case readVal newVal of
                Nothing -> pure $ Failed $ "Invalid value input: " ++ newVal
                Just x -> runEvalIO' r db $ k x
            Just x -> runEvalIO' r db $ k x
    runEvalIO' r db (Free (KvPutOp key val m)) = do
      readState <- readDB db
      case readState of
        Left e -> pure $ Failed e
        Right oldState -> do
            let newState = (key, val) : (filter ((key /=).fst) oldState)
              in writeDB db newState
            runEvalIO' r db m
    runEvalIO' r db (Free (TransactionOp m k)) = do
      res <- withTempDB $ \tempDB -> do
        copyDB db tempDB
        res' <- runEvalIO' r tempDB m -- Every write in this recursive call is made to tempDB
        case res' of
          Done _ -> copyDB tempDB db -- Success: commit by copying the temp DB back to db
          Broke _ -> copyDB tempDB db -- Broke is a success case
          Failed _ -> pure () -- Failure: skip commit i.e. db is unchanged
        pure res'
      case res of
        Failed e -> pure $ Failed e
        Broke v -> pure $ Broke v
        Done x -> runEvalIO' r db $ k x
    runEvalIO' _ _ (Free (BreakOp v)) = pure $ Broke v
    runEvalIO' r db (Free (LoopOp m k)) = do 
      res <- runEvalIO' r db m
      case res of
        Done x -> runEvalIO' r db $ k x
        Broke v -> runEvalIO' r db $ k v
        Failed e -> pure $ Failed e
