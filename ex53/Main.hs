-- Ex53: Todo List
-- ・Command-line todo list app.
-- ・Store data in Redis.
-- 
-- # Commands:
-- ・add: <task>  – Add a new task (empty tasks are not allowed)
-- ・list         – Show all tasks
-- ・remove: <id> – Remove completed task by ID
-- ・exit         – Quit the app

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans (lift)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Pool (Pool, newPool, withResource, defaultPoolConfig)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE
import qualified Database.Redis as R
import Database.Redis.Sentinel (Redis)
import System.Console.Haskeline (InputT)
import Common.System (putTextLn, repeatUntilValid)
import Common.App (runProgram)
import Command (Command(..), parseText)

type TodoApp = ReaderT (Pool R.Connection) (InputT IO)

newtype RedisException = RedisException R.Reply
  deriving (Show)

instance Exception RedisException

runCommand :: Redis (Either R.Reply a) -> TodoApp a
runCommand action = do
  pool <- ask
  liftIO $ withResource pool $ \conn ->
    R.runRedis conn action >>= \case
      Left err -> throwM (RedisException err)
      Right a  -> return a

toByteString :: Text -> BS.ByteString
toByteString = TE.encodeUtf8

onAddTask :: Text -> TodoApp ()
onAddTask task = do
  taskId <- runCommand (R.incr "ex53:taskid") <&> (BS.pack . show)
  void $ runCommand (R.hset "ex53:tasks" taskId $ toByteString task)

onListTasks :: TodoApp ()
onListTasks = do
  tasks <- runCommand (R.hgetall "ex53:tasks")
  mapM_ (\(a, b) -> putTextLn [i|#{a}) #{b}|] & lift) tasks

onRemoveTasks :: Text -> TodoApp ()
onRemoveTasks taskId =
  runCommand (R.hdel "ex53:tasks" [toByteString taskId]) & void

commandLoop :: TodoApp ()
commandLoop = readCommand >>= \case
  AddTask   task    -> onAddTask task       >> commandLoop
  ListTasks         -> onListTasks          >> commandLoop
  RemoveTask taskId -> onRemoveTasks taskId >> commandLoop
  Exit              -> putTextLn "Bye." & lift
  where
    readCommand :: TodoApp Command
    readCommand = lift
                $ repeatUntilValid parseText "> " "Invalid command."

main :: IO ()
main = do
  pool <- newPool $ defaultPoolConfig (R.connect R.defaultConnectInfo) R.disconnect 5.0 10
  runProgram $ runReaderT commandLoop pool
