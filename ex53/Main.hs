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
import Control.Monad.Catch (throwM, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans (lift)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Text (Text, pack)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE
import qualified Database.Redis as R
import Database.Redis.Sentinel (Redis)
import System.Console.Haskeline (InputT, runInputT, defaultSettings)
import Common.System (putTextLn, repeatUntilValid)
import Common.App (runProgram)
import Command (Command(..), parseText)

type AppType = ReaderT R.Connection (InputT IO)

newtype RedisException = RedisException R.Reply
  deriving (Show)

instance Exception RedisException

runCommand :: Redis (Either R.Reply a) -> AppType a
runCommand action = do
  conn <- ask
  liftIO $ R.runRedis conn action >>= \case
    Left err -> throwM (RedisException err)
    Right a  -> return a

fromText :: Text -> BS.ByteString
fromText = TE.encodeUtf8

onAddTask :: Text -> AppType ()
onAddTask task = do
  id <- runCommand (R.incr "ex53:taskid") <&> (BS.pack . show)
  void $ runCommand (R.hset "ex53:tasks" id $ fromText task)

onListTasks :: AppType ()
onListTasks = do
  tasks <- runCommand (R.hgetall "ex53:tasks")
  mapM_ (\(a, b) -> putTextLn [i|#{a}, #{b}|] & lift) tasks

onRemoveTasks :: Text -> AppType ()
onRemoveTasks id =
  runCommand (R.hdel "ex53:tasks" [fromText id]) & void

commandLoop :: AppType ()
commandLoop = readCommand >>= \case
  AddTask   task -> onAddTask task   >> commandLoop
  ListTasks      -> onListTasks      >> commandLoop
  RemoveTask id  -> onRemoveTasks id >> commandLoop
  Exit           -> putTextLn "Bye." & lift
  where
    readCommand :: AppType Command
    readCommand = lift
                $ repeatUntilValid parseText "> " "Invalid command."

main :: IO ()
main = do
  conn <- R.connect R.defaultConnectInfo
  runProgram $ runReaderT commandLoop conn
