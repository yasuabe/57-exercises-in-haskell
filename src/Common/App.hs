module Common.App (AppType, AppError(..), run, runProgram) where

import Control.Exception (Exception)
import Control.Monad.Catch (catch)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Typeable (Typeable)
import System.Console.Haskeline
    ( InputT, defaultSettings, outputStrLn, runInputT )

type AppType a = ExceptT String IO a

newtype AppError = AppError String deriving (Show, Typeable)
instance Exception AppError

run :: AppType () -> IO ()
run app = runExceptT app >>= either (putStrLn . ("Error: " ++)) (const (return ()))

runProgram :: InputT IO () -> IO ()
runProgram app =
  runInputT defaultSettings app'
  where
    app' = app `catch` handler
    handler :: AppError -> InputT IO ()
    handler (AppError msg) = outputStrLn $ "Caught exception: " ++ msg
