module Common.App (AppType, run) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)

type AppType a = ExceptT String IO a

run :: AppType () -> IO ()
run app = runExceptT app >>= either (putStrLn . ("Error: " ++)) (const (return ()))