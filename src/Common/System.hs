module Common.System (putText, putTextLn, getTextLn, promptInput, readDouble, repeatUntilValid) where

import System.IO (hFlush, stdout)
import Data.Text as T (Text, pack, strip, unpack)
import Data.Function ((&))

type Converter a = Text -> Maybe a

putText :: Text -> IO ()
putText text = putStr (unpack text) >> hFlush stdout

putTextLn :: Text -> IO ()
putTextLn text = putStrLn (unpack text) >> hFlush stdout

promptInput :: Text -> IO Text
promptInput promptText = do
  putText promptText
  T.strip . pack <$> getLine

getTextLn :: IO Text
getTextLn = pack <$> getLine

repeatUntilValid :: Converter a -> Text -> Text -> IO a
repeatUntilValid converter prompt errorMessage = do
  input <- promptInput prompt
  case converter input of
    Just value -> return value
    Nothing    -> do
      putTextLn errorMessage
      repeatUntilValid converter prompt errorMessage

readDouble :: Text -> Maybe Double
readDouble input =
  case input & T.strip & unpack & reads of
    [(x, "")] -> Just x
    _         -> Nothing
