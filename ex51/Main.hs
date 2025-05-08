{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

-- Ex51: Pushing Notes to Firebase
-- ・Build a command-line app that can:
--   ・Save a note: mynotes new <text>
--   ・Show all notes: mynotes show
-- ・Notes are saved to Firebase using its REST API (not client libraries).
-- ・Notes should be stored with a timestamp and displayed in reverse chronological order.
-- ## Constraints:
-- ・Use a config file to store the Firebase API key (not hardcoded).
-- ・Communicate via raw HTTP requests to Firebase's REST endpoint.

import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecode, encode, Value (Object), Value (String))
import Data.Aeson.Decoding (decode)
import Data.Aeson.Types (Value)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate (i)
import Data.Text (pack, unpack, Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Debug.Trace (trace)
import Network.HTTP.Client (method, requestBody, requestHeaders, RequestBody(..))
import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest, setRequestBodyJSON)
import System.Console.Haskeline (InputT, runInputT, defaultSettings)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

import Common.App (AppError (..))
import Common.System (putTextLn)
import qualified AddNoteReq as ANR
import qualified Config as C
import qualified GetNotesRes as GNR
import qualified IdTokenRequest as I
import qualified Note as N
import qualified TokenInfo as TI
import qualified TokenInfoRes as TIR

configFilePath :: FilePath
configFilePath = "ex51/config/config.json"
idTokenCachePath :: FilePath
idTokenCachePath = "ex51/cache/id_token.json"

type App = ReaderT C.Config (InputT IO)

loadConfig :: IO C.Config
loadConfig = do
  content <- BL.readFile configFilePath
  case eitherDecode content of
    Left err -> error $ "Failed to parse config: " ++ err
    Right config -> return config

saveNote :: Text -> App ()
saveNote text = do
  token <- tryGetCachedIdToken
  config <- ask
  let url = C.firebaseUrl config token
  requestBody' <- liftIO $ ANR.noteNowIO text
  initialRequest <- liftIO $ parseRequest (unpack url)
  let request = setRequestBodyJSON requestBody' $
                initialRequest { method = "POST" }
  response <- liftIO $ httpBS request
  let body = BL.fromStrict $ getResponseBody response
  case extractId body of
    Nothing -> error $ "Failed to extract ID from response: " ++ show body
    Just name -> do
      lift $ putTextLn [i|Your note was saved. (id: #{name})|]
  where
    extractId :: BL.ByteString -> Maybe Text
    extractId body = do
      res  <- decode body :: Maybe Value
      case res of
        Object obj -> case KM.lookup "name" obj of
          Just (Data.Aeson.String name) -> Just name
          _                             -> Nothing
        _          -> Nothing

tryGetCachedIdToken :: App Text
tryGetCachedIdToken = do
  fileExists <- liftIO $ doesFileExist idTokenCachePath
  if fileExists
    then do
      tokenInfo <- readIdTokenFromCache
      isExpired <- TI.isExpiredIO tokenInfo & liftIO
      if isExpired
        then do
          tokenInfo <- refreshIdToken (TI.refreshToken tokenInfo)
          saveIdTokenToCache tokenInfo
          return (TI.idToken tokenInfo)
        else
          return (TI.idToken tokenInfo)
    else do
      newIdToken <- retrieveIdToken =<< ask
      saveIdTokenToCache newIdToken
      return (TI.idToken newIdToken)
  where
    saveIdTokenToCache :: TI.TokenInfo -> App ()
    saveIdTokenToCache tokenInfoRes = do
      let content = encode tokenInfoRes
      liftIO $ BL.writeFile idTokenCachePath content

    readIdTokenFromCache :: App TI.TokenInfo
    readIdTokenFromCache = do
      content <- liftIO $ BL.readFile idTokenCachePath
      case eitherDecode content of
        Left err        -> error $ "Failed to parse ID token from cache: " ++ err
        Right tokenInfo -> return tokenInfo

refreshIdToken :: Text -> App TI.TokenInfo
refreshIdToken refreshToken = do
  config <- ask
  let url = [i|https://securetoken.googleapis.com/v1/token?key=#{C.apiKey config}|]
      requestBody' = TLE.encodeUtf8 . TL.fromStrict $ "grant_type=refresh_token&refresh_token=" <> refreshToken

  putStrLn (unpack url) & liftIO
  initialRequest <- liftIO $ parseRequest (unpack url)
  let request = initialRequest
                  { method = "POST"
                  , requestBody = RequestBodyLBS requestBody'
                  , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                  }
  response <- liftIO $ httpBS request
  let body = BL.fromStrict $ getResponseBody response
  case TIR.decodeSnakeCaseTokenInfoRes body of
    Just tokenInfo -> liftIO $ TI.makeTokenInfoIO tokenInfo
    Nothing        -> throwM $ trace (show body) $ AppError "Failed to decode token info"

retrieveIdToken :: C.Config -> App TI.TokenInfo
retrieveIdToken config = do
  let url = [i|https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=#{C.apiKey config}|]
      requestBody' = I.IdTokenRequest (C.email config) (C.password config) True
  initialRequest <- liftIO $ parseRequest (unpack url)
  let request = setRequestBodyJSON requestBody' $
                initialRequest { method = "POST" }
  response <- liftIO $ httpBS request
  let body = BL.fromStrict $ getResponseBody response
  case eitherDecode body of
    Left err -> error $ "Failed to retrieve ID token: " ++ err ++ "\nResponse body: " ++ show body
    Right token -> liftIO $ TI.makeTokenInfoIO token

showNotes :: App ()
showNotes = do
  token  <- tryGetCachedIdToken
  config <- ask
  let url = C.firebaseUrl config token
  request  <- liftIO $ parseRequest (unpack url)
  response <- liftIO $ httpBS request
  let body = BL.fromStrict $ getResponseBody response
  case eitherDecode body of
    Left err -> liftIO $ putStrLn $ "Error: " ++ err ++ "\nResponse body: " ++ show body
    Right notes -> displayNotes notes
  where
    displayNotes :: GNR.GetNotesRes -> App ()
    displayNotes notes = do
      let sortedNotes = GNR.toNotes notes
      mapM_ printNote sortedNotes

    printNote :: N.Note -> App ()
    printNote noteRes = lift $ putTextLn [i|#{N.date noteRes} - #{N.note noteRes}|]

program :: App ()
program = liftIO getArgs >>= \case
  ("new" : texts) -> saveNote (unwords texts & pack)
  ["show"]        -> showNotes
  _               -> putStrLn "Usage: mynotes new <text> | mynotes show"
                   & liftIO 
main :: IO ()
main = do
  config <- loadConfig
  runInputT defaultSettings $ runReaderT program config
