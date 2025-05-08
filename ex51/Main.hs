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
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Data.Aeson
    ( eitherDecode, encode, Value(Object), ToJSON )
import Data.Aeson.Decoding (decode)
import qualified Data.Aeson as DA
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Text (pack, unpack, Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Client
    ( method, requestBody, requestHeaders, RequestBody(..), Request )
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
import Data.Functor ((<&>))
import IdTokenRequest (IdTokenRequest)

configFilePath :: FilePath
configFilePath = "ex51/config/config.json"
tokenInfoCachePath :: FilePath
tokenInfoCachePath = "ex51/cache/id_token.json"

type App = ReaderT C.Config (InputT IO)

loadConfig :: IO C.Config
loadConfig = do
  content <- BL.readFile configFilePath
  case eitherDecode content of
    Right config -> return config
    Left err     -> throwM $ AppError $ "Failed to parse config: " ++ err

sendRequest :: Text -> (Request -> Request) -> App BL.ByteString
sendRequest url modifier = do
  request  <- parseRequest (unpack url) <&> modifier & liftIO
  response <- httpBS request & liftIO
  return $ BL.fromStrict $ getResponseBody response

postJson :: (ToJSON a) => Text -> a -> App BL.ByteString
postJson url body = sendRequest url
                  $ \req -> req { method = "POST" } & setRequestBodyJSON body

saveNote :: Text -> App ()
saveNote text = do
  token   <- tryGetCachedIdToken
  url     <- asks (`C.firebaseUrl` token)
  reqBody <- ANR.noteNowIO text & liftIO
  resBody <- postJson url reqBody
  case extractId resBody of
    Just name -> putTextLn [i|Your note was saved. (id: #{name})|] & lift
    Nothing   -> throwM $ AppError $ "Failed to extract ID from response: " ++ show resBody
  where
    extractId :: BL.ByteString -> Maybe Text
    extractId body = do
      Object obj     <- decode body
      DA.String name <- KM.lookup "name" obj
      return name

tryGetCachedIdToken :: App Text
tryGetCachedIdToken =
  ifM (doesFileExist tokenInfoCachePath & liftIO)
    (do tokenInfo <- readIdTokenFromCache
        ifM (TI.isExpiredIO tokenInfo & liftIO)
            (do refreshedTokenInfo <- refreshIdToken (TI.refreshToken tokenInfo)
                cacheTokenInfo refreshedTokenInfo
                return refreshedTokenInfo)
            (return tokenInfo))
    (do newTokenInfo <- retrieveIdToken
        cacheTokenInfo newTokenInfo
        return newTokenInfo) <&> TI.idToken
  where
    cacheTokenInfo tokenInfoRes =
      encode tokenInfoRes & BL.writeFile tokenInfoCachePath & liftIO

    readIdTokenFromCache = do
      content <- BL.readFile tokenInfoCachePath & liftIO
      case eitherDecode content of
        Right tokenInfo -> return tokenInfo
        Left err        -> throwM $ AppError $ "Failed to parse ID token from cache: " ++ err

refreshIdToken :: Text -> App TI.TokenInfo
refreshIdToken refreshToken = do
  url  <- buildUrl
  resp <- post url reqBody
  case TIR.decodeSnakeCaseRes resp of
    Just tokenInfo -> TI.makeTokenInfoIO tokenInfo & liftIO
    Nothing        -> throwM $ AppError "Failed to decode token info"
  where
    buildUrl :: App Text
    buildUrl = do apiKey <- asks C.apiKey
                  return [i|https://securetoken.googleapis.com/v1/token?key=#{apiKey}|]

    reqBody = TLE.encodeUtf8 . TL.fromStrict
            $ "grant_type=refresh_token&refresh_token=" <> refreshToken

    post :: Text -> BL.ByteString -> App BL.ByteString
    post url body = sendRequest url
                  $ \req -> req
                    { method         = "POST"
                    , requestBody    = RequestBodyLBS body
                    , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                    }

retrieveIdToken :: App TI.TokenInfo
retrieveIdToken = do
  url     <- asks buildUrl
  reqBody <- asks buildReqBody
  resBody <- postJson url reqBody
  case eitherDecode resBody of
    Right token -> TI.makeTokenInfoIO token & liftIO
    Left err    -> throwM $ AppError $ "Failed to retrieve ID token: " ++ err
  where
    buildUrl :: C.Config -> Text
    buildUrl config = [i|https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=#{C.apiKey config}|]
    buildReqBody :: C.Config -> IdTokenRequest
    buildReqBody config = I.IdTokenRequest (C.email config) (C.password config) True

showNotes :: App ()
showNotes = do
  token <- tryGetCachedIdToken
  url   <- asks (`C.firebaseUrl` token)
  body  <- sendRequest url id
  case eitherDecode body of
    Right notes -> displayNotes notes
    Left err    -> throwM $ AppError $ "Error: " ++ err
  where
    displayNotes notes = GNR.toNotes notes & mapM_ printNote
    printNote noteRes = lift $ putTextLn [i|#{N.date noteRes} - #{N.note noteRes}|]

program :: App ()
program = liftIO getArgs >>= \case
  ("new" : texts) -> saveNote (unwords texts & pack)
  ["show"]        -> showNotes
  _               -> putStrLn "Usage: mynotes new <text> | mynotes show" & liftIO

main :: IO ()
main = do
  config <- loadConfig
  runInputT defaultSettings $ runReaderT program config
