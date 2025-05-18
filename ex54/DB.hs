{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module DB (lookupUrl, insertUrl, incrementVisitCount, PrimaryKey (UrlId), Url, UrlT(..)) where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

-- Types
data UrlT f
  = Url
  { _urlShortUrl   :: Columnar f Text
  , _urlLongUrl    :: Columnar f Text
  , _urlVisitCount :: Columnar f Int32 }
  deriving Generic

type Url = UrlT Identity
type UrlId = PrimaryKey UrlT Identity

data UrlShortnerDb f where
  UrlShortnerDb :: {_url :: f (TableEntity UrlT)} -> UrlShortnerDb f
  deriving (Generic, Database be)

urlShortnerDb :: DatabaseSettings be UrlShortnerDb
urlShortnerDb = defaultDbSettings

-- Instances
instance Beamable UrlT

instance Table UrlT where
   data PrimaryKey UrlT f = UrlId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = UrlId . _urlShortUrl

deriving instance Show Url
deriving instance Eq Url

-- Functions
incrementVisitCount :: Text -> Connection -> IO (Maybe Text)
incrementVisitCount shortUrl conn
  = runBeamSqliteDebug putStrLn conn
  $ runLookup >>= traverse runIncrement
  where
    runLookup = runSelectReturningOne
              $ lookup_ (_url urlShortnerDb) (UrlId shortUrl)
    runIncrement (Url _ longUrl count) = do 
      runUpdate $ update (_url urlShortnerDb)
                         (\url -> _urlVisitCount url <-. val_ count + 1)
                         (\url -> _urlShortUrl url ==. val_ shortUrl)
      return longUrl

insertUrl :: Text -> Text -> Connection -> IO ()
insertUrl short long conn
  = runBeamSqliteDebug putStrLn conn
  $ runInsert
  $ insert (_url urlShortnerDb)
  $ insertValues [ Url short long 0 ]

lookupUrl :: UrlId -> Connection -> IO (Maybe Url)
lookupUrl urlId conn
  = runBeamSqliteDebug putStrLn conn
  $ runSelectReturningOne
  $ lookup_ (_url urlShortnerDb) urlId
