-- Ex55: Text Sharing
--
-- ・ Create a web app for sharing text snippets (like Pastie).
-- ・ Users can enter and save text through a form.
-- ・ The app stores the text in a persistent data store.
-- ・ Each saved text is assigned a URL-safe slug (e.g., via a hash like SHA or MD5), not a primary key.
-- ・ Users can:
--   ・ View the text by visiting its unique URL.
--   ・ Click "Edit" to copy it into the text submission form again.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Function ((&))
import Data.Text (Text)

import qualified Database.MongoDB as MDB (Document, lookup, defaultPort, select)
import Database.MongoDB.Query (findOne, save)
import Database.Persist.MongoDB (ConnectionPool, MongoContext, createMongoDBPool, runMongoDBPoolDef, (=:))
import Text.Hamlet (hamletFile)
import Yesod

import Ex55

data App = App { connPool :: ConnectionPool }

instance YesodPersist App where
    type YesodPersistBackend App = MongoContext
    runDB action = do
        App pool <- getYesod
        runMongoDBPoolDef action pool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "App" [parseRoutes|
/ex55                HomeR  GET
/ex55/view/#Text     ViewR  GET
/ex55/edit           EditR  POST
/ex55/share          ShareR POST
|]

instance Yesod App

snippetNotFound :: Text -> Handler Html
snippetNotFound slug = defaultLayout [whamlet|<h1>Snippet not found for slug: #{slug}|]

querySnippet :: Text -> Handler (Maybe MDB.Document)
querySnippet slug = runDB $ findOne (MDB.select ["slug" =: slug] "snippets")

getHomeR :: Handler Html
getHomeR = do
  let content = "" :: Text
  defaultLayout $ toWidget $(hamletFile "ex55/templates/input.hamlet")

getViewR :: Text -> Handler Html
getViewR slug = querySnippet slug >>= \case
  Just doc -> defaultLayout $ do
    let content = (maybe "" id (MDB.lookup "content" doc)) :: Text
    toWidget $(hamletFile "ex55/templates/display.hamlet")
  Nothing -> snippetNotFound slug

postEditR :: Handler Html
postEditR = do
  slug <- runInputPost $ ireq textField "slug"
  querySnippet slug >>= \case
    Just doc -> do
      let content = (maybe "" id (MDB.lookup "content" doc)) :: Text
      defaultLayout $ toWidget $(hamletFile "ex55/templates/input.hamlet")
    Nothing -> snippetNotFound slug

postShareR :: Handler Html
postShareR = do
  snippet <- runInputPost $ ireq textField "snippet"
  slug    <- generateSlug snippet & liftIO
  _       <- saveSnippet slug snippet
  defaultLayout $ redirect (ViewR slug)
  where
    saveSnippet slug snippet =
       runDB $ save "snippets" [ "slug"    =: slug
                               , "content" =: snippet
                               ]

main :: IO ()
main = createMongoDBPool
      "text_sharing"  -- Database
      "127.0.0.1"     -- HostName (type synonym for String)
      MDB.defaultPort -- PortID
      Nothing         -- Maybe MongoAuth
      1               -- pool size (number of stripes)
      1               -- stripe size (number of connections per stripe)
      20              -- time a connection is left idle before closing
    >>= (warp 3000 . App)
