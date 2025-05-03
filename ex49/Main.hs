{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text (pack)
import Data.Text (Text)
import qualified GI.GdkPixbuf as GdkPixbuf
import GI.Gtk.Objects.Image (imageNewFromPixbuf)
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib

import Control.Monad (forM_)
import Control.Concurrent (forkIO)
import Control.Exception (handle, SomeException)

import Data.Int (Int32)
import Data.Word (Word32)
import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (ByteString)

import Data.Aeson
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)

-- Flickr JSON types
data Feed = Feed { items :: [Item] } deriving (Show, Generic)
data Item = Item { media :: Media } deriving (Show, Generic)
data Media = Media { m :: String } deriving (Show, Generic)

instance FromJSON Feed
instance FromJSON Item
instance FromJSON Media

stripJsonFlickrFeedWrapper :: ByteString -> ByteString
stripJsonFlickrFeedWrapper raw =
  BSC.dropEnd 1 $ BSC.drop prefixLen raw
  where
    prefix = "jsonFlickrFeed("
    prefixLen = BSC.length prefix

data UI = UI
  { uiEntry     :: Gtk.Entry
  , uiButton    :: Gtk.Button
  , uiStatusbar :: Gtk.Statusbar
  , uiMainArea  :: Gtk.FlowBox
  }

buildUI :: IO (Gtk.Window, UI)
buildUI = do
  window <- new Gtk.Window
    [ #title := pack "Image Search App"
    , #defaultWidth := 800
    , #defaultHeight := 600
    ]

  vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]

  -- Toolbar
  toolbar <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 5, #margin := 5 ]
  label <- new Gtk.Label [ #label := "tags: " ]
  entry <- new Gtk.Entry [ #widthChars := 50 ]
  inputBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 0 ]
  #packStart inputBox label False False 0
  #packStart inputBox entry False False 0
  button <- new Gtk.Button [ #label := "Search" ]
  spacer <- new Gtk.Box [ #hexpand := True ]
  #packStart toolbar spacer True True 0
  #packStart toolbar inputBox False False 0
  #packStart toolbar button False False 0

  -- Main area
  mainArea <- new Gtk.FlowBox []

  scrolled <- new Gtk.ScrolledWindow []
  #setPolicy scrolled Gtk.PolicyTypeAutomatic Gtk.PolicyTypeAutomatic
  #add scrolled mainArea

  -- Status bar
  statusbar <- new Gtk.Statusbar []
  _ <- #push statusbar 0 (pack "tags: (none)")

  -- Compose UI
  #packStart vbox toolbar False False 0
  #packStart vbox scrolled True True 0
  #packStart vbox statusbar False False 0
  #add window vbox

  return (window, UI entry button statusbar mainArea)

setupHandlers :: UI -> IO ()
setupHandlers ui = do
  let button = uiButton ui
  _ <- on button #clicked (onSearchClicked ui)
  return ()




fetchFeedAndSpawnWorkers
  :: Text
  -> Gtk.FlowBox
  -> Gtk.Statusbar
  -> Word32
  -> IO ()
fetchFeedAndSpawnWorkers tag mainArea statusbar contextId = do
  handle (\e -> putStrLn $ "Error in worker thread: " ++ show (e :: SomeException)) $ do
    let tagStr = BSC.unpack (BSC.pack (show tag))
        feedUrl = "https://www.flickr.com/services/feeds/photos_public.gne?format=json&tags=" ++ tagStr

    feedReq <- parseRequest feedUrl
    feedRes <- httpBS feedReq
    let feedRaw = getResponseBody feedRes
        feedStripped = stripJsonFlickrFeedWrapper feedRaw

    case eitherDecode (BL.fromStrict feedStripped) :: Either String Feed of
      Left err -> putStrLn $ "JSON parse error: " ++ err
      Right parsedFeed -> do
        let urls = take 20 $ map (m . media) (items parsedFeed)
        forM_ urls $ \imgUrl -> forkIO $ do
          fetchAndDisplayImage mainArea statusbar contextId imgUrl

        _ <- GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
               #showAll mainArea
               _ <- #push statusbar contextId (pack ("tags: " ++ show tag))
               return False
        return ()






onSearchClicked :: UI -> IO ()
onSearchClicked ui = do
  let entry     = uiEntry ui
      statusbar = uiStatusbar ui
      mainArea  = uiMainArea ui

  contextId <- #getContextId statusbar "search"
  tag <- #getText entry
  _ <- #push statusbar contextId (pack "Fetching...")

  children <- Gtk.containerGetChildren mainArea
  forM_ children $ \child -> Gtk.containerRemove mainArea child

  _ <- forkIO $ fetchFeedAndSpawnWorkers tag mainArea statusbar contextId
  return ()












divRatio :: Int -> Int32 -> Double
divRatio a b = fromIntegral a / fromIntegral b

main :: IO ()
main = do
  Gtk.init Nothing
  (window, ui) <- buildUI
  setupHandlers ui

  contextId <- #getContextId (uiStatusbar ui) "initial"
  _ <- #push (uiStatusbar ui) contextId (pack "Loading initial images...")
  _ <- forkIO $ fetchFeedAndSpawnWorkers "" (uiMainArea ui) (uiStatusbar ui) contextId

  _ <- on window #destroy Gtk.mainQuit
  #showAll window
  Gtk.main













fetchAndDisplayImage
  :: Gtk.FlowBox
  -> Gtk.Statusbar
  -> Word32
  -> String
  -> IO ()
fetchAndDisplayImage mainArea statusbar contextId imgUrl = do
  putStrLn $ "Fetching image: " ++ imgUrl
  imgReq <- parseRequest imgUrl
  imgRes <- httpBS imgReq
  let imgBytes = getResponseBody imgRes
  putStrLn $ "Image fetched: " ++ imgUrl
  stream <- Gio.memoryInputStreamNewFromData imgBytes Nothing
  mbPixbuf <- GdkPixbuf.pixbufNewFromStream stream (Nothing :: Maybe Gio.Cancellable)
  case mbPixbuf of
    Nothing -> putStrLn $ "Failed to decode: " ++ imgUrl
    Just pixbufOrig -> do
      origWidth  <- GdkPixbuf.getPixbufWidth pixbufOrig
      origHeight <- GdkPixbuf.getPixbufHeight pixbufOrig

      let scaleFactor = min (200 `divRatio` origWidth) (200 `divRatio` origHeight)
          scaledW = floor (fromIntegral origWidth * scaleFactor)
          scaledH = floor (fromIntegral origHeight * scaleFactor)

      pixbuf <- GdkPixbuf.pixbufScaleSimple pixbufOrig scaledW scaledH GdkPixbuf.InterpTypeBilinear
      img <- imageNewFromPixbuf pixbuf

      _ <- GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
             #add mainArea img
             #show img
             return False
      return ()
