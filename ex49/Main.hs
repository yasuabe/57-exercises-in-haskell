{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- # Ex49: Flickr Photo Search
--
-- - Take in a search string via GUI.
-- - Fetch Flickr’s public photo feed matching the search.
-- - Display resulting photos visually.

import Data.Aeson (FromJSON, eitherDecode)
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.GI.Base (on, AttrOp((:=)), new)
import Data.Text (pack, unpack, Text)
import GHC.Generics (Generic)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified GI.GdkPixbuf as GdkPixbuf
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import GI.GLib.Callbacks (SourceFunc)
import qualified GI.Gtk as Gtk
import GI.Gtk.Objects.Image (imageNewFromPixbuf)
import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest)

import Common.Util (divRatio)

imageSize :: Int
imageSize = 200

newtype Feed = Feed { items :: [Item] } deriving (Show, Generic)
newtype Item = Item { media :: Media } deriving (Show, Generic)
newtype Media = Media { m :: String } deriving (Show, Generic)

instance FromJSON Feed
instance FromJSON Item
instance FromJSON Media

stripFlickrWrapper :: ByteString -> ByteString
stripFlickrWrapper raw =
  BSC.dropEnd 1 $ BSC.drop prefixLen raw
  where
    prefix = "jsonFlickrFeed("
    prefixLen = BSC.length prefix

idleAdd :: SourceFunc -> IO ()
idleAdd func = void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE func

byteStringToImage :: ByteString -> IO (Maybe Gtk.Image)
byteStringToImage imgBytes = do
  stream      <- Gio.memoryInputStreamNewFromData imgBytes Nothing
  maybePixbuf <- GdkPixbuf.pixbufNewFromStream stream (Nothing :: Maybe Gio.Cancellable)
  traverse pixbufToImage maybePixbuf 
  where
    scaleImage pixbuf = do
      origW <- GdkPixbuf.getPixbufWidth  pixbuf
      origH <- GdkPixbuf.getPixbufHeight pixbuf

      let factor  = min (imageSize `divRatio` origW) (imageSize `divRatio` origH) :: Double
          scale x = floor (fromIntegral x * factor)
          scaledW = scale origW
          scaledH = scale origH

      GdkPixbuf.pixbufScaleSimple pixbuf scaledW scaledH GdkPixbuf.InterpTypeBilinear

    pixbufToImage pixbuf = scaleImage pixbuf >>= imageNewFromPixbuf

-- UI構築
data UI = UI
  { uiEntry     :: Gtk.Entry
  , uiButton    :: Gtk.Button
  , uiStatusbar :: Gtk.Statusbar
  , uiMainArea  :: Gtk.FlowBox
  }

buildUI :: IO (Gtk.Window, UI)
buildUI = do
  window <- new Gtk.Window
    [ #title         := pack "Image Search App"
    , #defaultWidth  := 800
    , #defaultHeight := 600
    ]

  vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]

  -- Toolbar
  toolbar  <- new Gtk.Box    [ #orientation := Gtk.OrientationHorizontal, #spacing := 2, #margin := 2 ]
  label    <- new Gtk.Label  [ #label := "tags: " ]
  entry    <- new Gtk.Entry  [ #widthChars := 50 ]
  inputBox <- new Gtk.Box    [ #orientation := Gtk.OrientationHorizontal, #spacing := 0 ]
  button   <- new Gtk.Button [ #label := "Search" ]
  spacer   <- new Gtk.Box    [ #hexpand := True ]
  #packStart inputBox label    False False 0
  #packStart inputBox entry    False False 0
  #packStart toolbar  spacer   True  True  0
  #packStart toolbar  inputBox False False 0
  #packStart toolbar  button   False False 0

  -- Main area
  mainArea <- new Gtk.FlowBox []

  scrolled <- new Gtk.ScrolledWindow []
  #setPolicy scrolled Gtk.PolicyTypeAutomatic Gtk.PolicyTypeAutomatic
  #add scrolled mainArea

  -- Status bar
  statusbar <- new Gtk.Statusbar []
  _         <- #push statusbar 0 (pack "tags: (none)")

  -- Compose UI
  #packStart vbox toolbar   False False 0
  #packStart vbox scrolled  True  True  0
  #packStart vbox statusbar False False 0
  #add window vbox

  return (window, UI entry button statusbar mainArea)

setupHandlers :: UI -> IO ()
setupHandlers ui = do
  let button = uiButton ui
  void $ on button #clicked (onSearchClicked ui)

onSearchClicked :: UI -> IO ()
onSearchClicked ui = do
  let entry     = uiEntry ui
      statusbar = uiStatusbar ui
      mainArea  = uiMainArea ui

  contextId <- #getContextId statusbar "search"
  tag       <- #getText entry
  _         <- #push statusbar contextId (pack "Fetching...")

  Gtk.containerGetChildren mainArea >>= mapM_ (Gtk.containerRemove mainArea)

  void $ forkIO $ fetchFeedAndSpawnWorkers ui tag

fetchFeedAndSpawnWorkers :: UI -> Text -> IO ()
fetchFeedAndSpawnWorkers ui tag = do
  feedRes <- fmap getResponseBody (parseRequest feedUrl >>= httpBS)
  case extractUrls feedRes of
    Left err   -> putStrLn err
    Right urls -> mapM_ forkImageFetcher urls >> idleAdd updateUI

  where
    feedUrl = "https://www.flickr.com/services/feeds/photos_public.gne?format=json&tags=" <> unpack tag

    extractUrls = bimap ("JSON parse error: " ++) (take 20 . map (m . media) . items)
                . eitherDecode
                . BL.fromStrict
                . stripFlickrWrapper

    mainArea  = uiMainArea ui
    statusbar = uiStatusbar ui
    forkImageFetcher = void
                     . forkIO
                     . fetchAndDisplayImage mainArea
    updateUI = do
      contextId <- #getContextId statusbar "initial"
      #showAll mainArea
      _ <- #push statusbar contextId (pack ("tags: " ++ show tag))
      return False

fetchAndDisplayImage :: Gtk.FlowBox -> String -> IO ()
fetchAndDisplayImage mainArea imageUrl = do
  imageRequest  <- parseRequest imageUrl
  imageResponse <- httpBS imageRequest
  (byteStringToImage . getResponseBody) imageResponse >>= \case
    Just img -> idleAdd (addImageToMainArea img)
    Nothing  -> putStrLn $ "Failed to decode: " ++ imageUrl
  where
    addImageToMainArea :: Gtk.Image -> IO Bool
    addImageToMainArea img = do
      #add mainArea img
      #show img
      return False

showInitialImages :: UI -> IO ()
showInitialImages ui = do
  contextId <- #getContextId (uiStatusbar ui) "initial"
  _         <- #push (uiStatusbar ui) contextId (pack "Loading initial images...")
  void $ forkIO $ fetchFeedAndSpawnWorkers ui ""

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  (window, ui) <- buildUI

  setupHandlers ui
  showInitialImages ui

  _ <- on window #destroy Gtk.mainQuit
  #showAll window
  Gtk.main
