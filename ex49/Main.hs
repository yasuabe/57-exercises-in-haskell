{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text (pack)
import qualified GI.GdkPixbuf as GdkPixbuf
import GI.Gtk.Objects.Image (imageNewFromPixbuf)
import Control.Monad (replicateM_)
import Data.Int (Int32)

url :: FilePath
url = "ex49/Oyster_dubai.jpg"

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
  mainArea <- new Gtk.FlowBox [ #margin := 5 ]
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
  let entry     = uiEntry ui
      button    = uiButton ui
      statusbar = uiStatusbar ui
      mainArea  = uiMainArea ui

  contextId <- #getContextId statusbar "search"

  _ <- on button #clicked $ do
    text <- #getText entry
    _ <- #push statusbar contextId (pack ("tags: " ++ show text))

    Just pixbufOrig <- GdkPixbuf.pixbufNewFromFile url
    origWidth  <- GdkPixbuf.getPixbufWidth pixbufOrig
    origHeight <- GdkPixbuf.getPixbufHeight pixbufOrig

    let scaleFactor = min (200 `divRatio` origWidth) (200 `divRatio` origHeight)
        scaledW = floor (fromIntegral origWidth * scaleFactor)
        scaledH = floor (fromIntegral origHeight * scaleFactor)

    pixbuf <- GdkPixbuf.pixbufScaleSimple pixbufOrig scaledW scaledH GdkPixbuf.InterpTypeBilinear

    replicateM_ 20 $ do
      img <- imageNewFromPixbuf pixbuf
      #add mainArea img

    #showAll mainArea

    return ()
  return ()
  where
    divRatio :: Int -> Int32 -> Double
    divRatio a b = fromIntegral a / fromIntegral b

main = do
  Gtk.init Nothing
  (window, ui) <- buildUI
  setupHandlers ui
  _ <- on window #destroy Gtk.mainQuit
  #showAll window
  Gtk.main
