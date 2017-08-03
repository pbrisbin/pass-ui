module App
    ( app
    ) where

import Brick
import Brick.Widgets.List
import Control.Monad (void)
import Graphics.Vty
    ( Event(..)
    , Key(..)
    , defAttr
    )
import qualified Data.Vector as V

type AppState = List String String

handle :: AppState -> BrickEvent n e -> EventM String (Next AppState)
handle s (VtyEvent (EvKey (KChar 'q') [])) = halt s
handle s (VtyEvent e) = continue =<< handleListEvent e s
handle s _ = halt s

draw :: AppState -> [Widget String]
draw s = [renderList drawItem True s]
  where
    drawItem True x = str ">" <+> str x <+> str "<"
    drawItem False x = str " " <+> str x <+> str " "

app :: IO ()
app = void
    $ defaultMain
        App
            { appDraw = draw
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handle
            , appStartEvent = return
            , appAttrMap = \_ -> attrMap defAttr []
            }
    $ list "list" (V.fromList ["one", "two", "three"]) 1
