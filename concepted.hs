{-# Language RankNTypes #-}
{-# Language TupleSections #-}
module Main where

import System.Environment (getArgs)

import Graphics.UI.Gtk hiding
  ( eventKeyName, eventButton, eventModifier
  , Menu, Point, Rectangle, Widget
  , add, get
  )
import Graphics.UI.Gtk.Gdk.Events (
  eventX, eventY, eventKeyName, eventButton, eventDirection, eventModifier)
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Monad
import Control.Monad.State

import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map as M

import Concepted.Graphics
import Concepted.Widget
import Concepted.Syntax.Parser
import Concepted.Plane
import Concepted.State
import Concepted.Misc (snapXY)

-- TODO: it would be nice to automatically reload a file when it
-- is externally modified.
-- TODO: the addFollow is recomputed at each myMotion (it could be
-- computed only when a selection occurs). Also chop is used at each
-- renderConcept.
-- TODO: use applicative for the parsers.
-- TODO: push the background and other style-related bits in the state.
-- TODO: when multiple items get selected (because there are stacked
-- at same place), the selection should cycle between them (one) and
-- all.
-- TODO: display the indice of each concept, link, handle.
-- TODO: after saving the state to file, check the file can be reload
-- succesfully.
-- TODO: there is a parser bug when reading a negative int.

----------------------------------------------------------------------
-- The main program
----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> main' $ replaceCurrentPlane cleanState $ (getCurrentPlane cleanState) {widgets =
      [ Label (10,20) "Alphaner"
      , Button (140,20) "Play" "play"
      , Button (240,20) "Configuration" "configure"
      ]}
    [fn] -> do
      c <- readFile fn
      case unserialize c of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right a -> main' $ a { filename = Just fn } `addPlane` emptyPlane { widgets =
          [ Label (10,20) "Alphaner"
          , Button (140,20) "Play" "play"
          , Button (240,20) "Quit" "quit"
          ]}
    _ -> putStrLn "usage: concepted filename"

main' :: S -> IO ()
main' initialState = do
  let config = CConf
        { confBackground = white
        }
  initGUI
  window <- windowNew
  set window
    [ windowTitle := "Concepted"
    , windowDefaultWidth := 320
    , windowDefaultHeight := 200
    , containerBorderWidth := 0
    ]
  canvas <- drawingAreaNew
  containerAdd window canvas
  widgetShowAll window 

  let ws = map widgets $ planes initialState
  ms <- mapM newMenu ws
  let ms' = M.fromList $ zip ws ms
  sVar <- newMVar initialState { menus = ms' }
  
  onKeyPress window $ \e -> do
    modifyMVar_ sVar $ \s -> execC config s $ myKeyPress (eventKeyName e)
    widgetQueueDraw canvas
    return True

  onButtonPress canvas $ \e -> do
    case eventButton e of
      LeftButton -> do
        modifyMVar_ sVar $ \s -> execC config s $ myLmbPress (Control `elem` eventModifier e) (eventX e, eventY e)
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onButtonRelease canvas $ \e -> do
    case eventButton e of
      LeftButton -> do
        modifyMVar_ sVar $ \s -> execC config s $ myLmbRelease (eventX e, eventY e)
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onScroll canvas $ \e -> do
    case eventDirection e of
      ScrollUp -> do
        modifyMVar_ sVar $ \s -> execC config s $ myScroll True
        widgetQueueDraw canvas
      ScrollDown -> do
        modifyMVar_ sVar $ \s -> execC config s $ myScroll False
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onMotionNotify canvas False $ \e -> do
    s <- takeMVar sVar
    -- TODO The first time onMotionNotify is called, the computed dx
    -- and dy are wrong.
    let dx = eventX e - fst (mouseXY s)
        dy = eventY e - snd (mouseXY s)
        lmb = Button1 `elem` (eventModifier e)
        rmb = Button3 `elem` (eventModifier e)
    s' <- execC config s { mouseXY = (eventX e, eventY e) } $ myMotion lmb rmb (dx, dy)
    putMVar sVar s'
    widgetQueueDraw canvas
    return True

  onExpose canvas $ \_ -> do
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    s <- takeMVar sVar
    let s' = s { width = fromIntegral w, height = fromIntegral h }
    putMVar sVar s'
    renderWithDrawable drawin (myDraw config s')
    return True
 
  onDestroy window mainQuit
  mainGUI

----------------------------------------------------------------------
-- The main callbacks
----------------------------------------------------------------------

myKeyPress :: String -> C ()
myKeyPress k = get >>= \s -> case k of
  "r" -> case filename s of
    Nothing -> pass
    Just fn -> do
      c <- liftIO $ readFile fn
      case unserialize c of
        Left err ->
          liftIO . putStrLn $ "parse error: " ++ show err
        Right s' -> do
          liftIO . putStrLn $ fn ++ " reloaded"
          change currentPlane $ \cp -> cp
            { concepts = concepts (getCurrentPlane s')
            , links = links (getCurrentPlane s')
            , follow = follow (getCurrentPlane s')
            }
  "plus" -> change currentPlane $ zoomAt (mouseXY s) 1.1
  "minus" -> change currentPlane $ zoomAt (mouseXY s) (1 / 1.1)
  "l" -> modify $ \s -> s { hideLinks = not (hideLinks s) }
  "c" -> do
    mxy <- gets mouseXY
    cp <- grab currentPlane
    let xy = screenToScene cp mxy
    change currentPlane $ newConcept xy
  "Up" -> change currentPlane $ pan (0, 20)
  "Down" -> change currentPlane $ pan (0, -20)
  "Left" -> change currentPlane $ pan (20, 0)
  "Right" -> change currentPlane $ pan (-20, 0)
  "Escape" -> liftIO mainQuit
  _ -> pass

myLmbPress :: Bool -> Point -> C ()
myLmbPress ctrl xy = do
  cp <- grab currentPlane
  let xy' = screenToScene cp xy
      selc = select IdConcept xy' (IM.toList $ concepts cp)
      sell = select IdLink xy' (IM.toList $ links cp)
      selh = selectLinksHandles xy' (IM.toList $ links cp)
      sel = take 1 $ concat [selc, sell, selh]

  pms <- gets planeMenuPairs
  liftIO $ mapM_ (\(p, m) -> pressMenu (screenToScene p xy) m) pms

  nail currentPlane $ cp { selection = if ctrl
    then nub (sel ++ selection cp)
    else if null sel then selection cp else sel}

myLmbRelease :: Point -> C ()
myLmbRelease xy = do
  s <- get
  let q Nothing = return ()
      q _ = mainQuit
  liftIO $ mapM_ (\(p, m) -> releaseMenu (screenToScene p xy) m >>= q) $ planeMenuPairs s

  change currentPlane $ snapSelection' $ snapTreshold s

pass :: Monad m => m ()
pass = return ()

-- The bool specifies if it is up (true) or down (false).
myScroll :: Bool -> C ()
myScroll up = do
  mxy <- gets mouseXY
  change currentPlane $ zoomAt mxy (if up then 1.1 else 1 / 1.1)

-- The booleans specify if the lmb and rmb are pressed.
myMotion :: Bool -> Bool -> (Double, Double) -> C ()
myMotion True False (dx, dy) = do
  cp <- grab currentPlane
  let dxy' = screenToSceneDelta cp (dx, dy)
  change currentPlane $ mapSelection (move dxy')
myMotion False True dxy = change currentPlane $ pan dxy
myMotion _ _ _ = pass

myDraw :: CConf -> S -> Render ()
myDraw config s = do
  -- clear
  setSourceRGBA' $ confBackground config
  paint

  mapM_ (renderPlane s) $ planeMenuPairs s

renderPlane :: S -> (Plane, Menu) -> Render ()
renderPlane s (p, m) = do
  -- view the scene under the pan/zoom transform
  identityMatrix
  translate (fst $ panXY p) (snd $ panXY p)
  scale (zoom p) (zoom p)

  -- render
  mapM_ (\(a,b) -> render (a `isSelectedConcept` p) b)
    (IM.toList $ concepts p)
  unless (hideLinks s) $
    mapM_ (\(a,b) -> renderLink (a `isSelectedLink` p) b)
      (IM.toList $ links p)
  mapM_ (\(a,b) -> mapM_ (\(i,j) -> renderHandle (IdLinkHandle a i `elem` selection p) j) $ zip [0..] $ handles b)
    (IM.toList $ links p)

  let pos = screenToScene p $ mouseXY s
  renderMenu pos m

----------------------------------------------------------------------
-- Process the selection
----------------------------------------------------------------------

mapSelection :: (forall a . Moveable a => a -> a) -> Plane -> Plane
mapSelection f s = s
  { concepts = IM.mapWithKey fc $ concepts s
  , links = IM.mapWithKey fh' $ IM.mapWithKey fl $ links s
  }
  where
  fol = follow s
  fc b n = if IdConcept b `elem` (selection s `addFollow` fol) then f n else n
  fl b n = if IdLink b `elem` (selection s `addFollow` fol) then f n else n
  fh' b n = mapHandles (fh b) n
  fh b (i,n) = if IdLinkHandle b i `elem` (selection s `addFollow` fol) then f n else n

mapHandles :: ((Int, Handle) -> Handle) -> Link -> Link
mapHandles f (Link xy rgba from verb to hs w) =
  let hs' = map f $ zip [0..] hs
  in Link xy rgba from verb to hs' w

snapSelection :: Int -> Plane -> Plane
snapSelection t = mapSelection (\n -> setPosition (snapXY t $ position n) n)

snapSelection' :: Maybe Int -> Plane -> Plane
snapSelection' (Just t) = snapSelection t
snapSelection' Nothing = id

addFollow :: [Id] -> [(Id,Id)] -> [Id]
addFollow [] _ = []
addFollow sel fllw = sel ++ mapMaybe f fllw
  where f (a,b) = if a `elem` sel then Just b else Nothing

