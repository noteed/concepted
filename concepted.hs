{-# Language RankNTypes #-}
{-# Language TupleSections #-}
module Main where

import System.Environment (getArgs)

import Graphics.UI.Gtk hiding
  ( eventKeyName, eventButton, eventModifier
  , Menu, Point, Rectangle, Widget
  , add
  )
import Graphics.UI.Gtk.Gdk.Events (
  eventX, eventY, eventKeyName, eventButton, eventDirection, eventModifier)
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Monad

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

background :: RGBA
background = white

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> main' $ replaceCurrentPlane cleanState $ (currentPlane cleanState) {widgets =
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
          , Button (240,20) "Configuration" "configure"
          ]}
    _ -> putStrLn "usage: concepted filename"

main' :: S -> IO ()
main' initialState = do
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
    s <- takeMVar sVar
    mstate <- myKeyPress (eventKeyName e) s
    case mstate of
      Nothing -> do
        putMVar sVar s
        return ()
      Just s' -> do
        putMVar sVar s'
        widgetQueueDraw canvas
    return True

  onButtonPress canvas $ \e -> do
    case eventButton e of
      LeftButton -> do
        s <- takeMVar sVar
        s' <- myLmbPress (Control `elem` eventModifier e) (eventX e, eventY e) s
        putMVar sVar s'
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onButtonRelease canvas $ \e -> do
    case eventButton e of
      LeftButton -> do
        s <- takeMVar sVar
        s' <- myLmbRelease (eventX e, eventY e) s
        putMVar sVar s'
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onScroll canvas $ \e -> do
    case eventDirection e of
      ScrollUp -> do
        s <- takeMVar sVar
        putMVar sVar (myScroll True s)
        widgetQueueDraw canvas
      ScrollDown -> do
        s <- takeMVar sVar
        putMVar sVar (myScroll False s)
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
    s' <- myMotion lmb rmb (dx, dy) $ s { mouseXY = (eventX e, eventY e) }
    putMVar sVar s'
    widgetQueueDraw canvas
    return True

  onExpose canvas $ \_ -> do
    (w,h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    s <- takeMVar sVar
    let s' = s { width = fromIntegral w, height = fromIntegral h }
    putMVar sVar s'
    renderWithDrawable drawin (myDraw s')
    return True
 
  onDestroy window mainQuit
  mainGUI

----------------------------------------------------------------------
-- The main callbacks
----------------------------------------------------------------------

myKeyPress :: String -> S -> IO (Maybe S)
myKeyPress k s = case k of
  "r" -> case filename s of
    Nothing -> return Nothing
    Just fn -> do
      c <- readFile fn
      case unserialize c of
        Left err -> do
          putStrLn $ "parse error: " ++ show err
          return Nothing
        Right s' -> do
          putStrLn $ fn ++ " reloaded"
          return . Just . replaceCurrentPlane s $ (currentPlane s)
            { concepts = concepts (currentPlane s')
            , links = links (currentPlane s')
            , follow = follow (currentPlane s')
            }
  "plus" -> return . Just . replaceCurrentPlane s $ zoomAt (mouseXY s) 1.1 (currentPlane s)
  "minus" -> return . Just . replaceCurrentPlane s $ zoomAt (mouseXY s) (1 / 1.1) (currentPlane s)
  "l" -> return . Just $ s { hideLinks = not (hideLinks s) }
  "c" -> do
    let xy = screenToScene (currentPlane s) (mouseXY s)
    return . Just . replaceCurrentPlane s $ newConcept xy (currentPlane s)
  "Up" -> return . Just . replaceCurrentPlane s $ pan (0, 20) (currentPlane s)
  "Down" -> return . Just . replaceCurrentPlane s $ pan (0, -20) (currentPlane s)
  "Left" -> return . Just . replaceCurrentPlane s $ pan (20, 0) (currentPlane s)
  "Right" -> return . Just . replaceCurrentPlane s $ pan (-20, 0) (currentPlane s)
  "Escape" -> mainQuit >> return Nothing
  _ -> return Nothing

myLmbPress :: Bool -> Point -> S -> IO S
myLmbPress ctrl xy s = do
  let xy' = screenToScene (currentPlane s) xy
      selc = select IdConcept xy' (IM.toList $ concepts $ currentPlane s)
      sell = select IdLink xy' (IM.toList $ links $ currentPlane s)
      selh = selectLinksHandles xy' (IM.toList $ links $ currentPlane s)
      sel = take 1 $ concat [selc, sell, selh]

  mapM_ (\(p, m) -> pressMenu (screenToScene p xy) m) $ planeMenuPairs s

  return . replaceCurrentPlane s $ (currentPlane s) { selection = if ctrl
    then nub (sel ++ selection (currentPlane s))
    else if null sel then selection $ currentPlane s else sel}

myLmbRelease :: Point -> S -> IO S
myLmbRelease xy s = do
  let q Nothing = return ()
      q _ = mainQuit
  mapM_ (\(p, m) -> releaseMenu (screenToScene p xy) m >>= q) $ planeMenuPairs s

  case selection $ currentPlane s of
    [] -> return s
    _ -> case snapTreshold s of
      Nothing -> return s
      Just t -> return . replaceCurrentPlane s $ snapSelection t $ currentPlane s

-- The bool specifies if it is up (true) or down (false).
myScroll :: Bool -> S -> S
myScroll up s = replaceCurrentPlane s $ if up
  then zoomAt (mouseXY s) 1.1 $ currentPlane s
  else zoomAt (mouseXY s) (1 / 1.1) $ currentPlane s

-- The booleans specify if the lmb and rmb are pressed.
myMotion :: Bool -> Bool -> (Double, Double) -> S -> IO S
myMotion True False (dx, dy) s = do
  let dxy' = screenToSceneDelta (currentPlane s) (dx, dy)
  return . replaceCurrentPlane s $ mapSelection (move dxy') $ currentPlane s
myMotion False True dxy s = return . replaceCurrentPlane s $ pan dxy $ currentPlane s
myMotion _ _ _ s = return s

myDraw :: S -> Render ()
myDraw s = do
  -- clear
  setSourceRGBA' background
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

addFollow :: [Id] -> [(Id,Id)] -> [Id]
addFollow [] _ = []
addFollow sel fllw = sel ++ mapMaybe f fllw
  where f (a,b) = if a `elem` sel then Just b else Nothing

