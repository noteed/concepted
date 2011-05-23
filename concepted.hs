{-# Language RankNTypes #-}
{-# Language TupleSections #-}
module Main where

import System.Environment (getArgs)

import Graphics.UI.Gtk hiding (
  eventKeyName, eventButton, eventModifier, Menu, Rectangle, Widget)
import Graphics.UI.Gtk.Gdk.Events (
  eventX, eventY, eventKeyName, eventButton, eventDirection, eventModifier)
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Monad

import Data.List
import Data.Maybe
import qualified Data.IntMap as IM

import Concepted.Graphics
import Concepted.Widget
import Concepted.Syntax.Parser
import Concepted.State

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
    [] -> main' $ cleanState {widgets =
      [ Label (10,20) "Alphaner"
      , Button (140,20) "Play" "play"
      , Button (240,20) "Configuration" "configure"
      ]}
    [fn] -> do
      c <- readFile fn
      case unserialize c of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right a -> main' $ a { filename = Just fn }
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

  sVar <- newMVar initialState
  menu <- newMenu (widgets initialState)

  onKeyPress window $ \e -> do
    s <- takeMVar sVar
    ms <- myKeyPress (eventKeyName e) s
    case ms of
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
        s' <- myLmbPress (Control `elem` eventModifier e) (eventX e) (eventY e) s menu
        putMVar sVar s'
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onButtonRelease canvas $ \e -> do
    case eventButton e of
      LeftButton -> do
        s <- takeMVar sVar
        s' <- myLmbRelease (eventX e) (eventY e) s menu
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
    -- The first time onMotionNotify is called, the computed dx
    -- and dy are wrong.
    let dx = eventX e - mouseX s
        dy = eventY e - mouseY s
    let lmb = Button1 `elem` (eventModifier e)
        rmb = Button3 `elem` (eventModifier e)
    s' <- myMotion lmb rmb dx dy $ s { mouseX = eventX e, mouseY = eventY e }
    putMVar sVar s'
    widgetQueueDraw canvas
    return True

  onExpose canvas $ \_ -> do
    (w,h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    s <- readMVar sVar
    renderWithDrawable drawin (myDraw
      (s { width = fromIntegral w, height = fromIntegral h })
      menu)
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
          return . Just $ s
            { concepts = concepts s'
            , links = links s'
            , follow = follow s'
            }
  "plus" -> return . Just $ zoomAt (mouseX s) (mouseY s) 1.1 s
  "minus" -> return . Just $ zoomAt (mouseX s) (mouseY s) (1 / 1.1) s
  "l" -> return . Just $ s { hideLinks = not (hideLinks s) }
  "c" -> do
    let (x,y) = screenToScene s (mouseX s, mouseY s)
    return . Just $ newConcept x y s
  "Up" -> return . Just $ pan 0 20 s
  "Down" -> return . Just $ pan 0 (-20) s
  "Left" -> return . Just $ pan 20 0 s
  "Right" -> return . Just $ pan (-20) 0 s
  "Escape" -> mainQuit >> return Nothing
  _ -> return Nothing

myLmbPress :: Bool -> Double -> Double -> S -> Menu -> IO S
myLmbPress ctrl x y s menu = do
  let (x',y') = screenToScene s (x,y)
      selc = select IdConcept x' y' (IM.toList $ concepts s)
      sell = select IdLink x' y' (IM.toList $ links s)
      selh = selectLinksHandles x' y' (IM.toList $ links s)
      sel = take 1 $ concat [selc, sell, selh]

  pressMenu (x',y') menu

  return $ s { selection = if ctrl
    then nub (sel ++ selection s)
    else if null sel then selection s else sel}

myLmbRelease :: Double -> Double -> S -> Menu -> IO S
myLmbRelease x y s menu = do
  let (x',y') = screenToScene s (x,y)
  b <- releaseMenu (x',y') menu
  case b of
    Nothing -> return ()
    Just _ -> mainQuit

  case selection s of
    [] -> return s
    _ -> case snapTreshold s of
      Nothing -> return s
      Just t -> return $ snapSelection t s

-- The bool specifies if it is up (true) or down (false).
myScroll :: Bool -> S -> S
myScroll up s = if up
  then zoomAt (mouseX s) (mouseY s) 1.1 s
  else zoomAt (mouseX s) (mouseY s) (1 / 1.1) s

-- The bools specifies if the lmb and rmb are pressed.
myMotion :: Bool -> Bool -> Double -> Double -> S -> IO S
myMotion True False dx dy s = do
  let (dx',dy') = screenToSceneDelta s (dx,dy)
  return $ mapSelection (move dx' dy') s
myMotion False True dx dy s = return $ pan dx dy s
myMotion _ _ _ _ s = return s

myDraw :: S -> Menu -> Render ()
myDraw s menu = do
  -- clear
  setSourceRGBA' background
  paint

  -- view the scene under the pan/zoom transform
  translate (panX s) (panY s)
  scale (zoom s) (zoom s)

  -- render
  mapM_ (\(a,b) -> render (a `isSelectedConcept` s) b)
    (IM.toList $ concepts s)
  unless (hideLinks s) $
    mapM_ (\(a,b) -> renderLink (a `isSelectedLink` s) b)
      (IM.toList $ links s)
  mapM_ (\(a,b) -> mapM_ (\(i,j) -> renderHandle (IdLinkHandle a i `elem` selection s) j) $ zip [0..] $ handles b)
    (IM.toList $ links s)

  let pos = screenToScene s (mouseX s, mouseY s)
  renderMenu pos menu

----------------------------------------------------------------------
-- Process the selection
----------------------------------------------------------------------

mapSelection :: (forall a . Moveable a => a -> a) -> S -> S
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
mapHandles f (Link x y rgba from verb to hs w) =
  let hs' = map f $ zip [0..] hs
  in Link x y rgba from verb to hs' w

snapSelection :: Int -> S -> S
snapSelection t = mapSelection sn
  where
  sn n = let (a,b) = position n
         in setPosition (snap t a) (snap t b) n

addFollow :: [Id] -> [(Id,Id)] -> [Id]
addFollow [] _ = []
addFollow sel fllw = sel ++ mapMaybe f fllw
  where f (a,b) = if a `elem` sel then Just b else Nothing

----------------------------------------------------------------------
-- Manipulate S
----------------------------------------------------------------------

newConcept :: Double -> Double -> S -> S
newConcept x y s = s { concepts = IM.insert (IM.size $ concepts s) c $ concepts s }
  where c = Text x y black 20.0 14 ("concept #" ++ show (IM.size $ concepts s))

----------------------------------------------------------------------
-- Convenience functions
----------------------------------------------------------------------

-- Transform from screen coordinate to scene coordinate.
screenToScene :: S -> (Double,Double) -> (Double,Double)
screenToScene s (x,y) = ((x - panX s) / zoom s, (y - panY s) / zoom s)

-- Transform from screen coordinate delta to scene coordinate delta.
screenToSceneDelta :: S -> (Double, Double) -> (Double, Double)
screenToSceneDelta s (dx,dy) = (dx / zoom s, dy / zoom s)

-- Add dx and dy to the pan.
pan :: Double -> Double -> S -> S
pan dx dy s = s { panX = panX s + dx, panY = panY s + dy }

-- Multiply the zoom by a, modifying the panX and panY values
-- so that the scene-point under the screen coordinate (x,y)
-- remains at the same screen coordiante.
zoomAt :: Double -> Double -> Double -> S -> S
zoomAt x y a s =
  let (x1,y1) = screenToScene s (x,y)
      s' = s { zoom = zoom s * a }
      (x2,y2) = screenToScene s' (x,y)
  in s'
    { panX = panX s' - (x1 - x2) * zoom s'
    , panY = panY s' - (y1 - y2) * zoom s'
    }

-- Set n to its nearest multiple of t.
snap :: Int -> Double -> Double
snap t n = fromIntegral $
  let n' = floor n
      m =  n' `mod` t
  in if m > 5 then n' + t - m else n' - m

