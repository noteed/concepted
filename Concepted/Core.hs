{-# Language RankNTypes #-}
{-# Language TupleSections #-}
{-# Language DeriveDataTypeable #-}
module Concepted.Core where

import Paths_concepted (version)
import Data.Version (showVersion)

import Graphics.UI.Gtk hiding
  ( eventKeyName, eventButton, eventModifier
  , Menu, Point, Rectangle, Widget
  , add, get
  )
import Graphics.UI.Gtk.Gdk.Events (
  eventX, eventY, eventKeyName, eventButton, eventDirection, eventModifier)
import Graphics.Rendering.Cairo hiding (
  status, versionString, version)

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import System.Console.CmdArgs.Implicit hiding ((:=))

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

linkSplitter :: Handler (Maybe Int)
linkSplitter (Key "s" True) Nothing = do
  sel <- gets $ selection . getCurrentPlane
  case sel of
    [IdLink i] -> do
      status "link split"
      return . Continue $ Just i
    _ -> return Ignored
linkSplitter (Key "s" True) (Just i) = do
  s <- get
  let cp = getCurrentPlane s
      Just (Link p a b c d (Handle q:ps) e) = IM.lookup i $ links cp
      pq = q `sub` p `divs` 2 `add` p
  put $ replaceCurrentPlane s cp { links = IM.insert i (Link p a b c d (Handle pq:Handle q:ps) e) $ links cp }
  return . Continue $ Just i
linkSplitter _ _ = return Ignored

data LineEditor = NewLine Int

lineEditor :: Handler LineEditor
lineEditor (Key "lmb" True) (NewLine i) = do
  s <- get
  let cp = getCurrentPlane s
      mxy = mouseXY s
      xy = screenToPlane cp mxy
      Just (Line ps) = IM.lookup i $ pLines cp
  put $ replaceCurrentPlane s cp { pLines = IM.insert i (Line $ ps ++ [Handle xy]) $ pLines cp }
  return . Continue $ NewLine i
lineEditor (Key "Escape" True) _ = do
  status "Stopped line editing"
  return End
lineEditor _ _ = return Ignored

xxx :: Handler ()
xxx (Key "space" True) _ = do
  sel <- gets $ selection . getCurrentPlane
  status $ "Selection: " ++ show sel
  return $ Continue ()
xxx e _ = do
  status $ "Pressed " ++ show e
  return Ignored

main' :: CState -> IO ()
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
    modifyState config sVar $ myKeyPress (eventKeyName e)
    widgetQueueDraw canvas
    return True

  onButtonPress canvas $ \e -> do
    case eventButton e of
      LeftButton -> do
        modifyState config sVar $ myLmbPress (Control `elem` eventModifier e)
          (eventX e, eventY e)
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onButtonRelease canvas $ \e -> do
    case eventButton e of
      LeftButton -> do
        modifyState config sVar $ myLmbRelease (eventX e, eventY e)
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onScroll canvas $ \e -> do
    case eventDirection e of
      ScrollUp -> do
        modifyState config sVar $ myScroll True
        widgetQueueDraw canvas
      ScrollDown -> do
        modifyState config sVar $ myScroll False
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
    s' <- execC config s { mouseXY = (eventX e, eventY e) } $
      myMotion lmb rmb (dx, dy)
    putMVar sVar s'
    widgetQueueDraw canvas
    return True

  onExpose canvas $ \_ -> do
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    s <- takeMVar sVar
    let s' = s { wwidth = w, wheight = h }
    putMVar sVar s'
    renderWithDrawable drawin (myDraw config s')
    return True
 
  onDestroy window mainQuit
  mainGUI

modifyState :: CConf -> MVar CState -> C a -> IO ()
modifyState config sVar f =
  modifyMVar_ sVar $ \s -> execC config s f


----------------------------------------------------------------------
-- The main callbacks
----------------------------------------------------------------------

myKeyPress :: String -> C ()
myKeyPress k = do
  b <- handle $ Key k True
  if b
    then return ()
    else do
      s <- get
      case k of
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
        "l" -> put $ s { hideLinks = not (hideLinks s) }
        "c" -> changeAtXY currentPlane newConcept
        "n" -> do
          i <- newLine
          modify (\s' -> s' { handlers = HandlerState lineEditor (NewLine i) : handlers s })
          status $ "Editting line #" ++ show i ++ ", press Escape to stop"
        "Up" -> change currentPlane $ pan (0, 20)
        "Down" -> change currentPlane $ pan (0, -20)
        "Left" -> change currentPlane $ pan (20, 0)
        "Right" -> change currentPlane $ pan (-20, 0)
        "Print" -> do
          config <- ask
          liftIO $ withImageSurface FormatARGB32 (wwidth s) (wheight s) $
            \surf -> do
              renderWith surf $ myDraw config s
              surfaceWriteToPNG surf "screenshot.png"
        "Escape" -> liftIO mainQuit
        _ -> pass

myLmbPress :: Bool -> Point -> C ()
myLmbPress ctrl xy = do
  b <- handle $ Key "lmb" True
  if b
    then return ()
    else do
      cp <- grab currentPlane
      let xy' = screenToPlane cp xy
          selc = select IdConcept xy' (IM.toList $ concepts cp)
          sell = select IdLink xy' (IM.toList $ links cp)
          selh = selectLinksHandles xy' (IM.toList $ links cp)
          sel = take 1 $ concat [selc, sell, selh]

      pms <- gets planeMenuPairs
      liftIO $ mapM_ (\(p, m) -> pressMenu (screenToPlane p xy) m) pms

      nail currentPlane $ cp { selection = if ctrl
        then nub (sel ++ selection cp)
        else if null sel then selection cp else sel}

myLmbRelease :: Point -> C ()
myLmbRelease xy = do
  s <- get
  cmds <- liftIO $ mapM (\(p, m) -> releaseMenu (screenToPlane p xy) m) $ planeMenuPairs s
  sequence_ $ mapMaybe id cmds

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
  let dxy' = screenToPlaneDelta cp (dx, dy)
  change currentPlane $ mapSelection (move dxy')
myMotion False True dxy = change currentPlane $ pan dxy
myMotion _ _ _ = pass

myDraw :: CConf -> CState -> Render ()
myDraw config s = do
  -- clear
  identityMatrix
  setSourceRGBA' $ confBackground config
  paint

  -- status bar
  setFontSize 12
  setSourceRGBA' black
  moveTo 5 $ (fromIntegral $ wheight s) - 5
  showText $ wstatus s

  mapM_ (renderPlane s) $ planeMenuPairs s

renderPlane :: CState -> (Plane, Menu) -> Render ()
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
  mapM_ (\(_,b) -> renderLine b) (IM.toList $ pLines p)

  let pos = screenToPlane p $ mouseXY s
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

-- | Similar to 'change' used in the C monad, but also provide the
-- plane-local mouse coordinates.
changeAtXY :: GN CState a -> (Point -> a -> a) -> C ()
changeAtXY gn f = do
  mxy <- gets mouseXY
  cp <- grab currentPlane
  let xy = screenToPlane cp mxy
  change gn $ f xy

newLine :: C Int
newLine = do
  change currentPlane newLine'
  gets $ pred . IM.size . pLines . getCurrentPlane

