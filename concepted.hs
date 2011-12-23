{-# Language RankNTypes #-}
{-# Language TupleSections #-}
{-# Language DeriveDataTypeable #-}
{-# Language OverloadedStrings #-}
module Main where

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
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import System.Console.CmdArgs.Implicit hiding ((:=))
import Data.Time.Clock

import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map as M

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.Serialize as S
import Unsafe.Coerce (unsafeCoerce)
import Data.Word (Word8, Word64)
import Data.Bits (bit, testBit, (.|.))

import Network.Silo hiding (handle)

import Concepted.Graphics
import Concepted.Widget
import Concepted.Syntax.Parser
import Concepted.Plane
import Concepted.State
import Concepted.Misc (snapXY)
import Concepted.Zombies

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
main = (>>= processCmd) . cmdArgs $
  modes
    [ edit
    ]
  &= summary versionString
  &= program "concepted"

versionString :: String
versionString =
  "concepted " ++ showVersion version ++
  " -- Copyright (c) 2010-2011 Vo Minh Thu."

data Cmd =
    Edit
    { editFile :: FilePath
    , isServer :: Bool
    }
  deriving (Data, Typeable, Show, Eq)

edit :: Cmd
edit = Edit
  { editFile = def
    &= typFile
    &= argPos 0
    &= opt ("" :: String)
  , isServer = def
    &= explicit
    &= name "s"
    &= name "server"
    &= help "Start minizombs in server mode."
  } &= help "Edit a concept map."

processCmd :: Cmd -> IO ()
processCmd (Edit "" b) = do
  main' $ replaceCurrentPlane cleanState { wserver = b } $ (getCurrentPlane cleanState)
    { widgets =
      [ Label (10,20) "Concepted"
      , Button (140,20) "Reset" reset
      , Button (240,20) "Quit" (liftIO mainQuit)
      ]
    }

processCmd (Edit fn _) = do
  c <- readFile fn
  case unserialize c of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right a -> main' $ a
      { filename = Just fn
      , handlers = [HandlerState xxx ()]
      } `addPlane` emptyPlane
      { widgets =
        [ Label (10,20) "Concepted"
        , Button (140,20) "Pass" pass
        , Button (240,20) "Quit" (liftIO mainQuit)
        ]
      }

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
  t <- getCurrentTime
  sVar <- newMVar initialState { menus = ms', wtime = t }
  
  onKeyPress window $ \e -> do
    modifyState config sVar $ myKeyPress (eventKeyName e)
    widgetQueueDraw canvas
    return True

  onKeyRelease window $ \e -> do
    modifyState config sVar $ myKeyRelease (eventKeyName e)
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
    (r, s'') <- do
      widgetQueueDraw canvas
      runC config s' (advance >> myDraw)
    putMVar sVar s''
    renderWithDrawable drawin r
    return True
{-
  flip idleAdd priorityLow $ do
    s <- takeMVar sVar
    s' <- execC config s advance
    putMVar sVar s'
    return True
 -}
  onDestroy window mainQuit
  when (wserver initialState) $ do
    putStrLn "minizombs in server mode"
    _ <- forkIO $ serve Nothing 9000 $ handler config sVar
    return ()
  when (not $ wserver initialState) $ do
    _ <- forkIO $ messageThread config sVar
    return ()
  mainGUI

handler :: CConf -> MVar CState -> [Message] -> IO ()
handler config sVar msgs = do
  s <- takeMVar sVar
  s' <- execC config s $ do
    mapM_ player2 msgs
  putMVar sVar s'
  return ()

playerShoot player =
  change currentPlane $ \p -> p { pBullets = PlayerBullet (pPosition $ player p)
    (normalize (pMouse (player p) `sub` pPosition (player p)) `muls` 2) : take 20 (pBullets p) }

messageThread config sVar = do
  s <- takeMVar sVar
  s' <- execC config s $ do
    sendMessages
    processMessages
  putMVar sVar s'
  threadDelay (50 * 1000) -- in microseconds -- TODO avoid driftin -- TODO avoid drifting
  messageThread config sVar

io = liftIO

player2 :: Message -> C ()
player2 msg = case msg of
  Shoot -> io (print "shooted") >> playerShoot pPlayer2
  Look x y -> io (print "looked") >> (change currentPlane $ changePlayer2 (setMouse (x, y)))
  Move a -> change currentPlane $ changePlayer2 (setInput a)
  _ -> return ()

changePlayer1 f p = p { pPlayer1 = f $ pPlayer1 p }

changePlayer2 f p = p { pPlayer2 = f $ pPlayer2 p }

modifyState :: CConf -> MVar CState -> C a -> IO ()
modifyState config sVar f =
  modifyMVar_ sVar $ \s -> execC config s f

advance :: C ()
advance = do
  s <- get

  t <- liftIO getCurrentTime
  let dt = realToFrac (t `diffUTCTime` wtime s) -- in seconds
      available = dt + waccumulated s
      steps = floor $ available / stepsize :: Int
      remaining = available - (fromIntegral steps * stepsize)
  put s { wtime = t, waccumulated = remaining }

  change currentPlane $ \p -> p { pPlayer1 = updateN steps $ pPlayer1 p }
  change currentPlane $ \p -> p { pPlayer2 = updateN steps $ pPlayer2 p }
  change currentPlane $ \p -> p { pBullets = map (updateBulletN steps) $ pBullets p } -- TODO prune bullets
  change currentPlane $ \p -> p { pZombies = filter (not . isDead (pBullets p)) $ pZombies p }

reset :: C ()
reset = change currentPlane $ \p -> p { pZombies = pZombies emptyPlane }


----------------------------------------------------------------------
-- The main callbacks
----------------------------------------------------------------------

myKeyPress :: String -> C ()
myKeyPress k = do
  sv <- gets wserver
  let changePlayer = if sv then changePlayer1 else changePlayer2
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
        "z" -> change currentPlane $ changePlayer $ setUp True
        "q" -> change currentPlane $ changePlayer $ setLeft True
        "s" -> change currentPlane $ changePlayer $ setDown True
        "d" -> change currentPlane $ changePlayer $ setRight True
        "x" -> do
          if wserver s
            then playerShoot pPlayer1
            else queue Shoot
        "Up" -> change currentPlane $ pan (0, 20) -- TODO change the zombi mouse info
        "Down" -> change currentPlane $ pan (0, -20)
        "Left" -> change currentPlane $ pan (20, 0)
        "Right" -> change currentPlane $ pan (-20, 0)
        "Print" -> do
          config <- ask
          r <- liftIO $ evalC config s myDraw -- discard modified state
          liftIO $ withImageSurface FormatARGB32 (wwidth s) (wheight s) $
            \surf -> do
              renderWith surf r
              surfaceWriteToPNG surf "screenshot.png"
        "Escape" -> liftIO mainQuit
        _ -> pass

myKeyRelease :: String -> C ()
myKeyRelease k = do
  sv <- gets wserver
  let changePlayer = if sv then changePlayer1 else changePlayer2
  b <- handle $ Key k False
  if b
    then return ()
    else do
      case k of
        "z" -> change currentPlane $ changePlayer $ setUp False
        "q" -> change currentPlane $ changePlayer $ setLeft False
        "s" -> change currentPlane $ changePlayer $ setDown False
        "d" -> change currentPlane $ changePlayer $ setRight False
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
myMotion lmb rmb (dx, dy) = do
  cp <- grab currentPlane
  xy <- gets mouseXY
  let xy' = screenToPlane cp xy
  sv <- gets wserver
  let changePlayer = if sv then changePlayer1 else changePlayer2
  when (not sv) $ queue (Look (fst xy') (snd xy'))
  change currentPlane $ changePlayer (setMouse xy')
  myMotion' lmb rmb (dx, dy)

myMotion' :: Bool -> Bool -> (Double, Double) -> C ()
myMotion' True False (dx, dy) = do
  cp <- grab currentPlane
  let dxy' = screenToPlaneDelta cp (dx, dy)
  change currentPlane $ mapSelection (move dxy')
myMotion' False True dxy = do
  change currentPlane $ pan dxy
myMotion' _ _ _ = return ()

myDraw :: C (Render ())
myDraw = do
  config <- ask
  s <- get

  return $ do
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
  mapM_ renderZombie (pZombies p)
  renderPlayer $ pPlayer1 p
  renderPlayer $ pPlayer2 p
  mapM_ renderBullet $ pBullets p

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

instance S.Serialize Message where
  put Shoot = S.put (0 :: Word8)
  put (Look x y) = S.put (1 :: Word8) >> putDouble x >> putDouble y
  put (Move a) = S.put (2 :: Word8) >> S.put a
  get = do
    i <- S.get :: S.Get Word8
    case i of
      0 -> return Shoot
      1 -> Look <$> getDouble <*> getDouble
      2 -> Move <$> S.get

instance S.Serialize Zombie where
  put (Zombie xy) = putDoublePair xy
  get = Zombie <$> getDoublePair

instance S.Serialize PlayerInput where
  put = S.put . playerInputToWord8
  get = playerInputFromWord8 <$> S.get

instance S.Serialize Player where
  put (Player a b c d) = putDoublePair a >> S.put b >> S.put c >> putDoublePair d
  get = Player <$> getDoublePair <*> S.get <*> S.get <*> getDoublePair

instance S.Serialize PlayerBullet where
  put (PlayerBullet a b) = putDoublePair a >> putDoublePair b
  get = PlayerBullet <$> getDoublePair <*> getDoublePair

instance S.Serialize Game where
  put (Game a b c d) = S.put a >> S.put b >> S.put c >> S.put d
  get = Game <$> S.get <*> S.get <*> S.get <*> S.get

putDoublePair :: (Double, Double) -> S.Put
putDoublePair (a, b) = putDouble a >> putDouble b

getDoublePair :: S.Get (Double, Double)
getDoublePair = (,) <$> getDouble <*> getDouble

putDouble :: Double -> S.Put
putDouble d = S.put (unsafeCoerce d :: Word64)

getDouble :: S.Get Double
getDouble = S.get >>= return . (unsafeCoerce :: Word64 -> Double)

playerInputToWord8 :: PlayerInput -> Word8
playerInputToWord8 (PlayerInput a b c d) =
  a' .|. b' .|. c' .|. d'  
  where a' = if a then bit 0 else 0
        b' = if b then bit 1 else 0
        c' = if c then bit 2 else 0
        d' = if d then bit 3 else 0

playerInputFromWord8 :: Word8 -> PlayerInput
playerInputFromWord8 w = PlayerInput a b c d
  where a = testBit w 0
        b = testBit w 1
        c = testBit w 2
        d = testBit w 3

sendMessages :: C ()
sendMessages = do
  cp <- grab currentPlane
  --queue (Move . pInput $ pPlayer2 cp)
  s <- get
  let f (t, p, _) = t == turn s + 2 && p == wserver s -- send this turn's messages to the other end.
      g (_, _, m) = m
      msgs = map g $ filter f $ messages s
      (host, port) | wserver s = wclient s
                   | otherwise = ("127.0.0.1", 9000)
  when (not $ null msgs) $
    io $ send host port msgs

processMessages :: C ()
processMessages = do
  s <- get
  let f (t, _, _) = t == turn s
      (before, after) = span f $ messages s
  mapM_ processMessage before
  s <- get
  put s { messages = after, turn = turn s + 1 }

processMessage (_, _, m) = case m of
  Shoot -> io (print "shooted+++") >> playerShoot pPlayer2
--  Look x y -> io (print "looked") >> (change currentPlane $ changePlayer2 (setMouse (x, y)))
--  Move a -> change currentPlane $ changePlayer2 (setInput a)
  _ -> return ()
