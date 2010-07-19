module Main where

import System.Environment (getArgs)

import Graphics.UI.Gtk hiding (
  eventKeyName, eventButton, eventModifier, Rectangle)
import Graphics.UI.Gtk.Gdk.Events (
  eventX, eventY, eventKeyName, eventButton, eventModifier)
import Graphics.Rendering.Cairo

import Data.IORef
import Control.Concurrent
import Control.Monad

import Data.List
import Data.Maybe
import Text.PrettyPrint hiding (char, render)
import Text.ParserCombinators.Parsec

-- TODO: after a parse error when reloading the file with 'r',
-- (and reloading again) the program is stuck.
-- TODO: in xmonad, the meta-shift-c (close) doesn't work since
-- I have added the onKeyPress callback.
-- TODO: it would be nice to automitacally reload a file when it
-- is externally modified.

----------------------------------------------------------------------
-- The main program
----------------------------------------------------------------------

background :: RGBA
background = white

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      c <- readFile fn
      case unserialize c of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right a -> main' $ a { filename = Just fn }
    _ -> putStrLn "usage: TODO"

main' :: S -> IO ()
main' myState = do
  initGUI
  window <- windowNew
  set window
    [ windowTitle := "Cairo Attempt"
    , windowDefaultWidth := 320
    , windowDefaultHeight := 200
    , containerBorderWidth := 0
    ]
  canvas <- drawingAreaNew
  containerAdd window canvas
  widgetShowAll window 

  sVar <- newMVar myState
  xRef <- newIORef 0
  yRef <- newIORef 0

  forkIO (myCommand sVar $ widgetQueueDraw canvas)

  onKeyPress window $ \e -> do
    s <- takeMVar sVar
    ms <- myKeyPress (eventKeyName e) s
    case ms of
      Nothing -> return ()
      Just s' -> do
        putMVar sVar s'
        widgetQueueDraw canvas
    return True

  onButtonPress canvas $ \e -> do
    s <- takeMVar sVar
    case eventButton e of
      LeftButton -> do
        s' <- myLmbPress (eventX e) (eventY e) s
        putMVar sVar s'
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onMotionNotify canvas False $ \e -> do
    s <- takeMVar sVar
    x <- readIORef xRef
    y <- readIORef yRef
    -- The first time onMotionNotify is called, the computed dx
    -- and y are wrong.
    let dx = eventX e - x
        dy = eventY e - y
    writeIORef xRef (eventX e)
    writeIORef yRef (eventY e)
    let lmb = Button1 `elem` (eventModifier e)
    s' <- myMotion lmb dx dy s
    putMVar sVar s'
    widgetQueueDraw canvas
    return True

  onExpose canvas $ \_ -> do
    (w,h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    s <- readMVar sVar
    renderWithDrawable drawin (myDraw $
      s { width = fromIntegral w, height = fromIntegral h })
    return True
 
  onDestroy window mainQuit
  mainGUI

----------------------------------------------------------------------
-- The main state of the program
----------------------------------------------------------------------

data S = S
  { width :: Double
  , height :: Double
  , filename :: Maybe String
  , panX :: Double
  , panY :: Double
  , nodes :: [Node]
  , selection :: [Int]
  -- if (a,b) is in follow then whenever a is moved, b is moved too
  , follow :: [(Int,Int)]
  }
  deriving (Show)

cleanState :: S
cleanState = S
  { width = 320
  , height = 200
  , filename = Nothing
  , panX = 0
  , panY = 0
  , nodes = []
  , selection = []
  , follow = []
  }

myState = S
  { width = 320.0
  , height = 200.0
  , filename = Nothing
  , panX = 0.0
  , panY = 0.0
  , nodes =
  [
    Rectangle 0.0 0.0 (1.0,0.7,0.0,1.0) 100.0 40.0
    , Text 100.0 100.0 (0.0,0.0,0.0,1.0) 20.0 25 "Concepted"
    , Text 100.0 120.0 (0.0,0.0,0.0,1.0) 12.0 25 "Concept mapping tool"
    , Text 200.0 260.0 (0.0,0.0,0.0,1.0) 20.0 25 "Lorem ipsum dolor"
    , Text 200.0 300.0 (0.0,0.0,0.0,1.0) 12.0 25 "Lorem ipsum dolor\nsit\namet,\nconsectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat."
    , Handle 10.0 10.0
    , Handle 120.0 50.0
    , Handle 180.0 30.0
    , Handle 300.0 400.0
    , Link 50.0 50.0 (0.0,1.0,1.0,1.0) 1 "is way too" 3 [5,6,7,8,10] 2.0
  ]
  , selection = []
  , follow = [(0,1)]
  }

----------------------------------------------------------------------
-- The main callbacks
----------------------------------------------------------------------

myKeyPress :: String -> S -> IO (Maybe S)
myKeyPress k s = case k of
  "r" -> case filename s of
    Just fn -> do
      c <- readFile fn
      case unserialize c of
        Left err -> do
          putStrLn $ "parse error: " ++ show err
          return Nothing
        Right s' -> return . Just $ s { nodes = nodes s', follow = follow s' }
    Nothing -> return Nothing
  _ -> return Nothing

myLmbPress :: Double -> Double -> S -> IO S
myLmbPress x y s = do
  let sel = select (x - panX s) (y - panY s) (nodes s)
  return $ s { selection = sel }

myMotion :: Bool -> Double -> Double -> S -> IO S
myMotion lmb dx dy s = do
  if lmb
    then do
      let ns = zip ([0..]) (nodes s)
          sel = selection s
          f (b,n) = if b `elem` (sel `addFollow` follow s) then move dx dy n else n
          nodes' = map f ns
      if not (null sel)
        then return $ s { nodes = nodes' }
        else return s { panX = panX s + dx, panY = panY s + dy }
    else return s

addFollow :: [Int] -> [(Int,Int)] -> [Int]
addFollow [] _ = []
addFollow sel fllw = sel ++ mapMaybe (flip lookup fllw) sel

myDraw :: S -> Render ()
myDraw s = do
  setSourceRGBA' background
  paint

  translate (panX s) (panY s)

  mapM_ (\(a,b) -> render (nodes s) (a `elem` selection s) b)
    (zip [0..] (nodes s))

myCommand :: MVar S -> IO () -> IO ()
myCommand sVar redraw = do
  l <- getLine
  case l of
    "rect" -> do
      s <- takeMVar sVar
      putMVar sVar $ s
        { nodes = Rectangle 100 100 (1,0,0,1) 100 100:nodes s }
      redraw
    _ -> putStrLn l

----------------------------------------------------------------------
-- Data
----------------------------------------------------------------------

type RGBA = (Double,Double,Double,Double)

black :: RGBA
black = (0,0,0,1)

white :: RGBA
white = (1,1,1,1)

grey :: RGBA
grey = (0.6,0.6,0.6,1)

lightGrey :: RGBA
lightGrey = (0.9,0.9,0.9,1)

cyan :: RGBA
cyan = (0,1,1,1)

orange :: RGBA
orange = (1,0.7,0,1)

data Node =
    -- x y (the center, not the upper-left corner)
    Handle Double Double
    -- x y rgba width height
  | Rectangle Double Double RGBA Double Double
    -- x y rgba text-size max-line-length content
  | Text Double Double RGBA Double Int String
    -- x y from verb to control-points (probably handles) rgba line-width
  | Link Double Double RGBA Int String Int [Int] Double
  deriving (Read, Show)

position :: Node -> (Double,Double)
position (Handle x y) = (x,y)
position (Rectangle x y _ _ _) = (x,y)
position (Text x y _ _ _ _) = (x,y)
position (Link x y _ _ _ _ _ _) = (x,y)

render :: [Node] -> Bool -> Node -> Render ()
render _ selected (Handle x y) = do
  setLineWidth 0.6
  setSourceRGBA' lightGrey
  rectangle (x-10) (y-10) 20 20
  if selected then fill else stroke
render _ selected (Rectangle x y rgba w h) = do
  setSourceRGBA' rgba
  rectangle x y w h
  fill
  when selected $ do
    setSourceRGBA 0 0 0 1
    rectangle x y w h
    stroke

render _ selected (Text x y rgba sz n ss) = do
  -- the square handle
  render undefined selected (Handle (x-12) (y-12-sz)) -- hardcoded in pickText
  -- the info line
  setFontSize 10
  moveTo (x+2) (y-sz)
  showText $ unwords [show x, show y, showRGBA rgba, show sz, show n]
  -- the text
  setFontSize sz
  setSourceRGBA' rgba
  let txts = zip [(x,y + yi) | yi <- [0,sz*1.2..]] $ chop n ss
      f ((xi,yi),s) = do
        moveTo xi yi
        showText s
  mapM_ f txts

render pts selected (Link x y rgba _ v _ ps lw) = do
  -- the square handle
  render undefined selected (Handle x y)
  -- the verb
  setFontSize 10
  moveTo x y
  setSourceRGBA' rgba
  showText v
  -- the line
  setLineJoin LineJoinRound
  setLineWidth lw
  setSourceRGBA' rgba
  mapM_ (uncurry lineTo) (map (position . (pts !!)) ps)
  stroke

pick :: Double -> Double -> Node -> Bool
pick a b (Handle x y) =
  containXYWH a b (x-10) (y-10) 20 20

pick a b (Rectangle x y _ w h) =
  containXYWH a b x y w h

pick a b (Text x y _ sz _ _) =
  containXYWH a b (x-22) (y-sz-22) 20 20 -- hardcoded in render Text

pick a b (Link x y _ _ _ _ _ _) =
  containXYWH a b (x-10) (y-10) 20 20

move :: Double -> Double -> Node -> Node
move dx dy (Link x y rgba f v t ps lw) =
  Link (x + dx) (y + dy) rgba f v t ps lw

move dx dy (Handle x y) =
  Handle (x + dx) (y + dy)

move dx dy (Rectangle x y rgba w h) =
  Rectangle (x + dx) (y + dy) rgba w h

move dx dy (Text x y rgba sz n ss) =
  Text (x + dx) (y + dy) rgba sz n ss

-- Filter the selected nodes.
select :: Double -> Double -> [Node] -> [Int]
select x y = map fst . filter (pick x y . snd) . zip [0..]

----------------------------------------------------------------------
-- Convenience functions
----------------------------------------------------------------------

setSourceRGBA' :: RGBA -> Render ()
setSourceRGBA' (r,g,b,a) = setSourceRGBA r g b a

showRGBA :: RGBA -> String
showRGBA rgba
  | rgba == black = "black"
  | rgba == white = "white"
  | rgba == grey = "grey"
  | rgba == lightGrey = "lightGrey"
  | rgba == cyan = "cyan"
  | rgba == orange = "orange"
showRGBA (r,g,b,a) = unwords ["(",show r,",",show g,",",show b,",",show a,")"]

-- Test if the rectangle (x,y,w,h) contains the point (a,b).
containXYWH :: Double -> Double -> Double -> Double -> Double -> Double -> Bool
containXYWH a b x y w h =
  a >= x && b >= y && a <= x + w && b <= y + h

-- Givent a string, break it into a list of strings
-- - at each '\n'
-- - so that the sum of the words of each string is <= n
chop :: Int -> String -> [String]
chop n str = concatMap (f n [] [] . words) ls
  where ls = lines str
        f _ xs ys [] = ys ++ [unwords xs]
        f n' xs ys (w:ws) = let m = max n' (length w) in
          if sum (map length xs) + length w > m
          then f m [] (ys ++ [unwords xs]) (w:ws)
          else f m (xs++[w]) ys ws

----------------------------------------------------------------------
-- Pretty-printing
----------------------------------------------------------------------

tshow :: Show a => a -> Doc
tshow = text . show

ppS :: S -> Doc
ppS s = (vcat . map ppNode . nodes) s
  $+$ (vcat . map ppFollow . follow) s

ppNode :: Node -> Doc
ppNode n = case n of
  Handle x y ->
    text ":handle" <+> double x <+> double y
  Rectangle x y rgba w h ->
    text ":rectangle" <+> double x <+> double y <+> tshow rgba
    <+> double w <+> double h
  Text x y rgba sz w c ->
    text ":text" <+> double x <+> double y <+> tshow rgba
    <+> double sz <+> int w $+$ text c
  Link x y rgba f v t hs lw ->
    text ":link" <+> double x <+> double y <+> tshow rgba
    <+> int f <+> tshow v <+> int t <+> tshow hs <+> double lw

ppFollow :: (Int,Int) -> Doc
ppFollow (a,b) = text ":follow" <+> int a <+> int b

serialize :: S -> String
serialize = renderStyle (style { lineLength = 80 }) . ppS

----------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------

type P a = GenParser Char () a

hspaces :: P ()
hspaces = many (oneOf " ") >> return ()

pDouble :: P Double
pDouble = do
  a <- many1 digit
  b <- option "0" (char '.' >> many1 digit)
  hspaces
  return . read $ a ++ "." ++ b

pInt :: P Int
pInt = do
  a <- many1 digit
  hspaces
  return . read $ a

pRGBA :: P RGBA
pRGBA = do
  char '('
  r <- pDouble
  char ','
  g <- pDouble
  char ','
  b <- pDouble
  char ','
  a <- pDouble
  char ')'
  hspaces
  return (r,g,b,a)

pString :: P String
pString = do
  char '"'
  s <- many1 (noneOf "\"\n")
  char '"'
  hspaces
  return s

pInts :: P [Int]
pInts = do
  char '[' >> hspaces
  is <- sepBy1 pInt (char ',' >> hspaces)
  hspaces >> char ']' >> hspaces
  return is

pContentLine :: P String
pContentLine = do
  h <- try (newline >> noneOf ":")
  t <- many (noneOf "\n")
  return (h:t)

pContent :: P String
pContent = (concat . intersperse "\n") `fmap` many1 pContentLine

pHandle :: P Node
pHandle = do
  x <- pDouble
  y <- pDouble
  return $ Handle x y

pRectangle :: P Node
pRectangle = do
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  w <- pDouble
  h <- pDouble
  return $ Rectangle x y rgba w h

pText :: P Node
pText = do
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  sz <- pDouble
  n <- pInt
  hspaces
  c <- pContent
  return $ Text x y rgba sz n c

pLink :: P Node
pLink = do
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  f <- pInt
  v <- pString
  t <- pInt
  hs <- pInts
  lw <- pDouble
  return $ Link x y rgba f v t hs lw

pNode :: P Node
pNode = choice
  [ try (string ":handle") >> hspaces >> pHandle
  , try (string ":rectangle") >> hspaces >> pRectangle
  , try (string ":text") >> hspaces >> pText
  , try (string ":link") >> hspaces >> pLink
  ]

pNodes :: P [Node]
pNodes = many1 (pNode >>= \n -> spaces >> return n)

pFollow :: P (Int,Int)
pFollow = do
  string ":follow" >> hspaces
  a <- pInt
  b <- pInt
  spaces
  return (a,b)

pState :: P S
pState = do
  ns <- pNodes
  fs <- many pFollow
  return $ cleanState { nodes = ns, follow = fs }

unserialize :: String -> Either ParseError S
unserialize = parse pState "unserialize"

