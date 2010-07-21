{-# Language RankNTypes #-}
module Main where

import System.Environment (getArgs)
import System.Directory (renameFile)

import Graphics.UI.Gtk hiding (
  eventKeyName, eventButton, eventModifier, Rectangle)
import Graphics.UI.Gtk.Gdk.Events (
  eventX, eventY, eventKeyName, eventButton, eventModifier)
import Graphics.Rendering.Cairo

import Control.Concurrent
import Control.Monad

import Data.List
import Data.Maybe
import Text.PrettyPrint hiding (char, render)
import Text.ParserCombinators.Parsec hiding (setPosition)
import qualified Text.Pandoc as P

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
    ["-md", fn] -> do
      a <- loadMarkdown fn
      case a of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right a' -> main' a'
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
    [ windowTitle := "Cairo Attempt"
    , windowDefaultWidth := 320
    , windowDefaultHeight := 200
    , containerBorderWidth := 0
    ]
  canvas <- drawingAreaNew
  containerAdd window canvas
  widgetShowAll window 

  sVar <- newMVar initialState

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
        s' <- myLmbPress (Control `elem` eventModifier e) (eventX e) (eventY e) s
        putMVar sVar s'
        widgetQueueDraw canvas
      _ -> return ()
    return True

  onButtonRelease canvas $ \e -> do
    case eventButton e of
      LeftButton -> do
        s <- takeMVar sVar
        s' <- myLmbRelease (eventX e) (eventY e) s
        putMVar sVar s'
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
  , mouseX :: Double
  , mouseY :: Double
  , panX :: Double
  , panY :: Double
  , zoom :: Double
  , snapTreshold :: Maybe Int
  , hideLinks :: Bool
  , concepts :: [Concept]
  , links :: [Link]
  , handles :: [Handle]
  , selection :: [Id]
  -- if (a,b) is in follow then whenever a is moved, b is moved too
  , follow :: [(Id,Id)]
  }
  deriving (Show)

cleanState :: S
cleanState = S
  { width = 320
  , height = 200
  , filename = Nothing
  , mouseX = 0
  , mouseY = 0
  , panX = 0
  , panY = 0
  , zoom = 1
  , snapTreshold = Just 10
  , hideLinks = False
  , concepts = []
  , links = []
  , handles = []
  , selection = []
  , follow = []
  }

-- an S for testing purpose
myState :: S
myState = S
  { width = 320.0
  , height = 200.0
  , filename = Nothing
  , mouseX = 0
  , mouseY = 0
  , panX = 0.0
  , panY = 0.0
  , zoom = 1
  , snapTreshold = Just 10
  , hideLinks = False
  , concepts =
  [ Rectangle 0.0 0.0 (1.0,0.7,0.0,1.0) 100.0 40.0
    , Text 100.0 100.0 (0.0,0.0,0.0,1.0) 20.0 25 "Concepted"
    , Text 100.0 120.0 (0.0,0.0,0.0,1.0) 12.0 25 "Concept mapping tool"
    , Text 200.0 260.0 (0.0,0.0,0.0,1.0) 20.0 25 "Lorem ipsum dolor"
    , Text 200.0 300.0 (0.0,0.0,0.0,1.0) 12.0 25 "Lorem ipsum dolor\nsit\namet,\nconsectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat."
    ]
  , handles =
    [ Handle 10.0 10.0
    , Handle 120.0 50.0
    , Handle 180.0 30.0
    , Handle 300.0 400.0
    ]
  , links = [ Link 50.0 50.0 (0.0,1.0,1.0,1.0) 1 "is way too" 3 [0,1,2,3] 2.0 ]
  , selection = []
  , follow = [(IdConcept 0, IdConcept 1)]
  }

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
            , handles = handles s'
            , follow = follow s'
            }
  "s" -> case filename s of
    Nothing -> return Nothing
    Just fn -> do
    renameFile fn (fn ++ ".bak")
    writeFile fn (serialize s)
    putStrLn $ fn ++ " saved"
    return Nothing
  "plus" -> return . Just $ zoomAt (mouseX s) (mouseY s) 1.1 s
  "minus" -> return . Just $ zoomAt (mouseX s) (mouseY s) (1 / 1.1) s
  "l" -> return . Just $ s { hideLinks = not (hideLinks s) }
  _ -> return Nothing

myLmbPress :: Bool -> Double -> Double -> S -> IO S
myLmbPress ctrl x y s = do
  let (x',y') = screenToScene s (x,y)
      selc = select IdConcept x' y' (concepts s)
      selh = select IdHandle x' y' (handles s)
      sell = select IdLink x' y' (links s)
      sel = take 1 $ concat [selc,selh,sell]
  return $ s { selection = if ctrl
    then nub (sel ++ selection s)
    else if null sel then selection s else sel}

myLmbRelease :: Double -> Double -> S -> IO S
myLmbRelease _ _ s = do
  case selection s of
    [] -> return s
    _ -> case snapTreshold s of
      Nothing -> return s
      Just t -> return $ snapSelection t s

-- The bools specifies if the lmb and rmb are pressed.
myMotion :: Bool -> Bool -> Double -> Double -> S -> IO S
myMotion True False dx dy s = do
  let (dx',dy') = screenToSceneDelta s (dx,dy)
  return $ mapSelection (move dx' dy') s
myMotion False True dx dy s = return $ pan dx dy s
myMotion _ _ _ _ s = return s

myDraw :: S -> Render ()
myDraw s = do
  -- clear
  setSourceRGBA' background
  paint

  -- view the scene under the pan/zoom transform
  translate (panX s) (panY s)
  scale (zoom s) (zoom s)

  -- render
  mapM_ (\(a,b) -> render (IdConcept a `elem` selection s) b)
    (zip [0..] (concepts s))
  unless (hideLinks s) $
    mapM_ (\(a,b) -> renderLink (handles s) (IdLink a `elem` selection s) b)
      (zip [0..] (links s))
  mapM_ (\(a,b) -> render (IdHandle a `elem` selection s) b)
    (zip [0..] (handles s))

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

data Id =
    IdHandle Int
  | IdConcept Int
  | IdLink Int
  deriving (Eq, Show)

-- x y (the center, not the upper-left corner)
data Handle = Handle Double Double
  deriving Show

data Concept =
  -- x y rgba width height
    Rectangle Double Double RGBA Double Double
  -- x y rgba text-size max-line-length content
  | Text Double Double RGBA Double Int String
  deriving Show

-- x y from verb to control-points (probably handles) rgba line-width
data Link = Link Double Double RGBA Int String Int [Int] Double
  deriving Show

positionConcept :: Concept -> (Double,Double)
positionConcept (Text x y _ _ _ _) = (x,y)
positionConcept (Rectangle x y _ _ _) = (x,y)

setPositionConcept :: Double -> Double -> Concept -> Concept
setPositionConcept x y (Text _ _ rgba sz n ss) = Text x y rgba sz n ss
setPositionConcept x y (Rectangle _ _ rgba w h) = Rectangle x y rgba w h

positionLink :: Link -> (Double,Double)
positionLink (Link x y _ _ _ _ _ _) = (x,y)

setPositionLink :: Double -> Double -> Link -> Link
setPositionLink x y (Link _ _ rgba f v t ps lw) = Link x y rgba f v t ps lw

positionHandle :: Handle -> (Double,Double)
positionHandle (Handle x y) = (x,y)

setPositionHandle :: Double -> Double -> Handle -> Handle
setPositionHandle x y (Handle _ _) = Handle x y

renderHandle :: Bool -> Handle -> Render ()
renderHandle selected (Handle x y) = do
  setLineWidth 0.6
  setSourceRGBA' lightGrey
  rectangle (x-10) (y-10) 20 20
  if selected then fill else stroke

renderConcept :: Bool -> Concept -> Render ()
renderConcept selected (Rectangle x y rgba w h) = do
  setSourceRGBA' rgba
  rectangle x y w h
  fill
  when selected $ do
    setSourceRGBA 0 0 0 1
    rectangle x y w h
    stroke

renderConcept selected (Text x y rgba sz n ss) = do
  -- the square handle
  render selected (Handle (x-12) (y-12-sz)) -- hardcoded in pickText
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

renderLink :: [Handle] -> Bool -> Link -> Render ()
renderLink hs selected (Link x y rgba _ v _ ps lw) = do
  -- the square handle
  render selected (Handle x y)
  -- the verb
  setFontSize 10
  moveTo x y
  setSourceRGBA' rgba
  showText v
  -- the line
  setLineJoin LineJoinRound
  setLineWidth lw
  setSourceRGBA' rgba
  mapM_ (uncurry lineTo) (map (position . (hs !!)) ps)
  stroke
  -- the arrow head
  let (xc,yc) = position (hs !! last ps)
  arc xc yc 3.0 0 (2*pi)
  fill

pickConcept :: Double -> Double -> Concept -> Bool
pickConcept a b (Rectangle x y _ w h) =
  containXYWH a b x y w h
pickConcept a b (Text x y _ sz _ _) =
  containXYWH a b (x-22) (y-sz-22) 20 20 -- hardcoded in render Text

pickLink :: Double -> Double -> Link -> Bool
pickLink a b (Link x y _ _ _ _ _ _) =
  containXYWH a b (x-10) (y-10) 20 20

pickHandle :: Double -> Double -> Handle -> Bool
pickHandle a b (Handle x y) =
  containXYWH a b (x-10) (y-10) 20 20

moveConcept :: Double -> Double -> Concept -> Concept
moveConcept dx dy (Rectangle x y rgba w h) =
  Rectangle (x + dx) (y + dy) rgba w h

moveConcept dx dy (Text x y rgba sz n ss) =
  Text (x + dx) (y + dy) rgba sz n ss

moveLink :: Double -> Double -> Link -> Link
moveLink dx dy (Link x y rgba f v t ps lw) =
  Link (x + dx) (y + dy) rgba f v t ps lw

moveHandle :: Double -> Double -> Handle -> Handle
moveHandle dx dy (Handle x y) =
  Handle (x + dx) (y + dy)

----------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------

class Renderable a where
  render :: Bool -> a -> Render ()

class Pickable a where
  pick :: Double -> Double -> a -> Bool

class Moveable a where
  position :: a -> (Double,Double)
  setPosition :: Double -> Double -> a -> a
  move :: Double -> Double -> a -> a

instance Renderable Concept where
  render = renderConcept

instance Renderable Handle where
  render = renderHandle

instance Pickable Concept where
  pick = pickConcept

instance Pickable Link where
  pick = pickLink

instance Pickable Handle where
  pick = pickHandle

instance Moveable Concept where
  position = positionConcept
  setPosition = setPositionConcept
  move = moveConcept

instance Moveable Link where
  position = positionLink
  setPosition = setPositionLink
  move = moveLink

instance Moveable Handle where
  position = positionHandle
  setPosition = setPositionHandle
  move = moveHandle

----------------------------------------------------------------------
-- Process the selection
----------------------------------------------------------------------

-- Filter the selected nodes.
select :: Pickable a => (Int -> b) -> Double -> Double -> [a] -> [b]
select f x y = map (f . fst) . filter (pick x y . snd) . zip [0..]

mapSelection :: (forall a . Moveable a => a -> a) -> S -> S
mapSelection f s = s
  { concepts = map fc cs
  , links = map fl ls
  , handles = map fh hs
  }
  where
  cs = zip [0..] (concepts s)
  ls = zip [0..] (links s)
  hs = zip [0..] (handles s)
  sel = selection s
  fol = follow s
  fc (b,n) = if IdConcept b `elem` (sel `addFollow` fol) then f n else n
  fl (b,n) = if IdLink b    `elem` (sel `addFollow` fol) then f n else n
  fh (b,n) = if IdHandle b  `elem` (sel `addFollow` fol) then f n else n

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

-- Set n to its nearest multiple of t.
snap :: Int -> Double -> Double
snap t n = fromIntegral $
  let n' = floor n
      m =  n' `mod` t
  in if m > 5 then n' + t - m else n' - m

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
ppS s = (vcat . map ppConcept . concepts) s
  $+$ (vcat . map ppLink . links) s
  $+$ (vcat . map ppHandle . handles) s
  $+$ (vcat . map ppFollow . follow) s

ppConcept :: Concept -> Doc
ppConcept n = case n of
  Rectangle x y rgba w h ->
    text ":rectangle" <+> double x <+> double y <+> tshow rgba
    <+> double w <+> double h
  Text x y rgba sz w c ->
    text ":text" <+> double x <+> double y <+> tshow rgba
    <+> double sz <+> int w $+$ text c

ppHandle :: Handle -> Doc
ppHandle (Handle x y) =
  text ":handle" <+> double x <+> double y

ppLink :: Link -> Doc
ppLink (Link x y rgba f v t hs lw) =
  text ":link" <+> double x <+> double y <+> tshow rgba
  <+> int f <+> tshow v <+> int t <+> tshow hs <+> double lw

ppFollow :: (Id,Id) -> Doc
ppFollow (a,b) = text ":follow" <+> ppId a <+> ppId b

ppId :: Id -> Doc
ppId (IdConcept i) = text "(concept" <+> int i <> text ")"
ppId (IdLink i) = text "(link" <+> int i <> text ")"
ppId (IdHandle i) = text "(handle" <+> int i <> text ")"

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

pId :: P Id
pId = choice . map try $
  [ string "(concept" >> hspaces >> pInt >>= \i -> hspaces >> string ")" >> hspaces >> return (IdConcept i)
  , string "(link" >> hspaces >> pInt >>= \i -> hspaces >> string ")" >> hspaces >> return (IdLink i)
  , string "(handle" >> hspaces >> pInt >>= \i -> hspaces >> string ")" >> hspaces >> return (IdHandle i)
  ]

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

pHandle :: P Handle
pHandle = do
  try (string ":handle") >> hspaces
  x <- pDouble
  y <- pDouble
  spaces
  return $ Handle x y

pRectangle :: P Concept
pRectangle = do
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  w <- pDouble
  h <- pDouble
  return $ Rectangle x y rgba w h

pText :: P Concept
pText = do
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  sz <- pDouble
  n <- pInt
  hspaces
  c <- pContent
  return $ Text x y rgba sz n c

pLink :: P Link
pLink = do
  try (string ":link") >> hspaces
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  f <- pInt
  v <- pString
  t <- pInt
  hs <- pInts
  lw <- pDouble
  spaces
  return $ Link x y rgba f v t hs lw

pConcept :: P Concept
pConcept = choice
  [ try (string ":rectangle") >> hspaces >> pRectangle
  , try (string ":text") >> hspaces >> pText
  ]

pConcepts :: P [Concept]
pConcepts = many1 (pConcept >>= \n -> spaces >> return n)

pLinks :: P [Link]
pLinks = many pLink

pHandles :: P [Handle]
pHandles = many pHandle

pFollow :: P (Id,Id)
pFollow = do
  string ":follow" >> hspaces
  a <- pId
  b <- pId
  spaces
  return (a,b)

pState :: P S
pState = do
  cs <- pConcepts
  ls <- pLinks
  hs <- pHandles
  fs <- many pFollow
  return $ cleanState
    { concepts = cs
    , links = ls
    , handles = hs
    , follow = fs
    }

unserialize :: String -> Either ParseError S
unserialize = parse pState "unserialize"

----------------------------------------------------------------------
-- Pandoc support
----------------------------------------------------------------------

noMeta :: P.Meta
noMeta = P.Meta [] [] []

blocks :: P.Pandoc -> [P.Block]
blocks (P.Pandoc _ bs) = bs

readDoc :: String -> P.Pandoc
readDoc = P.readMarkdown P.defaultParserState

readDoc' :: FilePath -> IO P.Pandoc
readDoc' fn = readDoc `fmap` readFile fn

inline :: [P.Inline] -> String
inline = concat . map f
  where f (P.Str s) = s
        f P.Space = " "
        f x = error $ "extractTitle: unhandled title data structure: " ++ show x

extractConcept :: P.Block -> Maybe Concept
extractConcept (P.Header 1 is) = Just $ Text 0 0 black 20 14 $ inline is
extractConcept (P.Para is) = Just $ Text 0 0 black 10 25 $ inline is
extractConcept _ = Nothing

extractLinks :: [P.Block] -> Either ParseError [(String,String,String)]
extractLinks bs = readVerbs $ cs
  where cs = concat $ mapMaybe extractLinksCode bs

linkConcepts :: (String,String,String) -> [Concept]
linkConcepts (f,_,t) = [g f,g t]
  where g x = if length (words x) < 5
          then Text 0 0 black 20 14 x
          else Text 0 0 black 10 25 x

-- The links are stored in a code block like
-- ~~~{.links}
-- a -- verb -> c
-- ~~~
extractLinksCode :: P.Block -> Maybe String
extractLinksCode (P.CodeBlock (_,["links"],[]) code) = Just $ code
extractLinksCode _ = Nothing

readVerbs :: String -> Either ParseError [(String,String,String)]
readVerbs = parse (many pVerb) "readVerb"

pVerb :: P (String,String,String)
pVerb = do
  a <- many1 pWord
  string "--"
  hspaces
  b <- many1 pWord
  string "->"
  hspaces
  c <- many1 pWord
  spaces
  return (unwords a,unwords b,unwords c)

pWord :: P String
pWord = do
  cs <- many1 $ noneOf "- \n" <|>
    try (string "-" >> notFollowedBy (oneOf "->") >> return '-')
  hspaces
  return cs

findConcepts :: String -> [Concept] -> [(Int,Concept)]
findConcepts s = mapMaybe f . zip [0..]
  where f (i,c@(Text _ _ _ _ _ ss)) | s `isPrefixOf` ss = Just (i,c)
        f _ = Nothing

findConcept :: String -> [Concept] -> Maybe (Int,Concept)
findConcept s cs = case sortBy f $ findConcepts s cs of
  [] -> Nothing
  (c:_) -> Just c
  where f (_,Text _ _ _ _ _ s1) (_,Text _ _ _ _ _ s2) =
          compare (length s1) (length s2)
        f _ _ = error "can't happen"

horizontal :: [Concept] -> [Concept]
horizontal cs = zipWith f [0,200..] cs
  where f x = setPosition x (100)

vertical :: [Concept] -> [(Int,(String,String,String))] -> [(Link,Handle)]
vertical cs ls = zipWith f [0,50..] ls'
  where f y (l,h') = (move 0 y l, move 0 y h')
        ls' = map g ls
        g (i,(a,b,c)) =
          let (j1,x1,y1) = h a
              (j2,x2,y2) = h c
          in (Link (x1 + 150) y1 cyan j1 b j2 [i] 2, Handle x2 y2)
        h a = case findConcept a cs of
          Nothing -> (-1, - 100, 50) -- TODO -1 should be Nothing
          Just (j,c) -> (j,fst $ position c, 150)

loadMarkdown :: FilePath -> IO (Either ParseError S)
loadMarkdown fn = do
  bs <- blocks `fmap` readDoc' fn 
  case extractLinks bs of
    Left err -> return $ Left err
    Right ls_ -> do
      let cs_ = nubBy f $ mapMaybe extractConcept bs ++ concatMap linkConcepts ls_
          f (Text _ _ _ _ _ s1) (Text _ _ _ _ _ s2) = s1 == s2
          f _ _ = error "can't happen"
          cs = horizontal cs_
          (ls,hs) = unzip $ vertical cs (zip [0..] ls_)
          fs = map g (zip [0..] ls) ++ map g' (zip [0..] ls)
          g (i,Link _ _ _ f' _ _ _ _) = (IdConcept f',IdLink i)
          g' (i,Link _ _ _ _ _ t _ _) = (IdConcept t,IdHandle i)
      return . Right $ cleanState { concepts = cs, links = ls, handles = hs, follow = fs }
