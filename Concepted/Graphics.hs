module Concepted.Graphics where

import Graphics.Rendering.Cairo
import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Concepted.Misc

-- Cairo function for Points.

rectangleXY :: Point -> Double -> Double -> Render ()
rectangleXY = uncurry rectangle

moveToXY :: Point -> Render ()
moveToXY = uncurry moveTo

----------------------------------------------------------------------
-- Data
----------------------------------------------------------------------

type RGBA = (Double,Double,Double,Double)

black :: RGBA
black = (0,0,0,1)

white :: RGBA
white = (1,1,1,1)

red :: RGBA
red = (1,0,0,1)

grey :: RGBA
grey = (0.6,0.6,0.6,1)

lightGrey :: RGBA
lightGrey = (0.9,0.9,0.9,1)

cyan :: RGBA
cyan = (0,1,1,1)

magenta :: RGBA
magenta = (1,0,1,1)

orange :: RGBA
orange = (1,0.7,0,1)

type Point = (Double, Double)

add :: Point -> (Double, Double) -> Point
add (a, b) (x, y) = (a + x, b + y)

sub :: Point -> (Double, Double) -> Point
sub (a, b) (x, y) = (a - x, b - y)

muls :: (Double, Double) -> Double -> Point
muls (a, b) s = (a * s, b * s)

divs :: (Double, Double) -> Double -> Point
divs (a, b) s = (a / s, b / s)

-- Identify an object in a selection.
data Id =
    IdConcept Int
  | IdLink Int
  | IdLinkHandle Int Int -- The Link id, then the control-point id.
  deriving (Eq, Show)

-- x y (the center, not the upper-left corner)
newtype Handle = Handle Point
  deriving Show

data Concept =
  -- x y rgba width height
    Rectangle Point RGBA Double Double
  -- x y rgba text-size max-line-length content
  | Text Point RGBA Double Int String
  deriving Show

-- x y rgba from verb to control-points line-width
data Link = Link Point RGBA Int String Int [Handle] Double
  deriving Show

handles :: Link -> [Handle]
handles (Link _ _ _ _ _ hs _) = hs

positionConcept :: Concept -> Point
positionConcept (Text xy _ _ _ _) = xy
positionConcept (Rectangle xy _ _ _) = xy

setPositionConcept :: Point -> Concept -> Concept
setPositionConcept xy (Text _ rgba sz n ss) = Text xy rgba sz n ss
setPositionConcept xy (Rectangle _ rgba w h) = Rectangle xy rgba w h

positionLink :: Link -> Point
positionLink (Link xy _ _ _ _ _ _) = xy

setPositionLink :: Point -> Link -> Link
setPositionLink xy (Link _ rgba f v t ps lw) = Link xy rgba f v t ps lw

positionHandle :: Handle -> Point
positionHandle (Handle xy) = xy

setPositionHandle :: Point -> Handle -> Handle
setPositionHandle xy (Handle _) = Handle xy

renderHandle :: Bool -> Handle -> Render ()
renderHandle selected (Handle xy) = do
  setLineWidth 0.6
  setSourceRGBA' lightGrey
  rectangleXY (xy `sub` (10, 10)) 20 20
  if selected then fill else stroke

renderConcept :: Bool -> Concept -> Render ()
renderConcept selected (Rectangle (x, y) rgba w h) = do
  setSourceRGBA' rgba
  rectangle x y w h
  fill
  when selected $ do
    setSourceRGBA 0 0 0 1
    rectangle x y w h
    stroke

renderConcept selected (Text (x, y) rgba sz n ss) = do
  -- the square handle
  render selected (Handle (x-12, y-12-sz)) -- hardcoded in pickText
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

renderLink :: Bool -> Link -> Render ()
renderLink selected (Link xy rgba _ v _ ps lw) = do
  -- the square handle
  render selected (Handle xy)
  -- the verb
  setFontSize 10
  moveToXY xy
  setSourceRGBA' rgba
  showText v
  -- the line
  setLineJoin LineJoinRound
  setLineWidth lw
  setSourceRGBA' rgba
  mapM_ (uncurry lineTo) (map position ps)
  stroke
  -- the arrow head
  let (xc,yc) = position (last ps)
  arc xc yc 3.0 0 (2*pi)
  fill

pickConcept :: Point -> Concept -> Bool
pickConcept ab (Rectangle xy _ w h) =
  containXYWH ab xy w h
pickConcept ab (Text xy _ sz _ _) =
  containXYWH ab (xy `add` (-22, -sz-22)) 20 20 -- hardcoded in render Text

pickLink :: Point -> Link -> Bool
pickLink ab (Link xy _ _ _ _ _ _) =
  containXYWH ab (xy `sub` (10, 10)) 20 20

selectLink :: Point -> Link -> [Int]
selectLink ab (Link _ _ _ _ _ ps _) =
  select id ab $ zip [0..] ps

pickHandle :: Point -> Handle -> Bool
pickHandle ab (Handle xy) =
  containXYWH ab (xy `sub` (10, 10)) 20 20

moveConcept :: (Double, Double) -> Concept -> Concept
moveConcept dxy (Rectangle xy rgba w h) =
  Rectangle (xy `add` dxy) rgba w h

moveConcept dxy (Text xy rgba sz n ss) =
  Text (xy `add` dxy) rgba sz n ss

moveLink :: (Double, Double) -> Link -> Link
moveLink dxy (Link xy rgba f v t ps lw) =
  Link (xy `add` dxy) rgba f v t ps lw

moveHandle :: (Double, Double) -> Handle -> Handle
moveHandle dxy (Handle xy) = Handle $ dxy `add` xy

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
containXYWH :: Point -> Point -> Double -> Double -> Bool
containXYWH (a, b) (x, y) w h =
  a >= x && b >= y && a <= x + w && b <= y + h

-- Filter the selected nodes.
select :: Pickable a => (Int -> b) -> Point -> [(Int,a)] -> [b]
select f xy = map (f . fst) . filter (pick xy . snd)

selectLinksHandles :: Point -> [(Int, Link)] -> [Id]
selectLinksHandles xy ls =
  concatMap (\(i,l) -> map (IdLinkHandle i) $ selectLink xy l) ls

----------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------

class Renderable a where
  render :: Bool -> a -> Render ()

class Pickable a where
  pick :: Point -> a -> Bool

class Moveable a where
  position :: a -> Point
  setPosition :: Point -> a -> a
  move :: (Double, Double) -> a -> a

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
