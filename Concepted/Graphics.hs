module Concepted.Graphics where

import Graphics.Rendering.Cairo
import Control.Monad

import Concepted.Misc

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
