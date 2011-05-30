module Concepted.Plane where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Concepted.Graphics
import Concepted.Widget

-- | A plane is similar to a layer as found in other graphical application,
-- but each plane can be panned/zoomed individually.
data Plane = Plane
  { panXY :: (Double, Double)
  , zoom :: Double
  , concepts :: IntMap Concept
  , links :: IntMap Link
  , selection :: [Id]
  -- ^ List of selected objects (concepts and/or links).
  , follow :: [(Id,Id)]
  -- ^ If (a,b) is in follow then whenever a is moved, b is moved too.
  , widgets :: [Widget]
  }

isSelectedConcept :: Int -> Plane -> Bool
isSelectedConcept a s = IdConcept a `elem` selection s

isSelectedLink :: Int -> Plane -> Bool
isSelectedLink a s = IdLink a `elem` selection s

-- Transform from screen coordinate to scene coordinate.
screenToScene :: Plane -> Point -> Point
screenToScene s xy = xy `sub` panXY s `divs` zoom s

-- Transform from screen coordinate delta to scene coordinate delta.
screenToSceneDelta :: Plane -> Point -> Point
screenToSceneDelta s (dx,dy) = (dx / zoom s, dy / zoom s)

-- Add dx and dy to the pan.
pan :: (Double, Double) -> Plane -> Plane
pan dxy s = s { panXY = panXY s `add` dxy }

-- Multiply the zoom by a, modifying the panXY values
-- so that the scene-point under the screen coordinate (x,y)
-- remains at the same screen coordinate.
zoomAt :: Point -> Double -> Plane -> Plane
zoomAt xy a s =
  let xy1 = screenToScene s xy
      s' = s { zoom = zoom s * a }
      xy2 = screenToScene s' xy
  in s' { panXY = panXY s' `sub` (xy1 `sub` xy2 `muls` zoom s') }

newConcept :: Point -> Plane -> Plane
newConcept xy s = s { concepts = IM.insert (IM.size $ concepts s) c $ concepts s }
  where c = Text xy black 20.0 14 ("concept #" ++ show (IM.size $ concepts s))
