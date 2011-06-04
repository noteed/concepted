-- | This module defines some operations on it on the Plane data type.
module Concepted.Plane where

import qualified Data.IntMap as IM

import Concepted.Graphics
import Concepted.State

isSelectedConcept :: Int -> Plane -> Bool
isSelectedConcept a s = IdConcept a `elem` selection s

isSelectedLink :: Int -> Plane -> Bool
isSelectedLink a s = IdLink a `elem` selection s

-- | Transform from screen coordinate to plane coordinate.
screenToPlane :: Plane -> Point -> Point
screenToPlane s xy = xy `sub` panXY s `divs` zoom s

-- | Transform from screen coordinate delta to plane coordinate delta.
screenToPlaneDelta :: Plane -> Point -> Point
screenToPlaneDelta s (dx,dy) = (dx / zoom s, dy / zoom s)

-- | Add dx and dy to the pan.
pan :: (Double, Double) -> Plane -> Plane
pan dxy s = s { panXY = panXY s `add` dxy }

-- | Multiply the zoom by a, modifying the panXY values
-- so that the scene-point under the screen coordinate (x,y)
-- remains at the same screen coordinate.
zoomAt :: Point -> Double -> Plane -> Plane
zoomAt xy a s =
  let xy1 = screenToPlane s xy
      s' = s { zoom = zoom s * a }
      xy2 = screenToPlane s' xy
  in s' { panXY = panXY s' `sub` (xy1 `sub` xy2 `muls` zoom s') }

-- | Add a new Concept to the plane at the desired location.
newConcept :: Point -> Plane -> Plane
newConcept xy s = s { concepts = IM.insert (IM.size $ concepts s) c $ concepts s }
  where c = Text xy black 20.0 14 ("concept #" ++ show (IM.size $ concepts s))
