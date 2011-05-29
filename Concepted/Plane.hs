module Concepted.Plane where

import Data.IntMap (IntMap)

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

