module Concepted.State where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Concepted.Graphics
import Concepted.Widget

----------------------------------------------------------------------
-- The main state of the program
----------------------------------------------------------------------

data S = S
  { width :: Double
    -- ^ Gtk window width.
  , height :: Double
    -- ^ Gtk window height.
  , filename :: Maybe String
    -- ^ The edited file, if any.
  , mouseXY :: (Double, Double)
    -- ^ The mouse xy screen coordinates (i.e. w.r.t. the Gtk window)
  , panXY :: (Double, Double)
  , zoom :: Double
  , snapTreshold :: Maybe Int
  , hideLinks :: Bool
  , concepts :: IntMap Concept
  , links :: IntMap Link
  , selection :: [Id]
  -- if (a,b) is in follow then whenever a is moved, b is moved too
  , follow :: [(Id,Id)]
  , widgets :: [Widget]
  }

isSelectedConcept :: Int -> S -> Bool
isSelectedConcept a s = IdConcept a `elem` selection s

isSelectedLink :: Int -> S -> Bool
isSelectedLink a s = IdLink a `elem` selection s

cleanState :: S
cleanState = S
  { width = 320
  , height = 200
  , filename = Nothing
  , mouseXY = (0, 0)
  , panXY = (0, 0)
  , zoom = 1
  , snapTreshold = Just 10
  , hideLinks = False
  , concepts = IM.empty
  , links = IM.empty
  , selection = []
  , follow = []
  , widgets = []
  }

