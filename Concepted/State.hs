module Concepted.State where

import qualified Data.IntMap as IM

import Concepted.Plane

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
    -- ^ The mouse xy screen coordinates (i.e. w.r.t. the Gtk window).
  , snapTreshold :: Maybe Int
    -- ^ Specify a grid on which control points should snap.
  , hideLinks :: Bool
    -- ^ Specify if the links should be rendered or not.
  , planes :: [Plane]
    -- ^ The different planes of the application.
  }

currentPlane :: S -> Plane
currentPlane = head . planes

replaceCurrentPlane :: S -> Plane -> S
replaceCurrentPlane s p = s { planes = p : tail (planes s) }

cleanState :: S
cleanState = S
  { width = 320
  , height = 200
  , filename = Nothing
  , mouseXY = (0, 0)
  , snapTreshold = Just 10
  , hideLinks = False
  , planes = [Plane
    { panXY = (0, 0)
    , zoom = 1
    , concepts = IM.empty
    , links = IM.empty
    , selection = []
    , follow = []
    , widgets = []
    } ]
  }

