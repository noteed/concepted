module Concepted.State where

import Concepted.Graphics
import Concepted.Widget

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
  , widgets :: [Widget]
  }

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
  , widgets = []
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
  , widgets = []
  }
