{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Concepted.State where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M

import Control.Concurrent (MVar)

import Control.Monad.Reader
import Control.Monad.State

import Concepted.Graphics

----------------------------------------------------------------------
-- The main state of the program
----------------------------------------------------------------------

data CState = CState
  { wwidth :: Double
    -- ^ Gtk window width.
  , wheight :: Double
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
  , menus :: M.Map [Widget] Menu
    -- ^ Mapping between the (pure) menus description and the (stateful)
    -- menus. Each plane can have its own menu description.
  , handlers :: [HandlerState]
  , wstatus :: String
  }

cleanState :: CState
cleanState = CState
  { wwidth = 320
  , wheight = 200
  , filename = Nothing
  , mouseXY = (0, 0)
  , snapTreshold = Just 10
  , hideLinks = False
  , planes = [emptyPlane]
  , menus = M.empty
  , handlers = []
  , wstatus = "Uninitialized"
  }

currentPlane :: Cx Plane
currentPlane = (getCurrentPlane, replaceCurrentPlane)

getCurrentPlane :: CState -> Plane
getCurrentPlane = head . planes

replaceCurrentPlane :: CState -> Plane -> CState
replaceCurrentPlane s p = s { planes = p : tail (planes s) }

addPlane :: CState -> Plane -> CState
addPlane s p = s { planes = planes s ++ [p] }

planeMenuPairs :: CState -> [(Plane, Menu)]
planeMenuPairs s = zip (planes s) (M.elems $ menus s)

----------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------

data CConf = CConf
  { confBackground :: RGBA
  }

cleanConf :: CConf
cleanConf = CConf
  { confBackground = white
  }

----------------------------------------------------------------------
-- The main container
----------------------------------------------------------------------

-- | A plane is similar to a layer as found in other graphical application,
-- but each plane can be panned/zoomed individually.
data Plane = Plane
  { panXY :: (Double, Double)
  -- ^ How much is the plane translated w.r.t. to the screen.
  , zoom :: Double
  -- ^ How much is the plane zoomed w.r.t. to the screen.
  , concepts :: IntMap Concept
  -- ^ All the Concepts of the plane.
  , links :: IntMap Link
  -- ^ All the Links of the plane.
  , selection :: [Id]
  -- ^ List of selected objects (concepts and/or links).
  , follow :: [(Id,Id)]
  -- ^ If (a,b) is in follow then whenever a is moved, b is moved too.
  , widgets :: [Widget]
  , pLines :: IntMap Line
  }

emptyPlane :: Plane
emptyPlane = Plane
  { panXY = (0, 0)
  , zoom = 1
  , concepts = IM.empty
  , links = IM.empty
  , selection = []
  , follow = []
  , widgets = []
  , pLines = IM.empty
  }

----------------------------------------------------------------------
-- The C monad, inspired by the X monad.
----------------------------------------------------------------------

newtype C a = C (ReaderT CConf (StateT CState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState CState, MonadReader CConf)

runC :: CConf -> CState -> C a -> IO (a, CState)
runC c st (C a) = runStateT (runReaderT a c) st

execC :: CConf -> CState -> C a -> IO CState
execC c st (C a) = execStateT (runReaderT a c) st

-- Getter/Setter pair specialized on a CState.
-- ('Cx' as in CState exchange.) 
type Cx a = GN CState a

-- Getter/Setter pair, called Grabber/Nailer.
-- 's' is supposed to be the state in some State monad.
type GN s a = (s -> a, s -> a -> s)

grab :: MonadState s m => GN s a -> m a
grab = gets . fst

nail :: MonadState s m => GN s a -> a -> m ()
nail (_, f) = modify . flip f

change :: MonadState s m => GN s a -> (a -> a) -> m ()
change gn f = grab gn >>= nail gn . f

status :: String -> C ()
status x = do
  s <- get
  put $ s { wstatus = x }

--data Mouse = Mouse | MousePressed Int
type Mouse = Maybe Int

data Menu = Menu [Widget] (MVar [(PShape, Int)]) (MVar Mouse)

data Widget =
    Label Point String
  | Button Point String (C ())

instance Ord Widget where
  compare (Label _ a) (Label _ b) = compare a b
  compare (Button _ a _) (Button _ b _) = compare a b
  compare (Label _ a) (Button _ b _) = compare a b
  compare (Button _ a _) (Label _ b) = compare a b

instance Eq Widget where
  (==) (Label _ a) (Label _ b) = a == b
  (==) (Button _ a _) (Button _ b _) = a == b
  (==) (Label _ a) (Button _ b _) = a == b
  (==) (Button _ a _) (Label _ b) = a == b

widgetCommand :: Widget -> Maybe (C ())
widgetCommand (Label _ _) = Nothing
widgetCommand (Button _ _ c) = Just c

-- The pickable shape of a widget.
-- The computation of the size of a cairo text is done in the Render monad.
-- It is done during the rendering and kept for later (pure) reuse.
data PShape = PRectangle Point Double Double

-- Key is used for mouse buttons too. The string can be "lmb", or "rmb".
data Event = Key String Bool -- True/False means Pressed/Released.
  deriving (Show)

-- Nothing means the event was not handled and should be passed on to the next
-- handler. Just True, means the event was handled and the handler should be
-- removed. Just False, means the same but the handler should be kept.
data HandlerState = forall a. HandlerState (Handler a) a

type Handler a = Event -> a -> C (Handling a)

data Handling a =
    Ignored -- pass the event to the next handler
  | End -- event is handled and the handler can be removed.
  | Continue a -- the event is handled and the handler is kept with a new state.
  | Install a HandlerState -- the event is handled, the handler is kept with a new state, and a new handler is added.
  -- TODO the code of the handler can add the new handler by itself since it has acces to the C monad.

-- Pass a single event to handlers. The returned boolean specifies if the
-- event was handled or not.
handle :: Event -> C Bool
handle e = do
  hs <- gets handlers
  (hs', b) <- handle' e [] hs
  s <- get
  put $ s { handlers = hs' }
  return b

-- The bool specifies if the event was handled or not.
handle' :: Event -> [HandlerState] -> [HandlerState] -> C ([HandlerState], Bool)
handle' _ acc [] = return (reverse acc, False)
handle' e acc (HandlerState h a:hs) = do
  r <- h e a
  case r of
    Ignored -> handle' e (HandlerState h a : acc) hs
    End -> return (reverse acc ++ hs, True)
    Continue b -> return (reverse acc ++ (HandlerState h b : hs), True)
    Install b h' -> return (h' : reverse acc ++ (HandlerState h b : hs), True)
