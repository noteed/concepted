{-# Language TupleSections #-}
module Concepted.Widget where

import Data.Maybe
import Data.List
import Control.Concurrent
import Control.Monad
import Graphics.Rendering.Cairo

import Concepted.Graphics
import Concepted.Misc

type Command = String

--data Mouse = Mouse | MousePressed Int
type Mouse = Maybe Int

data Menu = Menu [Widget] (MVar [(PShape,Int)]) (MVar Mouse)

newMenu :: [Widget] -> IO Menu
newMenu widgets = do
  svar <- newMVar []
  mvar <- newMVar Nothing
  return $ Menu widgets svar mvar

-- The pickable shape of a widget.
-- The computation of the size of a cairo text is done in the Render monad.
-- It is done during the rendering and kept for later (pure) reuse.
data PShape = PRectangle Point Double Double

pressMenu :: Point -> Menu -> IO ()
pressMenu pos (Menu _ svar mvar) = do
  shapes <- readMVar svar
  _ <- takeMVar mvar
  putMVar mvar $ pickMenu pos shapes

releaseMenu :: Point -> Menu -> IO (Maybe Int)
releaseMenu pos (Menu _ svar mvar) = do
  shapes <- readMVar svar
  v <- takeMVar mvar
  putMVar mvar Nothing
  case (pickMenu pos shapes, v) of
    (Just i, Just j) | i == j -> return $ Just i
    _ -> return Nothing

pickMenu :: Point -> [(PShape,Int)] -> Maybe Int
pickMenu pos = fmap snd . find (pickPShape pos . fst)

pickPShape :: Point -> PShape -> Bool
pickPShape ab (PRectangle xy w h) = containXYWH ab xy w h

renderMenu :: Point -> Menu -> Render ()
renderMenu pos (Menu widgets svar _) = do
  _ <- liftIO $ takeMVar svar
  pshapes <- renderMenu' pos widgets
  liftIO $ putMVar svar pshapes

renderMenu' :: Point -> [Widget] -> Render [(PShape,Int)]
renderMenu' mouse menu = do
  ms <- zipWithM f menu [0..]
  return $ mapMaybe id ms
  where f w i = do
          s <- renderWidget mouse w
          return $ fmap (,i) s

data Widget =
    Label Point String
  | Button Point String Command
  deriving Show

sizeWidget :: Widget -> Render Double
sizeWidget (Label _ ss) = textWidth 18 20 ss
sizeWidget (Button _ ss _) = textWidth 18 20 ss

containWidget' :: Double -> (Double, Double) -> Widget -> Bool
containWidget' width mxy (Label xy _) =
  containXYWH mxy (xy `sub` (10, 16)) (width+21) 22
containWidget' width mxy (Button xy _ _) =
  containXYWH mxy (xy `sub` (10, 16)) (width+21) 22

renderWidget :: Point -> Widget -> Render (Maybe PShape)
renderWidget _ (Label xy ss) = do
  renderConcept False (Text xy black 18 20 ss)
  return Nothing

renderWidget mxy w@(Button xy ss _) = do
  renderConcept False (Text xy black 18 20 ss)

  width <- sizeWidget w
  when (containWidget' width mxy w) $ do
    setLineWidth 2
    setSourceRGBA' black
    rectangleXY (xy `sub` (10, 16)) (width+21) 22
    stroke
  return . Just $ PRectangle (xy `sub` (10, 16)) (width+21) 22

textWidth :: Double -> Int -> String -> Render Double
textWidth sz n ss = do
  setFontSize sz
  let txts = chop n ss
      f s = textExtentsWidth `fmap` textExtents s
  widths <- mapM f txts
  return $ maximum widths

