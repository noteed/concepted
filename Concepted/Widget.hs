{-# Language TupleSections #-}
module Concepted.Widget where

import Data.Maybe
import Data.List
import Control.Concurrent
import Control.Monad
import Graphics.Rendering.Cairo

import Concepted.Graphics
import Concepted.Misc
import Concepted.State

newMenu :: [Widget] -> IO Menu
newMenu ws = do
  svar <- newMVar []
  mvar <- newMVar Nothing
  return $ Menu ws svar mvar

pressMenu :: Point -> Menu -> IO ()
pressMenu pos (Menu _ svar mvar) = do
  shapes <- readMVar svar
  _ <- takeMVar mvar
  putMVar mvar $ pickMenu pos shapes

releaseMenu :: Point -> Menu -> IO (Maybe (C ()))
releaseMenu pos (Menu ws svar mvar) = do
  shapes <- readMVar svar
  v <- takeMVar mvar
  putMVar mvar Nothing
  case (pickMenu pos shapes, v) of
    (Just i, Just j) | i == j -> return $ widgetCommand (ws !! i)
    _ -> return Nothing

pickMenu :: Point -> [(PShape,Int)] -> Maybe Int
pickMenu pos = fmap snd . find (pickPShape pos . fst)

pickPShape :: Point -> PShape -> Bool
pickPShape ab (PRectangle xy w h) = containXYWH ab xy w h

renderMenu :: Point -> Menu -> Render ()
renderMenu pos (Menu ws svar _) = do
  _ <- liftIO $ takeMVar svar
  pshapes <- renderMenu' pos ws
  liftIO $ putMVar svar pshapes

renderMenu' :: Point -> [Widget] -> Render [(PShape,Int)]
renderMenu' mouse menu = do
  ms <- zipWithM f menu [0..]
  return $ mapMaybe id ms
  where f w i = do
          s <- renderWidget mouse w
          return $ fmap (,i) s

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

