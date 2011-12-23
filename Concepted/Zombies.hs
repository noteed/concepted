{-# LANGUAGE RecordWildCards #-}

module Concepted.Zombies where

import Graphics.Rendering.Cairo

import Concepted.Graphics

stepsize :: Double
stepsize = 0.01 -- 100Hz.

data Game = Game
  { gZombies :: [Zombie]
  , gPlayer1 :: Player
  , gPlayer2 :: Player
  , gBullets :: [PlayerBullet]
  }

data Zombie = Zombie
  { zPosition :: Point -- ^ zombie position
  }

data Player = Player
  { pPosition :: Point -- ^ player position
  , pOrientation :: Double -- ^ player orientation
  , pInput :: PlayerInput
  , pMouse :: Point -- ^ mouse position
  }

-- Left, right, up, down
data PlayerInput = PlayerInput
  { inputLeft :: Bool
  , inputRight :: Bool
  , inputUp :: Bool
  , inputDown :: Bool
  }
  deriving Show

data PlayerBullet = PlayerBullet
  { bPosition :: Point
  , bVelocity :: (Double, Double)
  }

setLeft b p = p { pInput = (pInput p) { inputLeft = b } }
setRight b p = p { pInput = (pInput p) { inputRight = b } }
setUp b p = p { pInput = (pInput p) { inputUp = b } }
setDown b p = p { pInput = (pInput p) { inputDown = b } }
setMouse xy p = p { pMouse = xy }
setInput x p = p { pInput = x }

setOrientationFromMouse p = p { pOrientation = r' }
  where (x, y) = pMouse p `sub` pPosition p
        r = atan2 y x
        r' = if r < 0 then r + 2 * pi else r

goUp p = p { pPosition = pPosition p `add` (0, -1) }
goDown p = p { pPosition = pPosition p `add` (0, 1) }
goLeft p = p { pPosition = pPosition p `add` (-1, 0) }
goRight p = p { pPosition = pPosition p `add` (1, 0) }

update p =
  setOrientationFromMouse .
  f inputDown goDown .
  f inputRight goRight .
  f inputLeft goLeft .
  f inputUp goUp $ p
  where Player{..} = p
        PlayerInput{..} = pInput
        f cond g q = if cond then g q else q

updateN 0 p = p
updateN n p = updateN (n - 1) $ update p

updateBullet b = b { bPosition = bPosition b `add` bVelocity b }

updateBulletN 0 b = b
updateBulletN n b = updateBulletN (n - 1) $ updateBullet b

isDead bs z = any (isTouched z) bs

isTouched z b = norm (zPosition z `sub` bPosition b) < 20

renderZombie :: Zombie -> Render ()
renderZombie (Zombie (x,y)) = do
  setSourceRGBA' grey
  arc x y 20.0 0 (2*pi)
  fill

renderPlayer :: Player -> Render ()
renderPlayer p = do
  save
  let (x, y) = pPosition p
      r = pOrientation p
  translate x y
  rotate r

  setSourceRGBA' cyan
  arc 0 0 20.0 0 (2*pi)
  fill

  setSourceRGBA' blue
  moveTo 0 0
  lineTo 25 0
  stroke

  restore

renderBullet :: PlayerBullet -> Render ()
renderBullet (PlayerBullet (x,y) _) = do
  setSourceRGBA' cyan
  arc x y 3.0 0 (2*pi)
  fill
