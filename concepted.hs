{-# Language RankNTypes #-}
{-# Language TupleSections #-}
{-# Language DeriveDataTypeable #-}
module Main where

import Paths_concepted (version)
import Data.Version (showVersion)

import Graphics.UI.Gtk hiding
  ( eventKeyName, eventButton, eventModifier
  , Menu, Point, Rectangle, Widget
  , add, get
  )
import Graphics.Rendering.Cairo hiding (
  status, versionString, version)

import System.Console.CmdArgs.Implicit hiding ((:=))

import Concepted.Core
import Concepted.Syntax.Parser
import Concepted.State

-- TODO: it would be nice to automatically reload a file when it
-- is externally modified.
-- TODO: the addFollow is recomputed at each myMotion (it could be
-- computed only when a selection occurs). Also chop is used at each
-- renderConcept.
-- TODO: use applicative for the parsers.
-- TODO: push the background and other style-related bits in the state.
-- TODO: when multiple items get selected (because there are stacked
-- at same place), the selection should cycle between them (one) and
-- all.
-- TODO: display the indice of each concept, link, handle.
-- TODO: after saving the state to file, check the file can be reload
-- succesfully.
-- TODO: there is a parser bug when reading a negative int.

----------------------------------------------------------------------
-- The main program
----------------------------------------------------------------------

main :: IO ()
main = (>>= processCmd) . cmdArgs $
  modes
    [ edit
    , generate
    ]
  &= summary versionString
  &= program "concepted"

versionString :: String
versionString =
  "concepted " ++ showVersion version ++
  " -- Copyright (c) 2010-2011 Vo Minh Thu."

data Cmd =
    Edit
    { editFile :: FilePath
    }
  | Generate
  deriving (Data, Typeable, Show, Eq)

edit :: Cmd
edit = Edit
  { editFile = def
    &= typFile
    &= argPos 0
    &= opt ""
  } &= help "Edit a concept map."

generate :: Cmd
generate = Generate
  &= help "Generate automatically some vector graphics."

processCmd :: Cmd -> IO ()
processCmd (Edit "") =
  main' $ replaceCurrentPlane cleanState $ (getCurrentPlane cleanState)
    { widgets =
      [ Label (10,20) "Concepted"
      , Button (140,20) "Pass" pass
      , Button (240,20) "Configuration" (liftIO mainQuit)
      ]
    }

processCmd (Edit fn) = do
  c <- readFile fn
  case unserialize c of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right a -> main' $ a
      { filename = Just fn
      , handlers = [HandlerState linkSplitter Nothing, HandlerState xxx ()]
      } `addPlane` emptyPlane
      { widgets =
        [ Label (10,20) "Concepted"
        , Button (140,20) "Pass" pass
        , Button (240,20) "Quit" (liftIO mainQuit)
        ]
      }

processCmd Generate = do
  putStrLn "Generating..."

