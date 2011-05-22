module Concepted.Syntax.Parser where

import Data.List (intersperse)

import Text.ParserCombinators.Parsec hiding (setPosition)

import Concepted.Graphics
import Concepted.State

----------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------

type P a = GenParser Char () a

hspaces :: P ()
hspaces = many (oneOf " ") >> return ()

pDouble :: P Double
pDouble = do
  a <- many1 digit
  b <- option "0" (char '.' >> many1 digit)
  hspaces
  return . read $ a ++ "." ++ b

pInt :: P Int
pInt = do
  a <- many1 digit
  hspaces
  return . read $ a

pRGBA :: P RGBA
pRGBA = do
  char '('
  r <- pDouble
  char ','
  g <- pDouble
  char ','
  b <- pDouble
  char ','
  a <- pDouble
  char ')'
  hspaces
  return (r,g,b,a)

pId :: P Id
pId = choice . map try $
  [ string "(concept" >> hspaces >> pInt >>= \i -> hspaces >> string ")" >> hspaces >> return (IdConcept i)
  , string "(link" >> hspaces >> pInt >>= \i -> hspaces >> string ")" >> hspaces >> return (IdLink i)
  , string "(handle" >> hspaces >> pInt >>= \i -> hspaces >> string ")" >> hspaces >> return (IdHandle i)
  ]

pString :: P String
pString = do
  char '"'
  s <- many1 (noneOf "\"\n")
  char '"'
  hspaces
  return s

pInts :: P [Int]
pInts = do
  char '[' >> hspaces
  is <- sepBy1 pInt (char ',' >> hspaces)
  hspaces >> char ']' >> hspaces
  return is

pContentLine :: P String
pContentLine = do
  h <- try (newline >> noneOf ":")
  t <- many (noneOf "\n")
  return (h:t)

pContent :: P String
pContent = (concat . intersperse "\n") `fmap` many1 pContentLine

pHandle :: P Handle
pHandle = do
  try (string ":handle") >> hspaces
  x <- pDouble
  y <- pDouble
  spaces
  return $ Handle x y

pRectangle :: P Concept
pRectangle = do
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  w <- pDouble
  h <- pDouble
  return $ Rectangle x y rgba w h

pText :: P Concept
pText = do
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  sz <- pDouble
  n <- pInt
  hspaces
  c <- pContent
  return $ Text x y rgba sz n c

pLink :: P Link
pLink = do
  try (string ":link") >> hspaces
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  f <- pInt
  v <- pString
  t <- pInt
  hs <- pInts
  lw <- pDouble
  spaces
  return $ Link x y rgba f v t hs lw

pConcept :: P Concept
pConcept = choice
  [ try (string ":rectangle") >> hspaces >> pRectangle
  , try (string ":text") >> hspaces >> pText
  ]

pConcepts :: P [Concept]
pConcepts = many1 (pConcept >>= \n -> spaces >> return n)

pLinks :: P [Link]
pLinks = many pLink

pHandles :: P [Handle]
pHandles = many pHandle

pFollow :: P (Id,Id)
pFollow = do
  string ":follow" >> hspaces
  a <- pId
  b <- pId
  spaces
  return (a,b)

pState :: P S
pState = do
  cs <- pConcepts
  ls <- pLinks
  hs <- pHandles
  fs <- many pFollow
  return $ cleanState
    { concepts = cs
    , links = ls
    , handles = hs
    , follow = fs
    }

unserialize :: String -> Either ParseError S
unserialize = parse pState "unserialize"
