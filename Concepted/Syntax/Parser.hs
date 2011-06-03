module Concepted.Syntax.Parser where

import Data.List (intersperse)
import Data.Maybe (fromJust)
import qualified Data.IntMap as IM

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

pId :: P (Either Id Int)
pId = choice . map try $
  [ string "(concept" >> hspaces >> pInt >>= \i -> hspaces >> string ")" >> hspaces >> return (Left $ IdConcept i)
  , string "(link" >> hspaces >> pInt >>= \i -> hspaces >> string ")" >> hspaces >> return (Left $ IdLink i)
  , string "(handle" >> hspaces >> pInt >>= \i -> hspaces >> string ")" >> hspaces >> return (Right i)
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
  return $ Handle (x, y)

pRectangle :: P Concept
pRectangle = do
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  w <- pDouble
  h <- pDouble
  return $ Rectangle (x, y) rgba w h

pText :: P Concept
pText = do
  x <- pDouble
  y <- pDouble
  rgba <- pRGBA
  sz <- pDouble
  n <- pInt
  hspaces
  c <- pContent
  return $ Text (x, y) rgba sz n c

pLink :: P OldLink
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
  return $ OldLink x y rgba f v t hs lw

pConcept :: P Concept
pConcept = choice
  [ try (string ":rectangle") >> hspaces >> pRectangle
  , try (string ":text") >> hspaces >> pText
  ]

pConcepts :: P [Concept]
pConcepts = many1 (pConcept >>= \n -> spaces >> return n)

pLinks :: P [OldLink]
pLinks = many pLink

pHandles :: P [Handle]
pHandles = many pHandle

pFollow :: P (Either Id Int, Either Id Int)
pFollow = do
  string ":follow" >> hspaces
  a <- pId
  b <- pId
  spaces
  return (a,b)

pState :: P CState
pState = do
  cs <- pConcepts
  ls <- pLinks
  hs <- pHandles
  fs <- many pFollow
  return $ cleanState { planes = [ (head $ planes cleanState)
    { concepts = IM.fromList $ zip [0..] cs
    , links = IM.fromList $ zip [0..] $ map (mkLink hs) ls
    , follow = map (mkFollow ls) fs
    }] }

-- x y from rgba verb to control-points line-width
data OldLink = OldLink Double Double RGBA Int String Int [Int] Double
  deriving Show

mkLink :: [Handle] -> OldLink -> Link
mkLink hs (OldLink x y rgba from verb to ps w) =
  Link (x, y) rgba from verb to ps' w
  where ps' = map (hs !!) ps

searchHandle :: Int -> [OldLink] -> Int -> Maybe Id
searchHandle _ [] _ = Nothing
searchHandle i (OldLink _ _ _ _ _ _ ps _:ls) n =
  if i `elem` ps
    then Just $ IdLinkHandle n (length $ takeWhile (/= i) ps)
    else searchHandle i ls (n + 1)

mkFollow :: [OldLink] -> (Either Id Int, Either Id Int) -> (Id, Id)
mkFollow ls (a,b) = (mkFollow' ls a, mkFollow' ls b)

mkFollow' :: [OldLink] -> Either Id Int -> Id
mkFollow' ls b = case b of
  Right i -> fromJust $ searchHandle i ls 0
  Left a -> a

unserialize :: String -> Either ParseError CState
unserialize = parse pState "unserialize"
