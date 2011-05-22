module Concepted.Syntax.Pretty where

import Text.PrettyPrint hiding (char, render)

import Concepted.Graphics
import Concepted.State

----------------------------------------------------------------------
-- Pretty-printing
----------------------------------------------------------------------

tshow :: Show a => a -> Doc
tshow = text . show

ppS :: S -> Doc
ppS s = (vcat . map ppConcept . concepts) s
  $+$ (vcat . map ppLink . links) s
  $+$ (vcat . map ppHandle . handles) s
  $+$ (vcat . map ppFollow . follow) s

ppConcept :: Concept -> Doc
ppConcept n = case n of
  Rectangle x y rgba w h ->
    text ":rectangle" <+> double x <+> double y <+> tshow rgba
    <+> double w <+> double h
  Text x y rgba sz w c ->
    text ":text" <+> double x <+> double y <+> tshow rgba
    <+> double sz <+> int w $+$ text c

ppHandle :: Handle -> Doc
ppHandle (Handle x y) =
  text ":handle" <+> double x <+> double y

ppLink :: Link -> Doc
ppLink (Link x y rgba f v t hs lw) =
  text ":link" <+> double x <+> double y <+> tshow rgba
  <+> int f <+> tshow v <+> int t <+> tshow hs <+> double lw

ppFollow :: (Id,Id) -> Doc
ppFollow (a,b) = text ":follow" <+> ppId a <+> ppId b

ppId :: Id -> Doc
ppId (IdConcept i) = text "(concept" <+> int i <> text ")"
ppId (IdLink i) = text "(link" <+> int i <> text ")"
ppId (IdHandle i) = text "(handle" <+> int i <> text ")"

serialize :: S -> String
serialize = renderStyle (style { lineLength = 80 }) . ppS

