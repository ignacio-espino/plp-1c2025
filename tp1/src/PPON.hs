module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico p = case p of
  ObjetoPP _ -> False
  _ -> True

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple ppon = case ppon of
  ObjetoPP listaPares -> all (pponAtomico . snd) listaPares
  _ -> False

-- Este ejercicio lo dividimos en dos: Agregamos el separador a todos los elementos menos el ultimo y
-- luego los concatenamos en un documento final
intercalar :: Doc -> [Doc] -> Doc
intercalar separador documentos = foldr (<+>) vacio (agregarATodosMenosAlUltimo separador documentos)

agregarATodosMenosAlUltimo :: Doc -> [Doc] -> [Doc]
agregarATodosMenosAlUltimo separador ld =
  case ld of
    [] -> []
    [x] -> [x]
    _ -> map (<+> separador) (init ld) ++ [last ld]

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = foldDoc vacio ((<+>) . texto) (\_ rec -> texto " " <+> rec)

recrPPON :: (String -> b) -> (Int -> b) -> ([(String, PPON)] -> [(String, b)] -> b) -> PPON -> b
recrPPON cTexto cInt cObjeto pp = case pp of
  TextoPP text -> cTexto text
  IntPP num -> cInt num
  ObjetoPP listaPares -> cObjeto listaPares (map (\(str, ppon') -> (str, rec ppon')) listaPares)
  where
    rec = recrPPON cTexto cInt cObjeto

{--
  pponADoc usa recursión primitiva, debido a que estamos accediendo a la sub-estructura del objeto(en este caso, la lista de pares),
  en adición a acceder al resultado de la recursión.
  Esto puede verse en recrPPON.
-}
pponADoc :: PPON -> Doc
pponADoc = recrPPON (texto . show) (texto . show) cObjeto

cObjeto :: ([(String, PPON)] -> [(String, Doc)] -> Doc)
cObjeto listaPares recs = if all (pponAtomico . snd) listaPares then aplanar (entreLlaves (recsADocs recs)) else entreLlaves (recsADocs recs)
  where
    recsADocs = map (\(label, doc) -> texto ("\"" ++ label ++ "\": ") <+> doc) -- Formateamos etiqueta y objeto a un Documento.