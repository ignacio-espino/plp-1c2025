module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico p = case p of
  TextoPP _ -> True
  IntPP _ -> True
  ObjetoPP _ -> False

-- Aca capaz se puede hacer mas conciso haciendo ObjetoPP _ = False y todo lo demas True.. pero lo escribi
-- y lo deje asi porque me gusta ser extensivo (?) pueden cambiarlo si gustan.

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP listaPares) = all (pponAtomico . snd) listaPares
pponObjetoSimple _ = False

-- Este ejercicio lo dividi en dos: le agrego el separador a todos los elementos menos el ultim y
-- despues los concateno en un documento final

agregarATodosMenosAlUltimo :: Doc -> [Doc] -> [Doc]
agregarATodosMenosAlUltimo separador ld =
  case ld of
    [] -> []
    [x] -> [x]
    _ -> map (<+> separador) (init ld) ++ [last ld]

intercalar :: Doc -> [Doc] -> Doc
intercalar separador documentos = foldr (<+>) vacio (agregarATodosMenosAlUltimo separador documentos)

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
aplanar = foldDoc vacio (\str rec -> texto str <+> rec) (\num rec -> texto " " <+> rec)

foldPpon :: (String -> b) -> (Int -> b) -> ([(String, b)] -> b) -> PPON -> b
foldPpon cTexto cInt cObjeto pp = case pp of
  TextoPP text -> cTexto text
  IntPP num -> cInt num
  ObjetoPP listaPares -> cObjeto (map (\(str, ppon') -> (str, rec ppon')) listaPares)
  where
    rec = foldPpon cTexto cInt cObjeto

pponADoc :: PPON -> Doc
pponADoc ppon = fst (foldPpon (\txt -> (texto (show txt), True)) (\num -> (texto (show num), True)) cObjeto ppon)

cObjeto :: [(String, (Doc, Bool))] -> (Doc, Bool)
cObjeto recs = (if all (snd . snd) recs then cObjetoPPONSimple (sacarFlagAtomico recs) else cObjetoPPONComplejo (sacarFlagAtomico recs), False)
  where
    sacarFlagAtomico = map (\(x, (y, z)) -> (x, y))

cObjetoPPONSimple :: [(String, Doc)] -> Doc
cObjetoPPONSimple recs = texto "{" <+> accumPPONSimple recs <+> texto " }"

accumPPONSimple :: [(String, Doc)] -> Doc
accumPPONSimple xxs = case xxs of
  [] -> vacio
  [x] -> texto ("\"" ++ fst x ++ "\": ") <+> snd x
  (x : xs) -> texto (" \"" ++ label ++ "\": ") <+> doc <+> texto ", " <+> accumPPONSimple xs
    where
      label = fst x; doc = snd x

cObjetoPPONComplejo :: [(String, Doc)] -> Doc
cObjetoPPONComplejo recs = texto "{" <+> indentar 2 (linea <+> accumPPON recs) <+> linea <+> texto "}"

accumPPON :: [(String, Doc)] -> Doc
accumPPON xxs = case xxs of
  [] -> vacio
  [x] -> texto ("\"" ++ fst x ++ "\": ") <+> snd x
  (x : xs) -> texto ("\"" ++ label ++ "\": ") <+> doc <+> texto "," <+> linea <+> accumPPON xs
    where
      label = fst x; doc = snd x