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

-- Agregue un fold porque pense que pponObjetoSimple iba por ese lado pero no me salio y quedo la implementacion
-- de arriba. si a alguno se le ocurre como hacerlo con foldPpon joya. Lo pusheo para tenerlo porque asumo que va a ser
-- necesario despues
foldPpon :: b -> b -> ([(String, b)] -> b) -> PPON -> b
foldPpon cTexto cInt cObjeto pp = case pp of
    TextoPP _ -> cTexto
    IntPP _ -> cInt
    ObjetoPP listaPares -> cObjeto (map (\(str, ppon') -> (str, rec ppon')) listaPares)
    where
        rec = foldPpon cTexto cInt cObjeto

-- Este ejercicio lo dividi en dos: le agrego el separador a todos los elementos menos el ultim y
-- despues los concateno en un documento final

agregarATodosMenosAlUltimo :: Doc -> [Doc] -> [Doc]
agregarATodosMenosAlUltimo separador ld =
  case ld of
    []  -> []
    [x] -> [x]
    _   -> map (<+> separador) (init ld) ++ [last ld]

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
aplanar = error "PENDIENTE: Ejercicio 8"

pponADoc :: PPON -> Doc
pponADoc = error "PENDIENTE: Ejercicio 9"
