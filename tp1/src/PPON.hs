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
pponObjetoSimple = error "PENDIENTE: Ejercicio 6"

intercalar :: Doc -> [Doc] -> Doc
intercalar = error "PENDIENTE: Ejercicio 7"

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
