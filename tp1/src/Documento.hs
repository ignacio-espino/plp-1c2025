module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc cVacio cTexto cLinea d = case d of
    Vacio -> cVacio
    Texto t d' -> cTexto t (rec d')
    Linea i d' -> cLinea i (rec d')
    where
        rec = foldDoc cVacio cTexto cLinea

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 combinarTexto Linea d1
  where
    combinarTexto t Vacio = Texto t Vacio
    combinarTexto t (Texto t' d) = Texto (t ++ t') d
    combinarTexto t d = Texto t d

--indentar 2 (texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c")

indentar :: Int -> Doc -> Doc
indentar i d = foldDoc Vacio (\s d' -> Texto s d') (\indPrev d' -> Linea (indPrev + i) d') d

mostrar :: Doc -> String
mostrar d = foldDoc "" cTexto cLinea d
    where
        cTexto str oldstr = str ++ oldstr
        cLinea num oldstr = "\n" ++ replicate num " " ++ oldstr

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
