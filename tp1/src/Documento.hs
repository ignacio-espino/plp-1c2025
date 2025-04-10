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

-- Precondicion: d1 y d2 son Documentos que cumplen invariante
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 =
  foldDoc
    -- caso Vacio: d2 cumple invariante y no lo modificamos
    d2
    -- caso Texto: en caso que se tengan dos constructores Texto sucesivos
    -- (posible únicamente en la unión de d1 y d2, pues cumplen invariante)
    -- hay que unir los textos para no romper invariante. No va a tener saltos
    -- de línea ni ser el string vacío pues son textos prevenientes de d1 y d2 y
    -- la operación de concatenación sobre strings no genera esas características.
    ( \str foldedDoc -> case foldedDoc of
        Texto prevText prevDoc -> Texto (str ++ prevText) prevDoc
        _ -> Texto str foldedDoc
    )
    -- caso Linea: como d1 cumple invariante, en caso que tenga un constructor Linea
    -- el numero que lo acompaña es mayor o igual a 0 y no lo modificamos
    Linea
    d1

indentar :: Int -> Doc -> Doc
indentar i d = foldDoc Vacio (\s d' -> Texto s d') (\indPrev d' -> Linea (indPrev + i) d') d

mostrar :: Doc -> String
mostrar d = foldDoc "" cTexto cLinea d
  where
    cTexto str oldstr = str ++ oldstr
    cLinea num oldstr = "\n" ++ replicate num ' ' ++ oldstr

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
