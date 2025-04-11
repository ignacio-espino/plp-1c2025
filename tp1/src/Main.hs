module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      texto "a" <+> texto "b" ~?= texto "ab",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b"),
      texto "a" <+> vacio ~?= texto "a",
      texto "a" <+> vacio <+> texto "b" ~?= texto "ab",
      (linea <+> texto "a") <+> linea ~?= linea <+> (texto "a" <+> linea),
      texto "a" <+> texto "b" <+> vacio ~?= texto "ab"
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
      indentar 3 (linea <+> indentar 2 linea) ~?= indentar 3 linea <+> indentar 5 linea
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",
      mostrar (indentar 3 linea) ~?= "\n   ",
      mostrar (linea <+> linea <+> texto "A") ~?= "\n\nA"
    ]

pericles, merlina, addams, familias, descripcionFamilia :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]
descripcionFamilia = ObjetoPP [("cantidadHijos", IntPP 2), ("familia", addams)]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      pponObjetoSimple descripcionFamilia ~?= False,
      pponObjetoSimple (IntPP 5) ~?= False
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",
      intercalar a [b, c] ~?= texto "bac",
      mostrar (intercalar vacio [vacio, vacio]) ~?= "",
      mostrar (intercalar linea [vacio, a, b, linea]) ~?= "\na\nb\n\n",
      mostrar (intercalar a [b]) ~?= "b"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",
      mostrar (aplanar vacio) ~?= "",
      mostrar (aplanar linea) ~?= " ",
      mostrar (aplanar (a <+> indentar 5 (linea <+> a))) ~?= "a a",
      mostrar (aplanar (intercalar (indentar 5 linea) [a, linea, a])) ~?= "a   a"
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      mostrar (pponADoc (IntPP 5)) ~?= "5",
      mostrar (pponADoc (TextoPP "abc")) ~?= "\"abc\"",
      mostrar (pponADoc (ObjetoPP [])) ~?= "{ }"
    ]
