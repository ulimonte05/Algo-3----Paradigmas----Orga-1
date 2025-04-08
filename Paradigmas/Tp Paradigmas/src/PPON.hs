module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico (TextoPP s) = True
pponAtomico (IntPP i) = True
pponAtomico _ = False

-- foldPpon :: (String -> b) -> (Int -> b) -> ([(String, PPON)] -> [b] -> b) -> PPON -> b
-- foldPpon fTextoPP fIntPP fObjPP ppon = 
--   case ppon of
--     TextoPP s -> fTextoPP s
--     IntPP i -> fIntPP i
--     ObjetoPP xs -> fObjPP xs (map (\(x, y) -> rec y) xs)
--     where 
--       rec = foldPpon fTextoPP fIntPP fObjPP

pericles :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]

addams :: PPON
addams = ObjetoPP [("0", pericles), ("1", pericles)]

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP ppon) = foldr (\x acc -> x || acc) False (map (\(_,v) -> pponAtomico v) ppon)

-- intercalar :: Doc -> [Doc] -> Doc
-- intercalar _ docs = foldDoc (vacio) (\s -> s) (\j -> j) (head docs)

-- entreLlaves :: [Doc] -> Doc
-- entreLlaves [] = texto "{ }"
-- entreLlaves ds =
--   texto "{"
--     <+> indentar
--       2
--       ( linea
--           <+> intercalar (texto "," <+> linea) ds
--       )
--     <+> linea
--     <+> texto "}"

aplanar :: Doc -> Doc
aplanar = error "PENDIENTE: Ejercicio 8"

pponADoc :: PPON -> Doc
pponADoc = error "PENDIENTE: Ejercicio 9"
