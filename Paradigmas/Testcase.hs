import Test.HUnit
import IntroHaskell
import GHC.Base (maxInt)
test1 = TestCase (assertEqual "Insertar en lista vacía" [5] (insertarOrdenado 5 []))
test2 = TestCase (assertEqual "Insertar al principio" [1, 2, 3, 4] (insertarOrdenado 1 [2, 3, 4]))
test3 = TestCase (assertEqual "Insertar al final" [1, 2, 3, 5] (insertarOrdenado 5 [1, 2, 3]))
test4 = TestCase (assertEqual "Insertar en medio" [1, 2, 3, 4, 5] (insertarOrdenado 3 [1, 2, 4, 5]))
test5 = TestCase (assertEqual "Insertar elemento repetido" [1, 2, 2, 3] (insertarOrdenado 2 [1, 2, 3]))
test6 = TestCase (assertEqual "Lista con duplicados" [1, 2, 2, 2, 3] (insertarOrdenado 2 [1, 2, 2, 3]))
test7 = TestCase (assertEqual "Lista unitaria (menor)" [0, 1] (insertarOrdenado 0 [1]))
test8 = TestCase (assertEqual "Lista unitaria (mayor)" [1, 2] (insertarOrdenado 2 [1]))

tests = TestList [test1, test2, test3, test4, test5, test6, test7, test8]

main = runTestTT tests