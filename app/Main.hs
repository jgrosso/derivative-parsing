module Main where


import Control.Monad (forever)
import Lib (derivativeRecognize, epsilon, literal, plus, prettyPrintGrammar, recurse, special, SpecialChar(Alphabetical, Digit, Whitespace), star, union)
import System.IO (hFlush, stdout)


writeStrLn :: String -> IO ()
writeStrLn input =
  putStr input >> hFlush stdout


main :: IO ()
main =
  forever $
    do
      writeStrLn $ prettyPrintGrammar testGrammar ++ " > "
      testInput <-
        getLine
      print $ derivativeRecognize testGrammar testInput
  where
    testGrammar =
      testGrammar1

    testGrammar1 =
      do
        union [literal 'a' >> literal 'b', literal 'b']
        literal 'c'
        special Digit
        star $ do
          literal 'd'
          special Whitespace
        plus $ literal 'e'
        special Alphabetical
        epsilon

    testGrammar2 =
      do
        literal '('
        star $ recurse testGrammar2
        literal ')'
