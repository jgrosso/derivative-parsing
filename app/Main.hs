module Main where


import Control.Monad (forever)
import Lib (epsilon, literal, parse, plus, prettyPrintGrammar, RecognizeResult(..), recurse, special, SpecialChar(Alphabetical, Digit, Whitespace), star, union)
import System.IO (hFlush, stdout)


writeStrLn :: String -> IO ()
writeStrLn input =
  putStr input >> hFlush stdout


letKeyword :: String -> Maybe String
letKeyword =
  parse $ literal 'l' >> literal 'e' >> literal 't'


main :: IO ()
main =
  forever $
    do
      writeStrLn $ prettyPrintGrammar testGrammar ++ " > "
      input <-
        getLine
      putStrLn $ prettyPrintResult $ parse testGrammar input
  where
    prettyPrintResult (Just rest) =
      "More input: " ++ rest
    prettyPrintResult Nothing =
      "FAILURE"

    testGrammar =
      letKeyword

    s =
      union
        [ recurse s >> literal '+' >> recurse s
        , recurse s >> literal '-' >> recurse s
        , recurse s >> literal '*' >> recurse s
        , recurse s >> literal '/' >> recurse s

        , literal '(' >> recurse s >> literal ')'

        , literal 'x'
        , literal 'y'
        , literal 'z'
        ]
