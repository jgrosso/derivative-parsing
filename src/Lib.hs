{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}


module Lib
  ( derivativeRecognizer
  , epsilon
  , literal
  , parse
  , plus
  , prettyPrintGrammar
  , Recognizer
  , RecognizeResult(..)
  , recurse
  , special
  , SpecialChar(..)
  , star
  , union
  ) where


import Control.Monad.Free
import Data.Monoid ((<>))
import Data.List (intercalate)
import TH (liftFree)


type Recognizer = Free Grammar ()


data SpecialChar
  = Alphabetical
  | Digit
  | Whitespace
  deriving (Eq)


instance Show SpecialChar where
  show :: SpecialChar -> String
  show Alphabetical = "\\w"
  show Digit = "\\d"
  show Whitespace = "\\s"


data Grammar next
  = Epsilon
  | Union [Recognizer] next
  | Literal Char next
  | Special SpecialChar next
  | Star Recognizer next
  | Plus Recognizer next
  | Recurse Recognizer next
  deriving (Eq, Functor)
$(liftFree ''Grammar)


prettyPrintGrammar :: Recognizer -> String
prettyPrintGrammar grammar =
  "(" ++ prettyPrint grammar
  where
    prettyPrint (Pure _) = ")"
    prettyPrint (Free Epsilon) = ")"
    prettyPrint (Free (Union recognizers next)) = "(" ++ intercalate "|" (map prettyPrintGrammar recognizers) ++ ")" ++ prettyPrint next
    prettyPrint (Free (Literal char next)) = char:(prettyPrint next)
    prettyPrint (Free (Special specialChar next)) = show specialChar ++ prettyPrint next
    prettyPrint (Free (Star repeated next)) = prettyPrintGrammar repeated ++ "*" ++ prettyPrint next
    prettyPrint (Free (Plus repeated next)) = prettyPrintGrammar repeated ++ "+" ++ prettyPrint next
    prettyPrint (Free (Recurse _ next)) = "(?R)" ++ prettyPrint next


data RecognizeResult
  = FullyRecognized
  | MoreInput String String
  | Unrecognized


parse :: Recognizer -> String -> Maybe (String, String)
parse recognizer input =
  case derivativeRecognizer recognizer (MoreInput "" input) of
    FullyRecognized ->
      Just (input, "")
    MoreInput result rest ->
      Just (drop (length input - length rest) input, result)
    Unrecognized ->
      Nothing


derivativeRecognizer :: Recognizer -> RecognizeResult -> RecognizeResult
derivativeRecognizer (Pure _) input =
  input
derivativeRecognizer (Free Epsilon) input =
  case input of
    FullyRecognized ->
      Unrecognized
    MoreInput _ rest ->
      case rest of
        "" ->
          FullyRecognized
        _ ->
          Unrecognized
    Unrecognized ->
      Unrecognized
derivativeRecognizer (Free (Union recognizers next)) input =
  case foldl combineAlternatives Unrecognized recognizers of
    MoreInput consumed rest ->
      derivativeRecognizer next (MoreInput rest)
    FullyRecognized ->
      FullyRecognized
    Unrecognized ->
      Unrecognized
  where
    combineAlternatives acc x =
      case acc of
        MoreInput _ ->
          acc
        FullyRecognized ->
          acc
        Unrecognized ->
          derivativeRecognizer x input
derivativeRecognizer (Free (Literal char next)) (MoreInput (current:rest)) =
  if current == char
  then
    derivativeRecognizer next (MoreInput rest)
  else
    Unrecognized
derivativeRecognizer (Free (Special specialChar next)) (MoreInput (current:rest)) =
  let
    matchList =
      case specialChar of
        Alphabetical ->
          ['a'..'z'] <> ['A'..'Z']
        Digit ->
          ['0'..'9']
        Whitespace ->
          [' ', '\t', '\n']
  in
    if current `elem` matchList
    then
      derivativeRecognizer next (MoreInput rest)
    else
      Unrecognized
derivativeRecognizer grammar@(Free (Star repeated next)) input =
  case derivativeRecognizer repeated input of
    MoreInput rest ->
      derivativeRecognizer grammar (MoreInput rest)
    FullyRecognized ->
      FullyRecognized
    Unrecognized ->
      derivativeRecognizer next input
derivativeRecognizer (Free (Plus repeated next)) input =
  case derivativeRecognizer repeated input of
    MoreInput _ ->
      derivativeRecognizer (Free (Star repeated next)) input
    FullyRecognized ->
      FullyRecognized
    Unrecognized ->
      Unrecognized
derivativeRecognizer (Free (Recurse recurseOn next)) input =
  case derivativeRecognizer recurseOn input of
    MoreInput nextInput ->
      derivativeRecognizer next (MoreInput nextInput)
    FullyRecognized ->
      FullyRecognized
    Unrecognized ->
      Unrecognized
derivativeRecognizer _ _ =
  Unrecognized
